require(XML)
require(RCurl)
require(rlist)
require(rvest)
require(tibble)
# require(xml2)

# # General Diamond Criteria
# Shape: Princess
# Caret: 0.85 - 2.0
# Color: G - D
# Clarity: Vs2- FL
# Depth: 73 - 76
# Table: 67 - 71


# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


# Blue Nile ---------------------------------------------------------------
BlueNileTable <- read_xml("BlueNileTable.html", as_html = T)
BlueNileTable <- xmlTreeParse(BlueNileTable)[[1]]
top <- xmlRoot(BlueNileTable)

# headers
headers <- getNodeSet(top, '//div[@class="row"]')
headers <- getNodeSet(headers[[1]], '//div')
headers <- headers[-1]
headerList <- list()
for(i in 1:length(headers)){
  headerList[i] <- xmlValue(headers[[i]])
}

# each records
eachRecord <- getNodeSet(top, '//a')
blueNileDataframe <- xmlToDataFrame(nodes = eachRecord, stringsAsFactors = FALSE)
urlList <- list()
for(i in 1:length(eachRecord)){
  urlList[i] <- paste('https://www.bluenile.com', substring((xmlGetAttr(eachRecord[[i]], 'href')),2), sep = '')
}
urlList <- t(as.data.frame(urlList, stringsAsFactors = FALSE))

# Create df
names(blueNileDataframe) <- headerList
blueNileDataframe <- cbind(blueNileDataframe, urlList)
rownames(blueNileDataframe) <- NULL
blueNileDataframe <- as.tibble(blueNileDataframe)

# Fix price into a numeric
blueNileDataframe$Price <- as.numeric(gsub('[$,]', '', blueNileDataframe$Price))

# James Allen -------------------------------------------------------------
JamesAllenTable <- readHTMLTable("JamesAllenWebTable.txt", stringsAsFactors = FALSE)
JamesAllenTable <- list.clean(JamesAllenTable, fun = is.null, recursive = FALSE)
JamesAllenTable <- as.tibble(JamesAllenTable$ResultsTable[,1:11])

# Getting the ids
JamesAllenIDs <- htmlTreeParse("JamesAllenWebTable.txt")[[1]]
idList <- list()
JamesAllenIDs <- getNodeSet(JamesAllenIDs, '//tr[@data-item-id]')
for(i in 1:length(JamesAllenIDs)){
  idList[i] <- paste('https://www.jamesallen.com/ese/?q=', xmlAttrs(JamesAllenIDs[[i]])[[5]], sep = '')
}

idList <- t(as.data.frame(idList, stringsAsFactors = FALSE))
rownames(idList) <- NULL
colnames(idList) <- "James_Allen_id"
JamesAllenTable <- cbind(JamesAllenTable, idList)
JamesAllenTable <- as.tibble(JamesAllenTable)
colnames(JamesAllenTable) <- make.names(colnames(JamesAllenTable))
colnames(JamesAllenTable) <- gsub('[X.]', '', colnames(JamesAllenTable))

# Fix price as numeric
JamesAllenTable$Price <- as.numeric(gsub('[$,]', '', JamesAllenTable$Price))
