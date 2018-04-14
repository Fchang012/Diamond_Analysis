require(XML)
require(RCurl)
require(rlist)
require(rvest)
require(tibble)
# require(xml2)

# # General Diamond Criteria
# Shape: Princess
# Caret: 0.85 - 1.5
# Color: J - D
# Clarity: SI2- FL
# Depth: 73 - 76
# Table: 67 - 71
# 
# Shape: Round
# https://www.lumeradiamonds.com/diamond-education/round-diamonds
# Caret: 0.90 - 1.5
# Color: J - D
# Clarity: SI2 - FL
# Depth: 59 - 63
# Table: 53 - 58
# Price: 0 - 6500



# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


# Blue Nile ---------------------------------------------------------------
BlueNileTable <- read_xml("./RAW/BlueNileTable.html", as_html = T)
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
JamesAllenTable <- readHTMLTable("./RAW/JamesAllenWebTable.txt", stringsAsFactors = FALSE)
JamesAllenTable <- list.clean(JamesAllenTable, fun = is.null, recursive = FALSE)
JamesAllenTable <- as.tibble(JamesAllenTable$ResultsTable[,1:11])

# Getting the ids
JamesAllenIDs <- htmlTreeParse("./RAW/JamesAllenWebTable.txt")[[1]]
idList <- list()
JamesAllenIDs <- getNodeSet(JamesAllenIDs, '//tr[@data-item-id]')
for(i in 1:length(JamesAllenIDs)){
  idList[i] <- paste('https://www.jamesallen.com/ese/?q=', xmlAttrs(JamesAllenIDs[[i]])[[5]], sep = '')
}

idList <- t(as.data.frame(idList, stringsAsFactors = FALSE))
rownames(idList) <- NULL
colnames(idList) <- "urlList"
JamesAllenTable <- cbind(JamesAllenTable, idList)
JamesAllenTable <- as.tibble(JamesAllenTable)
colnames(JamesAllenTable) <- make.names(colnames(JamesAllenTable))
colnames(JamesAllenTable) <- gsub('[X.]', '', colnames(JamesAllenTable))

# Fix price as numeric
JamesAllenTable$Price <- as.numeric(gsub('[$,]', '', JamesAllenTable$Price))

# Remove IGI Lab results
JamesAllenTable <- JamesAllenTable[JamesAllenTable$Lab != "IGI",]


# Combine And Write To CSV File -------------------------------------------
reqd <- as.vector(c("Price", "Carat", "Cut", "Color", "Clarity", "urlList"))
FinalDF <- blueNileDataframe[,reqd]

# Fixing cuts for bluenile
FinalDF[FinalDF$Cut == "GoodGood",]$Cut <- "Good"
FinalDF[FinalDF$Cut == "Very GoodVery Good",]$Cut <- "Very Good"
FinalDF[FinalDF$Cut == "Astor IdealAstor",]$Cut <- "Astor Ideal"
FinalDF[FinalDF$Cut == "IdealIdeal",]$Cut <- "Ideal"

#JamesAllen add in
tempJamesAllenDF <- JamesAllenTable[, reqd]
FinalDF <- rbind(FinalDF, tempJamesAllenDF)

#Turn Carat into numerica
FinalDF$Carat <- as.numeric(FinalDF$Carat)

#Write into Clean_data using char 127 as sep
write.table(FinalDF, "./Clean_Data/Diamond_Data.csv", sep = rawToChar(as.raw(127)))
