require(XML)
require(RCurl)
require(rlist)
require(rvest)
require(tibble)

# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Bluenile Table
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
blueNileDataframe <- xmlToDataFrame(nodes = eachRecord)
urlList <- list()
for(i in 1:length(eachRecord)){
  urlList[i] <- xmlGetAttr(eachRecord[[i]], 'href')
}
urlList <- t(as.data.frame(urlList))

# Create df
names(blueNileDataframe) <- headerList
blueNileDataframe <- cbind(blueNileDataframe, urlList)
rownames(blueNileDataframe) <- NULL
blueNileDataframe <- as.tibble(blueNileDataframe)

