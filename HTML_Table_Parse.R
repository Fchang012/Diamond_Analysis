require(XML)
require(RCurl)
require(rlist)

# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Bluenile Table
tables <- readHTMLTable("SearchforDiamondsbyShape SizeQualityPrice   BlueNile.html")
str(tables)
