library(XML)
library(filesstrings)
library(stringr)
setwd("F:/OneDrive/Documents")

doc1 <- htmlParse("F:/OneDrive/Documents/My Library _ Audible.compage1.html")
doc2 <- htmlParse("F:/OneDrive/Documents/My Library _ Audible.compage2.html")

links1 <- xpathSApply(doc1, "//td[@name='titleInfo' and @class='adbl-flyout-cont-marker adbl-library-item-title']/a/@href")
links1 <- str_replace_all(links1, "[\r\n]", "")
link1 <- as.vector(links1)
links2 <- xpathSApply(doc2, "//td[@name='titleInfo' and @class='adbl-flyout-cont-marker adbl-library-item-title']/a/@href")
links2 <- str_replace_all(links2, "[\r\n]", "")
link2 <- as.vector(links2)

links <- c(link1,link2)


write.csv(links, file="audiblelinks.csv", row.names=FALSE) #note some of the links will be downloads for resources (I manually removed these)

doc
