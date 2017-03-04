library(XML)
library(filesstrings)
library(stringr)
setwd("F:/OneDrive/Documents")


doc1 <- htmlParse("F:/OneDrive/Documents/WishListPage1.html")
doc2 <- htmlParse("F:/OneDrive/Documents/WishListPage2.html")
doc3 <- htmlParse("F:/OneDrive/Documents/WishListPage3.html")
doc4 <- htmlParse("F:/OneDrive/Documents/WishListPage4.html")
doc5 <- htmlParse("F:/OneDrive/Documents/WishListPage5.html")
doc6 <- htmlParse("F:/OneDrive/Documents/WishListPage6.html")
doc7 <- htmlParse("F:/OneDrive/Documents/WishListPage7.html")

#WishList Xpath
audibleWL <- function(doc){

     wlinks1 <- xpathSApply(doc, "//td[@class='adbl-col-4']//div[@class='adbl-prod-title']/a/@href")
     wlinks1 <- str_replace_all(wlinks1, "[\r\n]", "")
     wlinks1 <- as.vector(wlinks1)
     for (i in 1:length(wlinks1)){

          wlinks1[i] <- str_sub(wlinks1[i], 1, regexpr("ref=",wlinks1[i])[1]-1)
     }
     
     return(wlinks1)
}

wlLinks1 <- audibleWL(doc1)
wlLinks2 <- audibleWL(doc2)
wlLinks3 <- audibleWL(doc3)
wlLinks4 <- audibleWL(doc4)
wlLinks5 <- audibleWL(doc5)
wlLinks6 <- audibleWL(doc6)
wlLinks7 <- audibleWL(doc7)

wishlist <- c(wlLinks1, wlLinks2, wlLinks3, wlLinks4, wlLinks5, wlLinks6, wlLinks7)


write.csv(wishlist,"audiblewishlist.csv", row.names=FALSE)
