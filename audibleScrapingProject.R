require(XML)
require(stringr)
require(filesstrings)
require(RCurl)
audibleLinks <- read.csv("F:/OneDrive/Documents/audiblewishlist.csv", header=TRUE, stringsAsFactors = FALSE)



libSize <- length(audibleLinks[,1])
audibleLinks <- audibleLinks[libSize:1,]
audibleLin <- sub("s","",audibleLinks)


#testURL <- "http://www.audible.com/pd/Fiction/The-Gunslinger-Audiobook/B019NNU7XE"
#testdoc <- htmlParse(testURL)



collapseInfo <- function(x){
     if (length(x) >= 3) {
          y <- paste(x[3:length(x)], collapse = ",")
          #return(y)
     } else {
          y <- NA
          
     }
    return(y) 
}

ratingsAvailable <- function(doc){
     ret <- FALSE
     ra <- xpathSApply(doc,"//li[@class='adbl-ratings-row' and @itemprop='aggregateRating']//span[@class='rating-average' and @itemprop='ratingValue']/text()")
     ra <- sapply(ra, xmlValue)
     if (length(ra)>0){
          ret <- TRUE
     } else {
          ret <- FALSE
     }
     return(ret)
}

productNotAvailable <- function(doc){
     ret <- TRUE
     pd <- xpathSApply(doc, "//div[@class='adbl-errormessage-pd']//h1/text()")
     pd <- sapply(pd, xmlValue)

     if (length(pd)>0){
          ret <- FALSE
     } else {
          ret <- TRUE
     }
     return(ret)
}


pictureExtract <- function(doc){
     
     picture <- ""
     logical <- productNotAvailable(doc)
     if (logical==TRUE){
     picture <- xpathSApply(doc, "//img[@id='productImage' and @class='adbl-prod-image']/@src")
     picture <- str_replace_all(picture, "[\r\n]", "")
     
     }
     return(picture)
}

titleInfoFN <- function(doc){
     
     logical <- productNotAvailable(doc)
     
     title = "NA"
     
     auths <- "NA"
     auths1 <- "NA"
     auths2 <- "NA"
     auths3 <- "NA"
     
     authslink <- "NA"
     authsLink1 <- "NA"
     authsLink2 <- "NA"
     authsLink3 <- "NA"
     
     narrator <- "NA"
     narrator1 <- "NA"
     narrator2 <- "NA"
     narrator3 <- "NA"
     
     narratorLinks <- "NA"
     narratorLink1 <- "NA"
     narratorLink2 <- "NA"
     narratorLink3 <- "NA"
     
     seriesFN <- "NA"
     series1FN <- "NA"
     series2FN <- "NA"
     series3FN <- "NA"
     
     seriesLinks <- "NA"
     seriesLinks1 <- "NA"
     seriesLinks2 <- "NA"
     seriesLinks3 <- "NA"
     
     seriesN <- "NA"
     seriesN1 <- "NA"
     seriesN2 <- "NA"
     seriesN3 <- "NA"
     
     runTime <- "NA"
     
     genre <- "NA"
     
     genreLinks <- "NA"
     
     audioFormat <- "NA"
     
     audibleRelease <- "NA"
     
     audioPublisher <- "NA"
     
     apLinks <- "NA"
     
     if (logical==TRUE){
     #Title
     title <- xpathSApply(doc, "//h1[@class='adbl-prod-h1-title']/text()")
     title <- sapply(title, xmlValue)
     title <- title[1]
     
     #Authors
     auths <- xpathSApply(doc, "//div[@class='adbl-prod-data-column']/ul//li[@class='adbl-author-row']//span[@class='adbl-prod-author']//a[@class='adbl-link author-profile-link']/span/text()")
     auths <- sapply(auths, xmlValue)
     auths1 <- auths[1]
     auths2 <- auths[2]
     auths3 <- collapseInfo(auths)
     
     #Author Links
     authslink <- xpathSApply(doc, "//div[@class='adbl-prod-data-column']/ul//li[@class='adbl-author-row']//span[@class='adbl-prod-author']//a[@class='adbl-link author-profile-link']/@href")
     authslink <- str_replace_all(authslink, "[\r\n]", "")
     authsLink1 <- authslink[1]
     authsLink2 <- authslink[2]
     authsLink3 <- collapseInfo(authslink)
     
     #Narrator
     narrator <- xpathSApply(doc, "//div[@class='adbl-prod-data-column']/ul//li[@class='adbl-narrator-row']//span[@class='adbl-prod-author']//a[@class='adbl-link']/span/text()")
     narrator <- sapply(narrator, xmlValue)
     narrator1 <- narrator[1]
     narrator2 <- narrator[2]
     narrator3 <- collapseInfo(narrator)
     
     #Narrator Links
     narratorLinks <- xpathSApply(doc, "//div[@class='adbl-prod-data-column']/ul//li[@class='adbl-narrator-row']//span[@class='adbl-prod-author']//a[@class='adbl-link']/@href")
     narratorLinks <- str_replace_all(narratorLinks, "[\r\n]", "")
     narratorLink1 <- narratorLinks[1]
     narratorLink2 <- narratorLinks[2]
     narratorLink3 <- collapseInfo(narratorLinks)
     
     #Series 
     seriesFN <- xpathSApply(doc, "//div[@class='adbl-series-link']/ul/li/span[@class='adbl-prod-author']/a[@class='adbl-link']/text()")
     seriesFN <- sapply(seriesFN, xmlValue)
     seriesFN<- str_replace_all(seriesFN, "[\r\n]", "")
     series1FN <- seriesFN[1]
     series2FN <- seriesFN[2]
     series3FN <- collapseInfo(seriesFN)
     
     
     #Series Links
     seriesLinks <- xpathSApply(doc, "//div[@class='adbl-series-link']/ul/li/span[@class='adbl-prod-author']/a[@class='adbl-link']/@href")
     seriesLinks <- str_replace_all(seriesLinks, "[\r\n]", "")
     seriesLinks1 <- seriesLinks[1]
     seriesLinks2 <- seriesLinks[2]
     seriesLinks3 <- collapseInfo(seriesLinks)
     
     #Series Number
     seriesN <- xpathSApply(doc, "//div[@class='adbl-series-link']/ul/li/span[@class='adbl-prod-author']/span[@class='adbl-label']/text()")
     seriesN <- sapply(seriesN, xmlValue)
     seriesN <- str_replace_all(seriesN, ", ", "")
     seriesN1 <- seriesN[1]
     seriesN2 <- seriesN[2]
     seriesN3 <- collapseInfo(seriesN)
     
     #RunTime
     runTime <- xpathSApply(doc, "//span[@class='adbl-run-time']/text()")
     runTime <- sapply(runTime, xmlValue)
     
     #Genre
     genre <- xpathSApply(doc, "//div[@class='adbl-prod-detail-cont']//div[@class='adbl-pd-breadcrumb']//div[@itemprop='itemListElement']/a[@class='adbl-link']/span[@itemprop='name']/text()")
     genre <- sapply(genre,xmlValue)
     genre <- genre[length(genre)] #Assigns the Last genre in path - the lowest subgenre
     
     #Genre Links
     genreLinks <- xpathSApply(doc, "//div[@class='adbl-prod-detail-cont']//div[@class='adbl-pd-breadcrumb']//div[@itemprop='itemListElement']/a[@class='adbl-link']/@href")
     genreLinks <- str_replace_all(genreLinks,"[\r\n]","")
     genreLinks <- genreLinks[length(genreLinks)] #Assigns the Last link to genre Link  - match the Genre portion of the code
     
     #Audiobook Format
     audioFormat <- xpathSApply(doc, "//span[@class='adbl-format-type']/text() | //span[@class='adbl-prod-type']/text()")
     audioFormat <- sapply(audioFormat, xmlValue)
     audioFormat <- str_replace_all(audioFormat, "[\r\n]", "")
     audioFormat <- paste(audioFormat, collapse=" ")
     
     #Audible Release
     audibleRelease <- xpathSApply(doc, "//span[@class='adbl-date adbl-release-date']/text()")
     audibleRelease <- sapply(audibleRelease, xmlValue)
     audibleRelease <- trimws(audibleRelease, which="both")
     
     #Audio Publisher
     audioPublisher <- xpathSApply(doc, "//span[@class='adbl-publisher']/a/text()")
     audioPublisher <- sapply(audioPublisher, xmlValue)
     
     #Publisher Links
     apLinks <- xpathSApply(doc, "//span[@class='adbl-publisher']/a/@href")
     apLinks <- str_replace_all(apLinks, "[\r\n]","")
     }
     
     titleInfoList <- list(t = title, author = c(auths1,auths2,auths3), authorLinks = c(authsLink1, authsLink2, authsLink3), narr = c(narrator1, narrator2, narrator3), narrLinks = c(narratorLink1, narratorLink2, narratorLink3), ser = c(series1FN,series2FN,series3FN), serLinks = c(seriesLinks1,seriesLinks2,seriesLinks3), serN = c(seriesN1, seriesN2, seriesN3), rt = runTime, g = genre, gl = genreLinks, af = audioFormat, ar = audibleRelease, ap = audioPublisher, apl = apLinks)
     
     return(titleInfoList)
}


ratingsFN <- function(doc){
     
     aRate <- "NA"
     numberAgg <- "NA"
     ratings <- "NA"
     ratingNum <- "NA"
     overall <- "NA"
     performace <- "NA"
     story <- "NA"
     overallNum <- "NA"
     performaceNum <- "NA"
     storyNum <- "NA"
     
     logical <- productNotAvailable(doc)
     logical1 <- ratingsAvailable(doc)
     
     if (logical==TRUE){
          if (logical1==TRUE){
     aRate <- xpathSApply(doc, "//li[@class='adbl-ratings-row' and @itemprop='aggregateRating']//span[@class='rating-average' and @itemprop='ratingValue']/text()")
     aRate <- sapply(aRate, xmlValue)
     numberAgg <- xpathSApply(doc, "//a[@class='review-count']/span[@itemprop='ratingCount']/text()")
     numberAgg <- sapply(numberAgg, xmlValue)
     ratings <- xpathSApply(doc, "//div[@class='omega grid_4']//div[@class='graph clearfix']//div[@class='rating-stars-graph adbl-new-stars']//span[@class='adbl-product-rating-star-text-wrap boldrating']/text()")
     ratings <- sapply(ratings, xmlValue)
     ratingNum <- xpathSApply(doc, "//div[@class='omega grid_4']//div[@class='graph clearfix']//div[@class='rating-stars-graph adbl-new-stars']//span[@class='adbl-product-rating-star-text-wrap']/text()")
     ratingNum <- sapply(ratingNum, xmlValue)
     overall <- ratings[[1]]
     performace <- ratings[[2]]
     story <- ratings[[3]]
     overallNum <- ratingNum[[1]]
     performaceNum <- ratingNum[[2]]
     storyNum <- ratingNum[[3]]
          }
     }
     
     rate <- list(AggRate = as.numeric(aRate[[1]]), numR = as.numeric(numberAgg[[1]]), or = as.numeric(overall), pr = as.numeric(performace), sr= as.numeric(story), on = unlist(ExtractNumbers(overallNum)), pn = unlist(ExtractNumbers(performaceNum)), sn = unlist(ExtractNumbers(storyNum)))
     return(rate)
}


##Aggregator

audLink <- ""
aggRate <- 0
numRatings <- 0
overallR <- 0
perR <- 0
storyR <- 0
overallN <- 0
perN <- 0
storyN <- 0

titledf <- ""
author1 <- ""
author2 <- ""
author3 <- ""
author1Link <- ""
author2Link <- ""
author3Link <- ""
narrator1 <- ""
narrator2 <- ""
narrator3 <- ""
narrator1Link <- ""
narrator2Link <- ""
narrator3Link <- ""
series1DF <- ""
series2DF <- ""
series3DF <- ""
series1Link <- ""
series2Link <- ""
series3Link <- ""
seriesNUM1 <- ""
seriesNUM2 <- ""
seriesNuM3 <- ""
runTime <- ""
genre <- ""
genreLink <- ""
audioFormat <- ""
releaseDate <- ""
publisher <- ""
publisherLink <- ""

picURL <- ""



for (i in 1:libSize) {
     
     #Parsing HTML
     doc <- htmlParse(audibleLin[i])
     
     #Getting required INFO
     ratings <- ratingsFN(doc)
     titleInfo <- titleInfoFN(doc)
     picture <- pictureExtract(doc)
     
     #Vector Population
     #Link
     audLink[i] <- audibleLin[i]
     #ratingsINFO
     aggRate[i] <- ratings$AggRate[1]
     numRatings[i] <- ratings$numR[1]
     overallR[i] <- ratings$or[1]
     perR[i] <- ratings$pr[1]
     storyR[i] <-ratings$sr[1] 
     overallN[i] <- ratings$on[1] 
     perN[i] <- ratings$pn[1] 
     storyN[i] <- ratings$sn[1] 
     
     #TileINFO
     titledf[i] <- titleInfo$t[1]
     author1[i]  <- titleInfo$author[1]
     author2[i]  <- titleInfo$author[2]
     author3[i] <- titleInfo$author[3]
     author1Link[i] <- titleInfo$authorLinks[1]
     author2Link[i] <- titleInfo$authorLinks[2]
     author3Link[i] <- titleInfo$authorLinks[3]
     narrator1[i] <- titleInfo$narr[1]
     narrator2[i] <- titleInfo$narr[2]
     narrator3[i] <- titleInfo$narr[3]
     narrator1Link[i] <- titleInfo$narrLinks[1]
     narrator2Link[i] <- titleInfo$narrLinks[2]
     narrator3Link[i] <- titleInfo$narrLinks[3]
     series1DF[i] <- titleInfo$ser[1]
     series2DF[i] <- titleInfo$ser[2]
     series3DF[i] <- titleInfo$ser[3]
     series1Link[i] <- titleInfo$serLinks[1]
     series2Link[i] <- titleInfo$serLinks[2]
     series3Link[i] <- titleInfo$serLinks[3]
     seriesNUM1[i] <- titleInfo$serN[1]
     seriesNUM2[i] <- titleInfo$serN[2]
     seriesNuM3[i] <- titleInfo$serN[3]
     runTime[i] <- titleInfo$rt[1]
     genre[i] <- titleInfo$g[1]
     genreLink[i] <- titleInfo$gl[1]
     audioFormat[i] <- titleInfo$af[1]
     releaseDate[i] <- titleInfo$ar[1]
     publisher[i] <- titleInfo$ap[1]
     publisherLink[i] <- titleInfo$apl[1]
     
     #picture URL
     picURL[i] <- picture
     
}



audibleLibrary <- data.frame(audLink, aggRate, numRatings,overallR,perR,storyR,overallN,perN,storyN,titledf,author1,author2,author3,author1Link,author2Link,author3Link,narrator1,narrator2,narrator3,narrator1Link,narrator2Link,narrator3Link,series1DF, series2DF, series3DF, series1Link, series2Link, series3Link, seriesNUM1, seriesNUM2, seriesNuM3, runTime, genre, genreLink, audioFormat, releaseDate, publisher, publisherLink, picURL)

#audibleLibrary


write.csv(audibleLibrary, "audibleWLRAW.csv",row.names=FALSE,na="")
