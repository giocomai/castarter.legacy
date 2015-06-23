#' Generates links to index pages listing individual articles.. 
#' 
#' Generates links to index pages listing individual articles.
#'  
#' @param linkFirstChunkIndex First part of index link that does not change in other index pages.
#' @param linkSecondChunkIndex Part of index link appneded after the part of the link that varies. If not relevant, may be left empty. 
#' @return A character vector of links to index pages. 
#' @export
#' @examples
#' CreateIndexPagesLinks <- CreateIndexPagesLinks("http://www.example.com/news/")
CreateIndexPagesLinks <- function(linkFirstChunkIndex, linkSecondChunkIndex = "", firstIndexPage = 1, lastIndexPage = 10, increaseBy = 1, dateStyle = "", 
    firstYear = "", lastYear = "", leadingZero = TRUE, startDate = "", endDate = "", sortIndexPagesLinks = TRUE, dateSeparator = "/", export = FALSE, 
    reversedOrder = FALSE) {
    numberOfIndexPages <- lastIndexPage - firstIndexPage + 1
    if (dateStyle == "ym" | dateStyle == "Ym") {
        years <- firstYear:lastYear
        dates <- vector()
        for (i in years) {
            if (leadingZero == TRUE) {
                newDates <- paste(rep(i, 12), sprintf("%02d", 1:12), sep = dateSeparator)
            } else {
                newDates <- paste(rep(i, 12), 1:12, sep = dateSeparator)
            }
            dates <- c(dates, newDates)
        }
        indexPagesLinks <- paste0(linkFirstChunkIndex, dates)
    } else if (dateStyle == "Y") {
        years <- firstYear:lastYear
        indexPagesLinks <- paste0(linkFirstChunkIndex, years)
    } else if (dateStyle == "ymd") {
        years <- firstYear:lastYear
        datesTemp <- vector()
        for (i in years) {
            newDatesTemp <- paste(rep(i, 12), 1:12, sep = dateSeparator)
            datesTemp <- c(datesTemp, newDatesTemp)
        }
        dates <- vector()
        for (i in 1:length(datesTemp)) {
            newDates <- paste(rep(datesTemp[i], 31), 1:31, sep = dateSeparator)
            dates <- c(dates, newDates)
        }
        indexPagesLinks <- paste0(linkFirstChunkIndex, dates)
    } else if (dateStyle == "dmY") {
        dates <- seq(as.Date(startDate), as.Date(endDate), by = "day")
        dates <- format(as.Date(dates), paste("%d", "%m", "%Y", sep = dateSeparator))
        indexPagesLinks <- paste0(linkFirstChunkIndex, dates)
    } else if (linkSecondChunkIndex == "") {
        listOfNumbers <- seq(firstIndexPage, lastIndexPage, increaseBy)
        indexPagesLinks <- cbind(rep(linkFirstChunkIndex, length(listOfNumbers)), listOfNumbers)
        indexPagesLinks <- paste0(indexPagesLinks[, 1], indexPagesLinks[, 2])
    }
    if (linkSecondChunkIndex != "") {
        listOfNumbers <- seq(firstIndexPage, lastIndexPage, increaseBy)
        indexPagesLinks <- cbind(rep(linkFirstChunkIndex, length(listOfNumbers)), listOfNumbers)
        indexPagesLinks <- paste0(indexPagesLinks[, 1], indexPagesLinks[, 2])
        indexPagesLinks <- paste0(indexPagesLinks, linkSecondChunkIndex)
    }
    if (sortIndexPagesLinks == TRUE) {
        indexPagesLinks <- mixedsort(indexPagesLinks)
    }
    if (export == TRUE) {
        writeLines(indexPagesLinks, file.path(nameOfProject, nameOfWebsite, paste0(nameOfWebsite, "indexPagesLinks.txt")))
    }
    if (reversedOrder == TRUE) {
        rev(indexPagesLinks)
    } else {
        indexPagesLinks
    }
}

#' Downloads index pages of a website. 
#' 
#' Downloads index pages of a website. 
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param indexPagesLinks A character vector of index pages, possibly generated with the function CreateIndexPagesLinks.
#' @return A character vector of html files.
#' @export
#' @examples
#' indexPagesHtml <- DownloadIndexPages(nameOfProject, nameOfWebsite, indexPagesLinks)
DownloadIndexPages <- function(nameOfProject, nameOfWebsite, indexPagesLinks, start = 1, wget = FALSE, phantomjs = FALSE, wait = 0, update = FALSE) {
    numberOfIndexPages <- length(indexPagesLinks)
    listOfnumberOfIndexPages <- 1:numberOfIndexPages
    indexPagesHtml <- rep(NA, numberOfIndexPages)
    if (update == TRUE) {
        indexHtmlFilenames <- file.path(nameOfProject, nameOfWebsite, "IndexHtml", Sys.Date(), paste0(listOfnumberOfIndexPages, ".html"))
        dir.create(file.path(nameOfProject, nameOfWebsite, "IndexHtml", Sys.Date()))
    } else {
        indexHtmlFilenames <- file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(listOfnumberOfIndexPages, ".html"))
    }
    if (wget == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in start:numberOfIndexPages) {
            system(paste("wget", sQuote(indexPagesLinks[i]), "-O", indexHtmlFilenames[i]))
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            htmlFile <- readLines(indexHtmlFilenames[i])
            htmlFile <- paste(htmlFile, collapse = "\n")
            indexPagesHtml[i] <- htmlFile
            Sys.sleep(wait)
        }
    } else if (phantomjs == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in start:numberOfIndexPages) {
            system(paste("phantomjs save.js", sQuote(indexPagesLinks[i]), indexHtmlFilenames[i]))
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            htmlFile <- readLines(indexHtmlFilenames[i])
            htmlFile <- paste(htmlFile, collapse = "\n")
            indexPagesHtml[i] <- htmlFile
            Sys.sleep(wait)
        }
    } else {
        for (i in start:numberOfIndexPages) {
            indexPagesHtml[i] <- getURL(indexPagesLinks[i], timeout = 20)
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            write(indexPagesHtml[i], file = indexHtmlFilenames[i])
            Sys.sleep(wait)
        }
    }
    dateDownloadedIndex <- date()
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Index downloaded.txt", sep = " - ")))
    indexPagesHtml
}


ReDownloadMissingIndexPages <- function(nameOfProject, nameOfWebsite, links = indexPagesLinks, size = 500, wget = FALSE, phantomjs = FALSE, wait = 0) {
    htmlFilesList <- mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "IndexHtml"), full.names = TRUE))
    htmlFileSize <- file.info(htmlFilesList)["size"]
    htmlFilesToDownload <- htmlFileSize < size
    articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
    temp <- 1
    if (wget == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in links[htmlFilesToDownload]) {
            articleId <- articlesId[htmlFilesToDownload][temp]
            system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html"))))
            print(paste("Downloading index page", i, "of", length(links[htmlFileSize < size])), quote = FALSE)
            Sys.sleep(wait)
            temp <- temp + 1
        }
    } else if (phantomjs == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in links[htmlFilesToDownload]) {
            articleId <- articlesId[htmlFilesToDownload][temp]
            system(paste("phantomjs save.js", sQuote(i), file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html"))))
            print(paste("Index page", temp, "of", length(links[htmlFileSize < size]), "downloaded."), quote = FALSE)
            Sys.sleep(wait)
            temp <- temp + 1
        }
    } else {
        for (i in links[htmlFilesToDownload]) {
            htmlFile <- getURL(i, timeout = 20)
            print(paste("Downloading index page", temp, "of", length(links[htmlFileSize < size])), quote = FALSE)
            articleId <- articlesId[htmlFilesToDownload][temp]
            write(htmlFile, file = file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html")))
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Missing index pages downloaded.txt", sep = " - ")))
}


DownloadIndexPagesForm <- function(nameOfProject, nameOfWebsite, linkFirstChunkIndex, firstYear = "", lastYear = "", dateSeparator = "/", dateBy = "months", 
    start = 1) {
    startDate <- paste(firstYear, "01", "01", sep = dateSeparator)
    endDate <- paste(lastYear, "12", "31", sep = dateSeparator)
    listOfDates <- seq(as.Date(startDate, "%Y-%m-%d"), as.Date(endDate, "%Y-%m-%d"), by = dateBy)
    listOfDates <- c(listOfDates, as.Date(endDate, "%Y-%m-%d"))
    numberOfIndexPages <- length(listOfDates) - 1
    listOfnumberOfIndexPages <- 1:numberOfIndexPages
    indexHtmlFilenames <- file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(listOfnumberOfIndexPages, ".html"))
    indexPagesHtml <- vector()
    for (i in start:numberOfIndexPages) {
        indexPagesHtml[i] <- postForm(linkFirstChunkIndex, datefrom = listOfDates[i], dateto = listOfDates[i + 1])
        print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
        write(indexPagesHtml[i], file = indexHtmlFilenames[i])
    }
    dateDownloadedIndex <- date()
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Index downloaded.txt", sep = " - ")))
    indexPagesHtml
} 
