#' Generates links to index pages listing individual articles.. 
#' 
#' Generates links to index pages listing individual articles.
#'  
#' @param linkFirstChunk First part of index link that does not change in other index pages.
#' @param linkSecondChunk Part of index link appneded after the part of the link that varies. If not relevant, may be left empty. 
#' @param startPage, endPage If the links include a numerical component, define first and last number of the sequence.startPage defaults to 1, endPage defaults to 10.
#' @param increaseBy Defines by how much the number in the link should be increased in the numerical sequence. Defaults to 1.
#' @param dateFormat A charachter string that defines the format of the date to incldue in the link. Available options are: "Y" (e.g. 2015), "Ym" (2015-10), "Ymd" (e.g. 2015-10-24).
#' @param leadingZero Defaults to TRUE. If TRUE, days and months of one digit are preceded by a zero (e.g. 07/05/2014).
#' @param export Exports the links in a .txt file in the folder nameOfProject/nameOfWebsite.
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the nameOfProject/nameOfWebsite folder. They can be used to update the corpus. Requires parameters nameOfProject/nameOfWebsite.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @return A character vector of links to index pages. 
#' @export
#' @examples
#' indexLinks <- CreateLinks("http://www.example.com/news/")
CreateLinks <- function(linkFirstChunk, linkSecondChunk = NULL, startPage = 1, endPage = 10, increaseBy = 1, dateFormat = NULL, 
                        firstYear = "", lastYear = "", leadingZero = TRUE, startDate = "", endDate = "", sortIndexLinks = TRUE, dateSeparator = "/", export = FALSE, 
                        reversedOrder = FALSE, exportParameters = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (is.null(dateFormat) == FALSE) {
        if (dateFormat == "ym" | dateFormat == "Ym") {
            years <- firstYear:lastYear
            dates <- vector()
            for (i in years) {
                if (leadingZero == TRUE) {
                    newDates <- base::paste(base::rep(i, 12), base::sprintf("%02d", 1:12), sep = dateSeparator)
                } else {
                    newDates <-base:: paste(rep(i, 12), 1:12, sep = dateSeparator)
                }
                dates <- base::c(dates, newDates)
            }
            indexLinks <- base::paste0(linkFirstChunk, dates)
        } else if (dateFormat == "Y") {
            years <- firstYear:lastYear
            indexLinks <- paste0(linkFirstChunk, years)
        } else if (dateFormat == "ymd" | dateFormat == "Ymd" ) {
            dates <- base::seq(as.Date(startDate), as.Date(endDate), by = "day")
            dates <- base::format(as.Date(dates), paste("%Y", "%m", "%d", sep = dateSeparator))
            indexLinks <- paste0(linkFirstChunk, dates)
        } else if (dateFormat == "dmY") {
            dates <- base::seq(as.Date(startDate), as.Date(endDate), by = "day")
            dates <- base::format(as.Date(dates), paste("%d", "%m", "%Y", sep = dateSeparator))
            indexLinks <- paste0(linkFirstChunk, dates)
        } 
    }
    if (base::is.null(linkSecondChunk) == TRUE) {
        if (is.null(dateFormat) == FALSE) {
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        } else {
            listOfNumbers <- base::seq(startPage, endPage, increaseBy)
            if (base::is.element(endPage, listOfNumbers) == FALSE) {
                listOfNumbers <- base::c(listOfNumbers, endPage)
            }
            indexLinks <- base::cbind(rep(linkFirstChunk, length(listOfNumbers)), listOfNumbers)
            indexLinks <- base::paste0(indexLinks[, 1], indexLinks[, 2])
        }
    }
    if (base::is.null(linkSecondChunk) == FALSE) {
        if (is.null(dateFormat) == FALSE) {
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        } else {
            listOfNumbers <- base::seq(startPage, endPage, increaseBy)
            indexLinks <- base::cbind(base::rep(linkFirstChunk, base::length(listOfNumbers)), listOfNumbers)
            indexLinks <- base::paste0(indexLinks[, 1], indexLinks[, 2])
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        }
    }
    if (sortIndexLinks == TRUE) {
        indexLinks <- gtools::mixedsort(indexLinks)
    }
    if (export == TRUE) {
        base::writeLines(indexLinks, base::file.path(nameOfProject, nameOfWebsite, paste0(nameOfWebsite, "indexLinks.txt")))
    }
    if (exportParameters == TRUE) {
        args <- c("linkFirstChunk", "linkSecondChunk", "startPage", "endPage", "increaseBy", "dateFormatCreateLinks", "firstYear", "lastYear", "leadingZero", "startDate", "endDate", "sortIndexLinks", "dateSeparator", "export", "reversedOrder", "exportParameters", "nameOfProject", "nameOfWebsite")
        param <- list(linkFirstChunk, linkSecondChunk, startPage, endPage, increaseBy, dateFormat, firstYear, lastYear, leadingZero, startDate, endDate, sortIndexLinks, dateSeparator, export, reversedOrder, exportParameters, nameOfProject, nameOfWebsite)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    if (reversedOrder == TRUE) {
        base::rev(indexLinks)
    } else {
        indexLinks
    }
}

#' Downloads index pages of a website. 
#' 
#' Downloads index pages of a website. 
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param indexLinks A character vector of index pages, possibly generated with the function CreateindexLinks.
#' @return A character vector of html files.
#' @export
#' @examples
#' indexPagesHtml <- DownloadIndex(nameOfProject, nameOfWebsite, indexLinks)
DownloadIndex <- function(nameOfProject, nameOfWebsite, indexLinks, start = 1, wget = FALSE, phantomjs = FALSE, wait = 0, update = FALSE) {
    numberOfIndexPages <- length(indexLinks)
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
            system(paste("wget", sQuote(indexLinks[i]), "-O", indexHtmlFilenames[i]))
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            htmlFile <- readLines(indexHtmlFilenames[i])
            htmlFile <- paste(htmlFile, collapse = "\n")
            indexPagesHtml[i] <- htmlFile
            Sys.sleep(wait)
        }
    } else if (phantomjs == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in start:numberOfIndexPages) {
            system(paste("phantomjs save.js", sQuote(indexLinks[i]), indexHtmlFilenames[i]))
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            htmlFile <- readLines(indexHtmlFilenames[i])
            htmlFile <- paste(htmlFile, collapse = "\n")
            indexPagesHtml[i] <- htmlFile
            Sys.sleep(wait)
        }
    } else {
        for (i in start:numberOfIndexPages) {
            indexPagesHtml[i] <- RCurl::getURL(indexLinks[i], timeout = 20)
            print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
            write(indexPagesHtml[i], file = indexHtmlFilenames[i])
            Sys.sleep(wait)
        }
    }
    dateDownloadedIndex <- date()
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Index downloaded.txt", sep = " - ")))
    indexPagesHtml
}


ReDownloadMissingIndexPages <- function(nameOfProject, nameOfWebsite, links = indexLinks, size = 500, wget = FALSE, phantomjs = FALSE, wait = 0) {
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
            htmlFile <- RCurl::getURL(i, timeout = 20)
            print(paste("Downloading index page", temp, "of", length(links[htmlFileSize < size])), quote = FALSE)
            articleId <- articlesId[htmlFilesToDownload][temp]
            write(htmlFile, file = file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html")))
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Missing index pages downloaded.txt", sep = " - ")))
}


DownloadIndexPagesForm <- function(nameOfProject, nameOfWebsite, linkFirstChunk, firstYear = "", lastYear = "", dateSeparator = "/", dateBy = "months", 
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
        indexPagesHtml[i] <- postForm(linkFirstChunk, datefrom = listOfDates[i], dateto = listOfDates[i + 1])
        print(paste("Downloading index page", i, "of", numberOfIndexPages), quote = FALSE)
        write(indexPagesHtml[i], file = indexHtmlFilenames[i])
    }
    dateDownloadedIndex <- date()
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Index downloaded.txt", sep = " - ")))
    indexPagesHtml
} 
