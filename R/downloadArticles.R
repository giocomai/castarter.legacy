#' Downloads files in html format based on list of links. 
#' 
#' Downloads files in html format based on list of links.
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param start ID of articleLinks from which the download should start. Useful for interrupted downloads. Defaults to 1.
#' @return A character vector of html file, and html files saved in dedicated folder. 
#' @export
#' @examples
#' articlesHtml <- DownloadArticles(nameOfProject, nameOfWebsite, articlesLinks)

DownloadArticles <- function(nameOfProject, nameOfWebsite, articlesLinks, extractArticlesId = FALSE, start = 1, wait = 1, wget = FALSE) {
    numberOfArticles <- length(articlesLinks)
    articlesHtml <- rep(NA, numberOfArticles)
    if (extractArticlesId == FALSE) {
        articlesId <- 1:numberOfArticles
    } else {
        articlesId <- as.integer(regmatches(articlesLinks, regexpr("[[:digit:]]+", articlesLinks)))
    }
    articlesHtmlFilenames <- file.path(nameOfProject, nameOfWebsite, "Html", paste0(articlesId, ".html"))
    if (wget == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in start:numberOfArticles) {
            system(paste("wget", sQuote(articlesLinks[i]), "-O", articlesHtmlFilenames[i]))
            print(paste("Downloading article", i, "of", numberOfArticles), quote = FALSE)
            htmlFile <- readLines(articlesHtmlFilenames[i])
            htmlFile <- paste(htmlFile, collapse = "\n")
            articlesHtml[i] <- htmlFile
            Sys.sleep(wait)
        }
    } else {
        for (i in start:numberOfArticles) {
            articlesHtml[i] <- RCurl::getURL(articlesLinks[i], timeout = 20)
            print(paste("Downloading article", i, "of", numberOfArticles), quote = FALSE)
            write(articlesHtml[i], file = articlesHtmlFilenames[i])
            Sys.sleep(wait)
        }
    }
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Articles downloaded.txt", sep = " - ")))
    articlesHtml
}

#' ReDownloads missing articles. 
#'
#' Probably needs to be deleted, and integrated into DownloadArticles
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param links A character vector of links, tipically articlesLinks. 
#' @param linksToDownload A logical vector of the same length as the links vector. Links that correspond to TRUE in linksToDownload will be downloaded, and overwrite previous files by default.
#' @param missingArticles Logical, defaults to FALSE. If TRUE, verifies if a downloaded html file exists for each element in articlesLinks; when there is no such file, it downloads it.
#' @export
#' @examples
#' ReDownloadMissingArticles(nameOfProject, nameOfWebsite, articlesLinks)

ReDownloadMissingArticles <- function(nameOfProject, nameOfWebsite, links = articlesLinks, size = 500, linksToDownload = "", wget = FALSE, missingArticles = FALSE, wait = 3) {
    htmlFilesList <- mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "Html"), full.names = TRUE))
    htmlFileSize <- file.info(htmlFilesList)["size"]
    articlesId <- 1:length(articlesLinks)
    if (missingArticles == TRUE) {
        articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
        articlesHtmlFilenamesInTheory <- file.path(nameOfProject, nameOfWebsite, "Html", paste0(articlesId, ".html"))
        linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
    }
    if (linksToDownload == "") {
        linksToDownload <- htmlFileSize < size
    }
    temp <- 1
    if (wget == TRUE) {
        options(useFancyQuotes = FALSE)
        for (i in articlesLinks[linksToDownload]) {
            articleId <- articlesId[linksToDownload][temp]
            system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html"))))
            print(paste("Downloaded article", temp, "of", length(articlesLinks[linksToDownload]), ". ArticleID: ", articleId, quote = FALSE))
            htmlFile <- readLines(file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")))
            htmlFile <- paste(htmlFile, collapse = "\n")
            articlesHtml[linksToDownload][temp] <- htmlFile
            temp <- temp + 1
            Sys.sleep(wait)
        }
    } else {
        for (i in articlesLinks[linksToDownload]) {
            articleId <- articlesId[linksToDownload][temp]
            htmlFile <- RCurl::getURL(i, timeout = 20)
            write(htmlFile, file = file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")))
            print(paste("Downloaded article", temp, "of", length(articlesLinks[linksToDownload]), ". ArticleID: ", articleId, quote = FALSE))
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
    file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Missing articles downloaded.txt", sep = " - ")))
} 
