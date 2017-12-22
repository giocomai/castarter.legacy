#' Downloads html pages based on a vector of links
#'
#' Downloads html pages based on a vector of links.
#'
#' @param links A character vector of links, commonly generated either with the function CreateLinks or ExtractLinks.
#' @param type Accepted values are either "articles" (default), or "index"; it defines the folder where files are stored.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param method Defaults to "auto". Method is passed to the function utils::download.file(); available options are "internal", "wininet" (Windows only) "libcurl", "wget" and "curl". For more information see ?utils::download.file()
#' @param missingArticles Logical, defaults to TRUE. If TRUE, verifies if a downloaded html file exists for each element in articlesLinks; when there is no such file, it downloads it.
#' @param linksToDownload A logical vector. Only links corresponding to TRUE will be downloaded: links[linksToDownload]
#' @param wgetSystem Logical, defaults to FALSE. Calls wget as a system command through the system() function. Wget must be previously installed on the system.
#' @param start Integer. Only links with position higher than start in the links vector will be downloaded: links[start:length(links)]
#' @param ignoreSSLcertificates Logical, defaults to FALSE. If TRUE it uses wget to download the page, and does not check if the SSL certificate is valid. Useful, for example, for https pages with expired or mis-configured SSL certificate.
#' @return By default, returns nothing, used for its side effects (downloads html files in relevant folder). Download files can then be imported in a vector with the function ImportHtml.
#' @export
#' @examples
#' DownloadContents(links)

DownloadContents <- function(links, type = "articles", articlesHtml = NULL, size = 500, linksToDownload = NULL, wgetSystem = FALSE, method = "auto", missingArticles = TRUE, start = 1, wait = 1, ignoreSSLcertificates = FALSE, createScript = FALSE, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    articlesHtmlProvided <- is.null(articlesHtml) == FALSE
    if (type=="articles") {
        htmlFilePath <- file.path(project, website, "Html")
    } else if (type=="index") {
        htmlFilePath <- file.path(project, website, "IndexHtml")
    }
    htmlFilesList <- list.files(htmlFilePath, full.names = TRUE)
    # put them in order [equivalent to gtools::mixedorder()]
    htmlFilesList <- htmlFilesList[stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    htmlFileSize <- file.info(htmlFilesList)["size"]
    articlesId <- 1:length(links)
    if (missingArticles == TRUE) {
        articlesHtmlFilenamesInTheory <- file.path(htmlFilePath, paste0(articlesId, ".html"))
        linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
    }
    if (is.null(linksToDownload) == TRUE) {
        smallFiles <- htmlFilesList[htmlFileSize < size]
        smallFilesId <- as.integer(base::regmatches(x = smallFiles, m = regexpr("[[:digit:]]+", smallFiles)))
        linksToDownload <- rep(x = FALSE, times = length(links))
        linksToDownload[smallFilesId] <- TRUE
    }
    linksToDownload[1:start-1] <- FALSE
    if (is.null(articlesHtml) == TRUE) {
        articlesHtml <- rep(NA, length(links[linksToDownload]))
    }
    temp <- 1
    if (createScript == TRUE) {
        wgetSystem <- TRUE
    }
    if (wgetSystem == TRUE) {
        if (createScript == TRUE) {
            if (file.exists(file.path(project, website, "downloadArticles.sh")) == TRUE) {
                file.remove(file.path(project, website, "downloadArticles.sh"))
            }
            options(useFancyQuotes = FALSE)
            if (type=="articles") {
                write(x = paste("wget", sQuote(links[linksToDownload]), "-O", file.path("Html", paste0(articlesId[linksToDownload], ".html")), "-t=1 -T=20", ";", "sleep", wait), file = file.path(project, website, "downloadArticles.sh"), append = TRUE)
            } else if (type=="index") {
                write(x = paste("wget", sQuote(links[linksToDownload]), "-O", file.path("IndexHtml", paste0(articlesId[linksToDownload], ".html")), "-t=1 -T=20", ";", "sleep", wait), file = file.path(project, website, "downloadArticles.sh"), append = TRUE)
            }
            system(paste("chmod +x", file.path(project, website, "downloadArticles.sh")))
        } else {
            options(useFancyQuotes = FALSE)
            for (i in links[linksToDownload]) {
                articleId <- articlesId[linksToDownload][temp]
                if (type=="articles") {
                    system(paste("wget", sQuote(i), "-O", file.path(project, website, "Html", paste0(articleId, ".html"))))
                    message(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
                    htmlFile <- readLines(file.path(project, website, "Html", paste0(articleId, ".html")))
                } else if (type=="index") {
                    system(paste("wget", sQuote(i), "-O", file.path(project, website, "IndexHtml", paste0(articleId, ".html"))))
                    message(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
                    htmlFile <- readLines(file.path(project, website, "IndexHtml", paste0(articleId, ".html")))
                }
                htmlFile <- paste(htmlFile, collapse = "\n")
                articlesHtml[linksToDownload][temp] <- htmlFile
                temp <- temp + 1
                Sys.sleep(wait)
            }
        }
    } else {
        for (i in links[linksToDownload]) {
            articleId <- articlesId[linksToDownload][temp]
            if (type=="articles") {
                if (ignoreSSLcertificates==TRUE) {
                    try(utils::download.file(url = i, destfile = file.path(project, website, "Html", paste0(articleId, ".html")), method = "wget", extra = "--no-check-certificate"))
                } else {
                    try(utils::download.file(url = i, destfile = file.path(project, website, "Html", paste0(articleId, ".html")), method = method))
                }
            } else if (type=="index") {
                if (ignoreSSLcertificates==TRUE) {
                    try(utils::download.file(url = i, destfile = file.path(project, website, "IndexHtml", paste0(articleId, ".html")), method = "wget", extra = "--no-check-certificate"))
                } else {
                    try(utils::download.file(url = i, destfile = file.path(project, website, "IndexHtml", paste0(articleId, ".html")), method = method))
                }
            }
            message(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId))
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
    if (type=="articles") {
        file.create(file.path(project, website, "Logs", paste(Sys.Date(), "Articles downloaded.txt", sep = " - ")))
    } else if (type=="index") {
        file.create(file.path(project, website, "Logs", paste(Sys.Date(), "Index files downloaded.txt", sep = " - ")))
    }
    if (articlesHtmlProvided == TRUE) {
        articlesHtml
    }
}


#' Downloads html pages that are loaded through a web form.
#'
#' Downloads html pages that are loaded through a web form.
#'
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A vector of Html files. As a side effect, html files are stored in the IndexHtml folder.
#' @export
#' @examples
#' DownloadContentsForm(links)

DownloadContentsForm <- function(linkFirstChunk, startDate, endDate, dateSeparator = "-", dateBy = "months", start = 1, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    listOfDates <- seq(as.Date(startDate, "%Y-%m-%d"), as.Date(endDate, "%Y-%m-%d"), by = dateBy)
    listOfDates <- c(listOfDates, as.Date(endDate, "%Y-%m-%d"))
    numberOfIndexPages <- length(listOfDates) - 1
    listOfnumberOfIndexPages <- 1:numberOfIndexPages
    indexHtmlFilenames <- file.path(project, website, "IndexHtml", paste0(listOfnumberOfIndexPages, ".html"))
    indexPagesHtml <- vector()
    for (i in start:numberOfIndexPages) {
        indexPagesHtml[i] <- RCurl::postForm(linkFirstChunk, datefrom = listOfDates[i], dateto = listOfDates[i + 1])
        message(paste("Downloading index page", i, "of", numberOfIndexPages))
        write(indexPagesHtml[i], file = indexHtmlFilenames[i])
    }
    dateDownloadedIndex <- date()
    file.create(file.path(project, website, "Logs", paste(Sys.Date(), "Index downloaded.txt", sep = " - ")))
    indexPagesHtml
}
