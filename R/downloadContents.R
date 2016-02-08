#' Downloads html pages based on a vector of links 
#' 
#' Downloads html pages based on a vector of links.
#'  
#' @param links A character vector of links, commonly generated either with the function CreateLinks or ExtractLinks.
#' @param type Accepted values are either "articles" (default), or "index"; it defines the folder where files are stored. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return By default, returns nothing, used for its side effects (downloads html files in relevant folder). Download files can then be imported in a vector with the function ImportHtml. 
#' @export
#' @examples
#' html <- DownloadContents(nameOfProject, nameOfWebsite, links)

DownloadContents <- function(links, type = "articles", nameOfProject, nameOfWebsite, articlesHtml = NULL, size = 500, linksToDownload = NULL, wget = FALSE, missingArticles = FALSE, wait = 3, createScript = FALSE) {
    articlesHtmlProvided <- is.null(articlesHtml) == FALSE
    if (type=="articles") {
        htmlFilePath <- file.path(nameOfProject, nameOfWebsite, "Html")
    } else if (type=="index") {
        htmlFilePath <- file.path(nameOfProject, nameOfWebsite, "IndexHtml")
    }
    htmlFilesList <- gtools::mixedsort(list.files(htmlFilePath, full.names = TRUE))
    htmlFileSize <- file.info(htmlFilesList)["size"]
    articlesId <- 1:length(articlesLinks)
    if (missingArticles == TRUE) {
        # articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
        articlesHtmlFilenamesInTheory <- file.path(htmlFilePath, paste0(articlesId, ".html"))
        linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
    }
    if (is.null(linksToDownload) == TRUE) {
        linksToDownload <- htmlFileSize < size
    }
    if (is.null(articlesHtml) == TRUE) {
        articlesHtml <- rep(NA, length(articlesLinks[linksToDownload]))
    }
    temp <- 1
    if (wget == TRUE) {
        if (createScript == TRUE) {
            if (file.exists(file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh")) == TRUE) {
                file.remove(file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh"))
            }
            options(useFancyQuotes = FALSE)
            for (i in articlesLinks[linksToDownload]) {
                articleId <- articlesId[linksToDownload][temp]
                if (type=="articles") {
                    write(x = paste("wget", sQuote(i), "-O", file.path("Html", paste0(articleId, ".html"))), file = file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh"), append = TRUE)
                } else if (type=="index") {
                    write(x = paste("wget", sQuote(i), "-O", file.path("IndexHtml", paste0(articleId, ".html"))), file = file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh"), append = TRUE)
                }
                write(x = paste("sleep", wait), file = file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh"), append = TRUE)
                temp <- temp + 1
            }
            system(paste("chmod +x", file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh")))
        } else {
            options(useFancyQuotes = FALSE)
            for (i in articlesLinks[linksToDownload]) {
                articleId <- articlesId[linksToDownload][temp]
                if (type=="articles") {
                    system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html"))))
                } else if (type=="index") {
                    system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html"))))
                }
                print(paste("Downloaded article", temp, "of", length(articlesLinks[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
                htmlFile <- readLines(file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")))
                htmlFile <- paste(htmlFile, collapse = "\n")
                articlesHtml[linksToDownload][temp] <- htmlFile
                temp <- temp + 1
                Sys.sleep(wait)
            }
        }
    } else {
        for (i in articlesLinks[linksToDownload]) {
            articleId <- articlesId[linksToDownload][temp]
            htmlFile <- RCurl::getURL(i, timeout = 20)
            if (type=="articles") {
                write(htmlFile, file = file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")))
            } else if (type=="index") {
                write(htmlFile, file = file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html")))
            }
            print(paste("Downloaded article", temp, "of", length(articlesLinks[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
    if (type=="articles") {
        file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Missing articles downloaded.txt", sep = " - ")))
    } else if (type=="index") {
        file.create(file.path(nameOfProject, nameOfWebsite, "Logs", paste(Sys.Date(), "Missing index files downloaded.txt", sep = " - ")))
    }
    if (articlesHtmlProvided == TRUE) {
        articlesHtml
    }
} 
