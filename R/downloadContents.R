#' Downloads html pages based on a vector of links 
#' 
#' Downloads html pages based on a vector of links.
#'  
#' @param links A character vector of links, commonly generated either with the function CreateLinks or ExtractLinks.
#' @param type Accepted values are either "articles" (default), or "index"; it defines the folder where files are stored. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param method Defaults to "auto". Method is passed to the function utils::download.file(); available options are "internal", "wininet" (Windows only) "libcurl", "wget" and "curl". For more information see ?utils::download.file()
#' @param missingArticles Logical, defaults to TRUE. If TRUE, verifies if a downloaded html file exists for each element in articlesLinks; when there is no such file, it downloads it.
#' @param linksToDownload A numeric vector. Only corresponding links will be downloaded: links[linksToDownload]
#' @param wgetSystem Logical, defaults to FALSE. Calls wget as a system command through the system() function. Wget must be previously installed on the system. 
#' @param start Integer. Only links with position higher than start in the links vector will be downloaded: links[start:length(links)]
#' @return By default, returns nothing, used for its side effects (downloads html files in relevant folder). Download files can then be imported in a vector with the function ImportHtml. 
#' @export
#' @examples
#' DownloadContents(nameOfProject, nameOfWebsite, links)

DownloadContents <- function(links, type = "articles", articlesHtml = NULL, size = 500, linksToDownload = NULL, wgetSystem = FALSE, method = "auto", missingArticles = TRUE, start = NULL, wait = 1, createScript = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfWebsite")
    }
    articlesHtmlProvided <- is.null(articlesHtml) == FALSE
    if (type=="articles") {
        htmlFilePath <- file.path(nameOfProject, nameOfWebsite, "Html")
    } else if (type=="index") {
        htmlFilePath <- file.path(nameOfProject, nameOfWebsite, "IndexHtml")
    }
    htmlFilesList <- gtools::mixedsort(list.files(htmlFilePath, full.names = TRUE))
    htmlFileSize <- file.info(htmlFilesList)["size"]
    articlesId <- 1:length(links)
    if (missingArticles == TRUE) {
        # articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
        articlesHtmlFilenamesInTheory <- file.path(htmlFilePath, paste0(articlesId, ".html"))
        linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
    }
    if (is.null(linksToDownload) == TRUE) {
        linksToDownload <- htmlFileSize < size
    }
    if (gtools::invalid(start)==FALSE){
        linksToDownload <- start:length(links)
    }
    if (is.null(articlesHtml) == TRUE) {
        articlesHtml <- rep(NA, length(links[linksToDownload]))
    }
    temp <- 1
    if (createScript == TRUE) {
        wgetSystem <- TRUE
    }
    if (wgetSystem == TRUE) {
        if (createScript == TRUE) {
            if (file.exists(file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh")) == TRUE) {
                file.remove(file.path(nameOfProject, nameOfWebsite, "downloadArticles.sh"))
            }
            options(useFancyQuotes = FALSE)
            for (i in links[linksToDownload]) {
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
            for (i in links[linksToDownload]) {
                articleId <- articlesId[linksToDownload][temp]
                if (type=="articles") {
                    system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html"))))
                    print(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
                    htmlFile <- readLines(file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")))
                } else if (type=="index") {
                    system(paste("wget", sQuote(i), "-O", file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html"))))
                    print(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
                    htmlFile <- readLines(file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html")))
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
                utils::download.file(url = i, destfile = file.path(nameOfProject, nameOfWebsite, "Html", paste0(articleId, ".html")), method = method)
            } else if (type=="index") {
                utils::download.file(url = i, destfile = file.path(nameOfProject, nameOfWebsite, "IndexHtml", paste0(articleId, ".html")), method = method)
            }
            print(paste("Downloaded article", temp, "of", length(links[linksToDownload]), ". ArticleID: ", articleId), quote = FALSE)
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
