#' Downloads html pages based on a vector of links
#'
#' Downloads html pages based on a vector of links.
#'
#' @param links A character vector of links, commonly generated either with the function CreateLinks or ExtractLinks.
#' @param type Accepted values are either "articles" (default), "index", or "pdf"; it defines the folder where files are stored. If "pdf" is given a dedicated pdf folder is created.
#' @param path Defaults to NULL. If given, overrides the "type" param and stores html files in given path as a subfolder of project/website. Folder must already exist, and should be empty.
#' @param wait Defaults to 1. Number of seconds to wait between downloading one page and the next. Can be increased to reduce server load, or can be set to 0 when this is not an issue.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param method Defaults to "auto". Method is passed to the function utils::download.file(); available options are "internal", "wininet" (Windows only) "libcurl", "wget" and "curl". For more information see ?utils::download.file()
#' @param missingPages Logical, defaults to TRUE. If TRUE, verifies if a downloaded html file exists for each element in articlesLinks; when there is no such file, it downloads it.
#' @param size Defaults to 500. It represents the minimum size in bytes that downloaded html files should have: files that are smaller will be downloaded again. Used only when missingPages == FALSE.
#' @param linksToDownload A logical vector. Only links corresponding to TRUE will be downloaded. Corresponds to `links[linksToDownload]` but keeps the id in line with the original location in the links vector. If given, it ignores other parameters and downloads all selected pages (overwriting if they exist already).
#' @param linksToCheck A logical vector. Only links corresponding to TRUE will be considered for download. Corresponds to `links[linksToCheck]` but keeps the id in line with the original location in the links vector. If given, unlike the `linksToDownload` parameter it considers other parameters, e.g. if `missingPages=TRUE`, it downloads selected pages only if they have not been previously downloaded.
#' @param wgetSystem Logical, defaults to FALSE. Calls wget as a system command through the system() function. Wget must be previously installed on the system.
#' @param start Integer. Only links with position higher than start in the links vector will be downloaded: links[start:length(links)]
#' @param ignoreSSLcertificates Logical, defaults to FALSE. If TRUE it uses wget to download the page, and does not check if the SSL certificate is valid. Useful, for example, for https pages with expired or mis-configured SSL certificate.
#' @param use_headless_chromium Logical, defaults to FALSE. If TRUE uses the `crrri` package to download pages. Useful in particular when web pages are generated via javascript. See in particular: https://github.com/RLesur/crrri#system-requirements
#' @param createScript Logical, defaults to FALSE. Tested on Linux only. If TRUE, creates a downloadPages.sh executable file that can be used to download all relevant pages from a terminal.
#' @return By default, returns nothing, used for its side effects (downloads html files in relevant folder). Download files can then be imported in a vector with the function ImportHtml.
#' @export
#' @examples
#' \dontrun{
#' DownloadContents(links)
#' }

DownloadContents <- function(links,
                             type = "articles",
                             path = NULL,
                             size = 500,
                             linksToCheck = NULL,
                             linksToDownload = NULL,
                             wgetSystem = FALSE,
                             method = "auto",
                             missingPages = TRUE,
                             start = 1,
                             wait = 1,
                             ignoreSSLcertificates = FALSE,
                             use_headless_chromium = FALSE,
                             use_phantomjs = FALSE,
                             createScript = FALSE,
                             project = NULL,
                             website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }

    if (use_headless_chromium==TRUE) {
        if (requireNamespace("crrri", quietly = TRUE)==FALSE) {
            stop("You need to install the `crrri` package to download pages with headless chrome/chromium. For details, see: https://github.com/RLesur/crrri. Make sure to read the note on system requirements: https://github.com/RLesur/crrri#system-requirements")
        }
    }

    fileFormat <- "html"
    if (type=="articles") {
        htmlFilePath <- file.path(baseFolder, project, website, "Html")
    } else if (type=="index") {
        htmlFilePath <- file.path(baseFolder, project, website, "IndexHtml")
    } else if (type=="pdf"){
        dir.create(path = file.path(baseFolder, project, website, "Pdf"), showWarnings = FALSE)
        htmlFilePath <- file.path(baseFolder, project, website, "Pdf")
        fileFormat <- "pdf"
    }  else if (type=="html"){
        dir.create(path = file.path(baseFolder, project, website, "Html"), showWarnings = FALSE)
        htmlFilePath <- file.path(baseFolder, project, website, "Html")
    }
    if (is.null(path)==FALSE) {
        htmlFilePath <- file.path(baseFolder, project, website, path)
    }
    htmlFilesList <- list.files(htmlFilePath, full.names = TRUE)
    # put them in order [equivalent to gtools::mixedorder()]
    htmlFilesList <- htmlFilesList[stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+[[:punct:]]html") %>%
                                       stringr::str_sub(start = 1L, end = -(nchar(fileFormat)+2)) %>%
                                       as.integer() %>%
                                       order()]
    htmlFileSize <- file.info(htmlFilesList)["size"]
    articlesId <- 1:length(links)
    if (is.null(linksToDownload) == TRUE) {
        if (missingPages == TRUE) {
            articlesHtmlFilenamesInTheory <- file.path(htmlFilePath, paste0(articlesId, ".", fileFormat))
            linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
            if (is.null(linksToCheck)==FALSE) {
                linksToDownload <- Reduce(f = "&", x = list(linksToDownload, linksToCheck))
            }
        } else if (missingPages == FALSE) {

            smallFiles <- htmlFilesList[htmlFileSize < size]
            smallFilesId <- as.integer(stringr::str_extract(string = smallFiles,
                                                            pattern = paste0("[[:digit:]]+[[:punct:]]", fileFormat)) %>%
                                           stringr::str_sub(start = 1L, end = -(nchar(fileFormat)+2)))
            linksToDownload <- rep(x = FALSE, times = length(links))
            linksToDownload[smallFilesId] <- TRUE
            if (is.null(linksToCheck)==FALSE) {
                linksToDownload <- Reduce(f = "&", x = list(linksToDownload, linksToCheck))
            }
        }
    } else if (is.null(linksToDownload) == FALSE) {
        # do nothing
    }
    linksToDownload[1:start-1] <- FALSE
    temp <- 1
    if (createScript == TRUE) {
        wgetSystem <- TRUE
    }
    if (wgetSystem == TRUE) {
        if (createScript == TRUE) {
            if (file.exists(file.path(baseFolder, project, website, "downloadPages.sh")) == TRUE) {
                file.remove(file.path(baseFolder, project, website, "downloadPages.sh"))
            }
            write(x = paste0("wget '", links[linksToDownload], "' -O '",
                             file.path("..", "..", "..", htmlFilePath, paste0(articlesId[linksToDownload], ".", fileFormat)), "'",
                             " -t 1 -T 20", "; ", "sleep ", wait),
                  file = file.path(baseFolder, project, website, "downloadPages.sh"), append = TRUE)
            system(paste("chmod +x", file.path(baseFolder, project, website, "downloadPages.sh")))
        } else {
            options(useFancyQuotes = FALSE)
            for (i in links[linksToDownload]) {
                articleId <- articlesId[linksToDownload][temp]
                system(paste("wget '", i, "' -O", file.path(htmlFilePath, paste0(articleId, ".", fileFormat))))
                message(paste("Downloaded item", temp, "of", length(links[linksToDownload]), ". ID: ", articleId), quote = FALSE)
                temp <- temp + 1
                Sys.sleep(wait)
            }
        }
    } else {
        for (i in links[linksToDownload]) {
            articleId <- articlesId[linksToDownload][temp]
            if (use_headless_chromium == TRUE) {
                #based on example from: https://github.com/RLesur/crrri
                crrri::perform_with_chrome(function(client) {
                    Network <- client$Network
                    Page <- client$Page
                    Runtime <- client$Runtime
                    Network$enable() %...>% {
                        Page$enable()
                    } %...>% {
                        Network$setCacheDisabled(cacheDisabled = TRUE)
                    } %...>% {
                        Page$navigate(url = i)
                    } %...>% {
                        Page$loadEventFired()
                    } %...>% {
                        Runtime$evaluate(
                            expression = 'document.documentElement.outerHTML'
                        )
                    } %...>% (function(result) {
                        writeLines(text = result$result$value,
                                   con = file.path(htmlFilePath,
                                                   paste0(articleId, ".", fileFormat)),
                                   sep = "\n")
                    })
                })


            } else if (use_phantomjs == TRUE) {
                options(useFancyQuotes = FALSE)
                if (fs::file_exists("save.js")==FALSE) {
                    download.file(url = "https://raw.githubusercontent.com/giocomai/castarter/master/inst/save.js", destfile = "save.js")
                }
                system(paste("phantomjs save.js", sQuote(i), file.path(htmlFilePath, paste0(articleId, ".", fileFormat))))
            } else {
                if (ignoreSSLcertificates==TRUE) {
                    try(utils::download.file(url = i, destfile = file.path(htmlFilePath, paste0(articleId, ".", fileFormat)), method = "wget", extra = "--no-check-certificate"))
                } else {
                    try(utils::download.file(url = i, destfile = file.path(htmlFilePath, paste0(articleId, ".", fileFormat)), method = method))
                }
            }
            message(paste("Downloaded item", temp, "of", length(links[linksToDownload]), ". ID: ", articleId))
            temp <- temp + 1
            Sys.sleep(wait)
        }
    }
}
