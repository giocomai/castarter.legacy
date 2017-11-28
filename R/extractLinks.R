#' Extracts direct links to individual articles from index pages.
#'
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links, type = "index") are stored. If not given, it defaults to the IndexHtml folder inside project/website folders.
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @param partOfLink Part of URL found only in links of individual articles to be downloaded. If more than one provided, it includes all links that contains either of the strings provided.
#' @param partOfLinkToExclude If an URL includes this string, it is excluded from the output. One or more strings may be provided.
#' @param indexLinks A character vector, defaults to NULL. If provided, indexLinks are removed from the extracted articlesLinks.
#' @param containerType Type of html container from where links are to be extracted, such as "div", "ul", and others. containerClass or containerId must also be provided.
#' @param attributeType Type of attribute to extract from links, when different from href.
#' @param minLength If a link is shorter than the number of characters given in minLength, it is excluded from the output.
#' @param maxLength If a link is longer than the number of characters given in maxLength, it is excluded from the output.
#' @param sortLinks Defaults to TRUE. If TRUE, links are sorted in aphabetical order.
#' @param linkTitle Defaults to TRUE. If TRUE, text of links is included as names of the vector.
#' @param appendString If provided, appends given string to the extracted articles. Typically used to create links for print or mobile versions of the extracted page.
#' @param removeString If provided, remove given string (or strings) from links.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown (useful for example when including scripts in rmarkdown)
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus.
#' @return A named character vector of links to articles. Name of the link may be the article title.
#' @export
#' @examples
#' articlesLinks <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/", html)
ExtractLinks <- function(htmlLocation = NULL,
                         domain = NULL,
                         partOfLink = NULL,
                         partOfLinkToExclude = NULL,
                         extractText = FALSE,
                         containerType = NULL,
                         containerClass = NULL,
                         containerId = NULL,
                         attributeType = NULL,
                         minLength = NULL,
                         maxLength = NULL,
                         indexLinks = NULL,
                         sortLinks = TRUE,
                         linkTitle = TRUE,
                         export = FALSE, appendString = NULL, removeString = NULL,
                         progressBar = TRUE,
                         project = NULL,
                         website = NULL,
                         importParameters = NULL,
                         exportParameters = TRUE) {
    # legacy params
    # If `project` and `website` not given, tries to get them from environment
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with SetCastarter(project = '...', website = '...').")
    }
    paramsFile <- base::file.path(project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))
    if (is.null(importParameters)==FALSE) {
        if (importParameters == TRUE) { # Import parameters
            if (file.exists(paramsFile) == TRUE) {
                params <- readRDS(paramsFile)
                for (i in seq_along(params$ExtractLinks)) {
                    assign(names(params$ExtractLinks)[i], params$ExtractLinks[[i]])
                }
            } else {
                # throw error if parameters file not found
                stop(paste("Parameters file not found in", paramsFile))
            }
        }
    } else {
        importParameters <- FALSE
    }
    if (exportParameters == TRUE & importParameters == FALSE) { # Export parameters
        if (file.exists(paramsFile) == TRUE) {
            params <- readRDS(paramsFile)
        } else {
            params <- list()
        }
        params$ExtractLinks <-  as.list(environment())
        saveRDS(object = params, file = paramsFile)
    }
    # define htmlLocation, if not given
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(project, website, "IndexHtml")
    }
    # list files
    indexHtml <- list.files(path = htmlLocation, full.names = TRUE)
    # put them in order [equivalent to gtools::mixedorder()]
    indexHtml <- indexHtml[stringr::str_extract(string = indexHtml, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    tempLinks <- vector("list", length(indexHtml))
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = length(indexHtml), style = 3, title = "Extracting links")
    }
    if (is.null(containerType)) {
        # if no div or such, get all links
        for (i in seq_along(indexHtml)) {
            temp <-  xml2::read_html(indexHtml[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes("a")
                tempLinks[[i]] <- temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    } else if (is.null(containerId)) {
        for (i in seq_along(indexHtml)) {
            temp <- xml2::read_html(indexHtml[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", containerType, "[@class='", containerClass, "']//a"))
                tempLinks[[i]] <-  temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    } else if (is.null(containerClass)) {
        for (i in seq_along(indexHtml)) {
            temp <- xml2::read_html(indexHtml[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", containerType, "[@id='", containerId, "']//a"))
                tempLinks[[i]] <-  temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    }
    if (progressBar == TRUE) {
        close(pb)
    }
    links <- unlist(tempLinks, recursive = FALSE)
    # introduce logical filter vector
    linkFilter <- seq_along(links)
    if (is.null(partOfLink)==FALSE) {
        linkFilter <- links %>% stringr::str_which(pattern = partOfLink)
    }
    if (is.null(partOfLinkToExclude)==FALSE) {
        for (i in seq_along(partOfLinkToExclude)) {
            linkFilter <- dplyr::setdiff(linkFilter, links %>% stringr::str_which(pattern = stringr::fixed(partOfLinkToExclude[i])))
        }
    }
    links <- links[linkFilter]
    if (is.null(domain)==FALSE&is.null(names(links))==FALSE) {
        linkTitles <- names(links)
        links <- paste0(domain, links)
        names(links) <- linkTitles
    } else if (is.null(domain)==FALSE&is.null(names(links))==TRUE) {
        links <- paste0(domain, links)
    }
    links <- gsub("//", "/", links, fixed = TRUE)
    links <- gsub("http:/", "http://", links, fixed = TRUE)
    links <- gsub("https:/", "https://", links, fixed = TRUE)
    links <- links[!duplicated(links)]
    if (is.null(minLength)==FALSE) {
        links <- links[nchar(links)>minLength]
    }
    if (is.null(maxLength)==FALSE) {
        links <- links[nchar(links)<maxLength]
    }
    if (export == TRUE) {
        writeLines(links, file.path(project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = "-")))
        message(paste("All links stored in", file.path(project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = "-"))))
    }
    links
}

ReadLinkNodes <- function(x, .pb=NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    xml2::read_html(x) %>% rvest::html_nodes("a")
}

ExtractLinksHref <- function(x, .pb=NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    x %>% rvest::html_attr('href')
}
