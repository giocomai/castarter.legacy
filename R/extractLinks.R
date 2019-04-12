#' Extracts direct links to individual articles from index pages.
#'
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links, type = "index") are stored. If not given, it defaults to the IndexHtml folder inside project/website folders.
#' @param id Defaults to NULL. If provided, it should be a vector of integers. Only html files corresponding to given id in the relevant htmlLocation will be processed.
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @param partOfLink Part of URL found only in links of individual articles to be downloaded. If more than one provided, it includes all links that contains either of the strings provided.
#' @param partOfLinkToExclude If an URL includes this string, it is excluded from the output. One or more strings may be provided.
#' @param indexLinks A character vector, defaults to NULL. If provided, indexLinks are removed from the extracted articlesLinks.
#' @param container Type of html container from where links are to be extracted, such as "div", "ul", and others. containerClass or containerId must also be provided.
#' @param attributeType Type of attribute to extract from links, when different from href.
#' @param minLength If a link is shorter than the number of characters given in minLength, it is excluded from the output.
#' @param maxLength If a link is longer than the number of characters given in maxLength, it is excluded from the output.
#' @param sortLinks Defaults to FALSE If TRUE, links are sorted in alphabetical order.
#' @param linkTitle Defaults to TRUE. If TRUE, text of links is included as names of the vector.
#' @param appendString If provided, appends given string to the extracted articles. Typically used to create links for print or mobile versions of the extracted page.
#' @param removeString If provided, remove given string (or strings) from links.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown (useful for example when including scripts in rmarkdown)
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus.
#' @return A named character vector of links to articles. Name of the link may be the article title.
#' @export
#' @examples
#' \dontrun{
#' links <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/")
#' }
ExtractLinks <- function(domain = NULL,
                         partOfLink = NULL,
                         partOfLinkToExclude = NULL,
                         container = NULL,
                         containerClass = NULL,
                         containerId = NULL,
                         attributeType = NULL,
                         htmlLocation = NULL,
                         id = NULL,
                         minLength = NULL,
                         maxLength = NULL,
                         indexLinks = NULL,
                         sortLinks = FALSE,
                         linkTitle = TRUE,
                         appendString = NULL,
                         export = FALSE,
                         removeString = NULL,
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
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with SetCastarter(project = '...', website = '...').")
    }
    if (is.null(importParameters)==FALSE) {
        if (importParameters == TRUE) { # Import parameters
            if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
                params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
                params$ExtractLinks$exportParameters <- FALSE
                if (is.null(id)==FALSE) {
                    params$ExtractLinks$id <- id
                }
                for (i in seq_along(params$ExtractLinks)) {
                    assign(names(params$ExtractLinks)[i], params$ExtractLinks[[i]])
                }
            } else {
                # throw error if parameters file not found
                stop(paste("Parameters file not found in", base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))))
            }
        }
    } else {
        importParameters <- FALSE
    }
    if (exportParameters == TRUE & importParameters == FALSE) { # Export parameters
        extractLinksParams <-  as.list(environment())
        if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
            params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
            params$ExtractLinks <- NULL
        } else {
            params <- list()
        }
        params$ExtractLinks <- extractLinksParams
        saveRDS(object = params, file = base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
    }
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(baseFolder, project, website, "IndexHtml")
    }
    # If IDs not given, list files
    if (is.null(id)==FALSE) {
        HtmlFiles <- file.path(htmlLocation, paste0(id, ".html"))
    } else {
        # list files
        HtmlFiles <- list.files(path = htmlLocation, full.names = TRUE)
        # put them in order [equivalent to gtools::mixedorder()]
        HtmlFiles <- HtmlFiles[stringr::str_extract(string = HtmlFiles, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    }
    tempLinks <- vector("list", length(HtmlFiles))
    if (progressBar == TRUE) {
        pb <- dplyr::progress_estimated(n = length(HtmlFiles), min_time = 1)
    }
    if (is.null(container)) {
        # if no div or such, get all links
        for (i in seq_along(HtmlFiles)) {
            if (progressBar == TRUE) {
                pb$tick()$print()
            }
            temp <-  xml2::read_html(x = HtmlFiles[i],
                                     options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"))
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes("a")
                tempLinks[[i]] <- temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
            }
        }
    } else if (is.null(containerId)==TRUE&is.null(containerClass)==FALSE) {
        for (i in seq_along(HtmlFiles)) {
            if (progressBar == TRUE) {
                pb$tick()$print()
            }
            temp <- xml2::read_html(HtmlFiles[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']//a"))
                tempLinks[[i]] <-  temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
            }
        }
    } else if (is.null(containerClass)==TRUE&is.null(containerId)==FALSE) {
        for (i in seq_along(HtmlFiles)) {
            if (progressBar == TRUE) {
                pb$tick()$print()
            }
            temp <- xml2::read_html(HtmlFiles[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", container, "[@id='", containerId, "']//a"))
                tempLinks[[i]] <-  temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
            }
        }
    } else if (is.null(containerClass)&is.null(containerId)) {
        for (i in seq_along(HtmlFiles)) {
            if (progressBar == TRUE) {
                pb$tick()$print()
            }
            temp <- xml2::read_html(HtmlFiles[i])
            if (is.element("xml_node", set = class(temp))==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", container, "//a"))
                tempLinks[[i]] <-  temp %>%
                    xml2::xml_attr("href")
                if (linkTitle == TRUE) {
                    names(tempLinks[[i]]) <- temp %>% rvest::html_text()
                }
            }
        }
    }
    links <- unlist(tempLinks, recursive = FALSE)
    # introduce logical filter vector
    linkFilter <- seq_along(links)
    if (is.null(partOfLink)==FALSE) {
        linkFilter <- links %>% stringr::str_which(pattern = paste(partOfLink, collapse = "|"))
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
    if (is.null(appendString)==FALSE) {
        links <- paste0(links, appendString)
    }
    if (is.null(removeString)==FALSE) {
        for (i in seq_along(removeString)) {
            links <- stringr::str_remove_all(string = links, pattern = stringr::fixed(removeString[i]))
        }
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
    if (sortLinks == TRUE) {
        links <- sort(links)
    }
    if (export == TRUE) {
        writeLines(links, file.path(baseFolder, project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = "-")))
        message(paste("All links stored in", file.path(baseFolder, project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = "-"))))
    }
    links
}
