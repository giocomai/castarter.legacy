#' Extracts direct links to individual articles from index pages.
#'
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'
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
                         attributeType = NULL, minLength = NULL, maxLength = NULL, indexLinks = NULL, sortLinks = TRUE, linkTitle = TRUE, export = FALSE, appendString = NULL, removeString = NULL,
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
    # list files
    indexHtml <- list.files(path = file.path(project, website, "IndexHtml"), full.names = TRUE)
    # put them in order [equivalent to gtools::mixedorder()]
    indexHtml <- indexHtml[stringr::str_extract(string = indexHtml, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    tempLinks <- vector("list", length(indexHtml))
    if (is.null(containerType)) {
        # if no div or such, get all links
        for (i in seq_along(indexHtml)) {
            tempLinks[[i]] <- xml2::read_html(indexHtml[i]) %>%
                rvest::html_nodes("a") %>%
                xml2::xml_attr("href")
        }
    } else {
        for (i in seq_along(indexHtml)) {
            tempLinks[[i]] <- xml2::read_html(indexHtml[i]) %>%
                rvest::html_nodes(xpath = paste0("//", containerType, "[@class='", containerClass, "']//a")) %>%
                xml2::xml_attr("href")
        }
    }
    links <- unlist(tempLinks, recursive = TRUE)
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
    links <- paste0(domain, links)
    links <- gsub("//", "/", links, fixed = TRUE)
    links <- gsub("http:/", "http://", links, fixed = TRUE)
    links <- gsub("https:/", "https://", links, fixed = TRUE)
    links <- unique(links)
    # if (extractText==TRUE) {
    #     names(links) <- purrr::map_chr(.x = temp[linkFilter], .f = function(x) x %>% rvest::html_text('href'))
    # }
    if (export == TRUE) {
        writeLines(links, file.path(project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = "-")))
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
