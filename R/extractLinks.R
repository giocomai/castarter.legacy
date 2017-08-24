#' Extracts direct links to individual articles from index pages.
#'
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @param partOfLink Part of URL found only in links of individual articles to be downloaded. If more than one provided, it includes all links that contains either of the strings provided.
#' @param partOfLinkToExclude If an URL includes this string, it is excluded from the output. One or more strings may be provided.
#' @param indexLinks A character vector, defaults to NULL. If provided, indexLinks are removed from the extracted articlesLinks.
#' @param containerType Type of html container from where links are to be extracted, such as "ul", "div", and others. containerClass must also be provided.
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
                         extractText = TRUE, containerType = NULL, containerClass = NULL, containerId = NULL, divClass = NULL, attributeType = NULL, minLength = NULL, maxLength = NULL, indexLinks = NULL, sortLinks = TRUE, linkTitle = TRUE, export = FALSE, appendString = NULL, removeString = NULL, progressBar = TRUE, project = NULL, website = NULL,
                         importParameters = NULL,
                         exportParameters = TRUE) {
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
    # list files and keep in order
    indexHtml <- list.files(path = file.path(project, website, "IndexHtml"), full.names = TRUE)[stringr::str_extract(string = indexHtml, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    temp <- purrr::flatten(purrr::map(.x = indexHtml, .f = function(x) read_html(x) %>% html_nodes("a")))
    links <- purrr::map_chr(.x = temp, .f = function(x) x %>% html_attr('href'))
    # introduce logical filter vector
    linkFilter <- seq_along(links)
    if (is.null(partOfLink)==FALSE) {
        linkFilter <- links %>% stringr::str_which(pattern = partOfLink)
    }
    if (is.null(partOfLinkToExclude)==FALSE) {
        linkFilter <- dplyr::setdiff(linkFilter, links %>% stringr::str_which(pattern = partOfLinkToExclude))
    }
    links <- links[linkFilter]
    links <- paste0(domain, links)
    if (extractText==TRUE) {
        names(links) <- purrr::map_chr(.x = temp[linkFilter], .f = function(x) x %>% html_text('href'))
    }
    links
}

