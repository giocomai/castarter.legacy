#' Extracts textual contents from a vector of html files
#'
#' Extracts textual contents from a vector of html files.
#'
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links) are stored. If not given, it defaults to the Html folder inside project/website folders.
#' @param metadata Defaults to NULL. A data.frame presumably created with ExportMetadata() including information on all articles. Number of rows must correspond to the number of articles to be elaborated. This is required when export == TRUE, in order to provide meaningful filenames.
#' @param keepEverything Logical. If TRUE, the functions calls the boilerpipeR::KeepEverythingExtractor from boilerpipeR, instead of boilerpipeR::ArticleExtractor.
#' @param removeString A character vector of one or more strings. Provided strings are removed from each article.
#' @param export Logical, defaults to TRUE. If TRUE, textual contents are saved as individual txt files in a dedicated folder. Filename is based on the medatadata.
#' @param maxTitleCharacters Maximum number of characters allowed in the title. Defaults to 80.
#' @param removePunctuationInFilename Logical, defaults to TRUE. If TRUE (and export == TRUE), it removes punctuation signs from filemanes to prevent errors in saving files.
#' @return A character vector of text, and individual articles saved as txt files in a dedicated folder if 'export' is set to TRUE.
#' @export
#' @examples
#' text <- ExtractText(container = "div", containerClass = "article")
ExtractText <- function(container = NULL,
                        containerClass = NULL,
                        containerId = NULL,
                        htmlLocation = NULL,
                        metadata = NULL, export = FALSE, maxTitleCharacters = 80, removeString = NULL, divClass = NULL, divId = NULL, customXpath = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL, removePunctuationInFilename = TRUE, removeTitleFromTxt = FALSE, titles = NULL, keepEverything = FALSE, exportParameters = TRUE, progressBar = TRUE, project = NULL, website = NULL) {
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
                for (i in seq_along(params$ExtractText)) {
                    assign(names(params$ExtractText)[i], params$ExtractText[[i]])
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
        params$ExtractText <-  as.list(environment())
        saveRDS(object = params, file = paramsFile)
    }
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(project, website, "Html")
    }
    # list files
    HtmlFiles <- list.files(path = htmlLocation, full.names = TRUE)
    # put them in order [equivalent to gtools::mixedorder()]
    HtmlFiles <- HtmlFiles[stringr::str_extract(string = HtmlFiles, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    text <- vector(mode = "character", length = length(HtmlFiles))
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = length(HtmlFiles), style = 3, title = "Extracting text")
    }
    # if no div or such, get all links
    for (i in seq_along(HtmlFiles)) {
        temp <-  xml2::read_html(HtmlFiles[i])
        if (is.element("xml_node", set = class(temp))==TRUE) {
            if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE) {
                temp <- temp %>%
                    rvest::html_nodes(container) %>% rvest::html_text()
            } else if (is.null(containerClass)==FALSE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']")) %>%
                    rvest::html_text()
            } else if (is.null(containerClass)==FALSE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = paste0("//", container, "[@id='", containerId, "']")) %>%
                    rvest::html_text()
            }
            if (length(temp)>1) {
                text[i] <- temp[1]
                warning(paste0("ID", stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L), ": Found more than one string per page, keeping only first occurrence."))
            } else if (length(temp)==0) {
                text[i] <- NA
            } else {
                text[i] <- temp
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    }
    if (progressBar == TRUE) {
        close(pb)
    }
    if (removeTitleFromTxt == TRUE) {
        if (is.null(titles) == TRUE) {
            if (is.null(metadata) == TRUE) {
                stop("When removeTitleFromTxt, either metadata or titles must be provided.")
            } else {
                titles <- metadata$titles
            }
        }
        for (i in seq_along(text)) {
            if (is.null(titles[i])==FALSE) {
                if (titles[i]!="") {
                    text[i] <- base::sub(pattern = titles[i], replacement = "", x = text[i], fixed = TRUE)
                }
            }
        }
    }
    if (is.null(removeEverythingAfter) == FALSE) {
        text <- base::gsub(base::paste0(removeEverythingAfter, ".*"), "", text, fixed = FALSE)
    }
    if (is.null(removeEverythingBefore) == FALSE) {
        text <- base::gsub(base::paste0(".*", removeEverythingBefore), "", text, fixed = FALSE)
    }
    if (is.null(removeString) == FALSE) {
        for (i in 1:length(removeString)) {
            text <- gsub(removeString[i], "", text, fixed = TRUE)
        }
    }
    if (export == TRUE) {
        for (i in 1:length(text)) {
            base::write(text[i], file = txtFilenames[i])
        }
    }
    return(text)
}
