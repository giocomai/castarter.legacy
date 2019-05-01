#' Extracts textual contents from a vector of html files
#'
#' Extracts textual contents from a vector of html files.
#'
#' @param container Defaults to NULL. If provided, it must be an html element such as "div", "span", etc.
#' @param containerClass Defaults to NULL. If provided, also `container` must be given (and `containerId` must be NULL). Only text found inside the provided combination of container/class will be extracted.
#' @param containerId Defaults to NULL. If provided, also `container` must be given (and `containerClass` must be NULL). Only text found inside the provided combination of container/class will be extracted.
#' @param containerInstance Defaults to NULL. If given, it must be an integer. If a given element is found more than once in the same page, it keeps only the relevant occurrence for further extraction.
#' @param subElement Defaults to NULL. If provided, also `container` must be given. Only text within elements of given type under the chosen combination of container/containerClass will be extracted. When given, it will tipically be "p", to extract all p elements inside the selected div.
#' @param noChildren Defaults to FALSE, i.e. by default all subelements of the selected combination (e.g. div with given class) are extracted. If TRUE, only text found under the given combination (but not its subelements) will be extracted. Corresponds to the xpath string `/node()[not(self::div)]`.
#' @param id Defaults to NULL. If provided, it should be a vector of integers. Only html files corresponding to given id in the relevant htmlLocation will be processed.
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links) are stored. If not given, it defaults to the Html folder inside project/website folders.
#' @param metadata Defaults to NULL. A data.frame presumably created with ExportMetadata() including information on all articles. Number of rows must correspond to the number of articles to be elaborated. This is required when export == TRUE, in order to provide meaningful filenames.
#' @param removeEverythingBefore_pre Defaults to NULL. Everything before this string is removed before processing the HTML file.
#' @param removeEverythingAfter_pre Defaults to NULL. Everything after this string is removed before processing the HTML file.
#' @param keepEverything Logical. If TRUE, extracts all visible text.
#' @param removeString A character vector of one or more strings. Provided strings are removed from each article.
#' @param export Logical, defaults to TRUE. If TRUE, textual contents are saved as individual txt files in a dedicated folder. Filename is based on the medatadata.
#' @param maxTitleCharacters Maximum number of characters allowed in the title. Defaults to 80.
#' @param removePunctuationInFilename Logical, defaults to TRUE. If TRUE (and export == TRUE), it removes punctuation signs from filemanes to prevent errors in saving files.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown.
#' @return A character vector of text, and individual articles saved as txt files in a dedicated folder if 'export' is set to TRUE.
#' @export
#' @examples
#' \dontrun{
#' text <- ExtractText(container = "div", containerClass = "article")
#' }
ExtractText <- function(container = NULL,
                        containerClass = NULL,
                        containerId = NULL,
                        containerInstance = NULL,
                        subElement = NULL,
                        noChildren = NULL,
                        htmlLocation = NULL,
                        id = NULL,
                        encoding = "UTF-8",
                        metadata = NULL, export = FALSE, maxTitleCharacters = 80, removeString = NULL, customXpath = NULL,
                        removeEverythingAfter_pre = NULL,
                        removeEverythingBefore_pre = NULL,
                        removeEverythingBefore = NULL,
                        removeEverythingAfter = NULL,
                        keepEverything = FALSE,
                        removePunctuationInFilename = TRUE, removeTitleFromTxt = FALSE, titles = NULL,
                        importParameters = NULL,
                        exportParameters = TRUE,
                        progressBar = TRUE, project = NULL, website = NULL) {
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
                params$ExtractText$exportParameters <- FALSE
                if (is.null(id)==FALSE) {
                    params$ExtractText$id <- id
                }
                for (i in seq_along(params$ExtractText)) {
                    assign(names(params$ExtractText)[i], params$ExtractText[[i]])
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
        ExtractTextParams <-  as.list(environment())
        if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
            params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
            params$ExtractText <- NULL
        } else {
            params <- list()
        }
        params$ExtractText <- ExtractTextParams
        saveRDS(object = params, file = base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
    }

    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(baseFolder, project, website, "Html")
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
    text <- vector(mode = "character", length = length(HtmlFiles))
    if (progressBar == TRUE) {
        pb <- dplyr::progress_estimated(n = length(HtmlFiles), min_time = 1)
    }
    for (i in seq_along(HtmlFiles)) {
        if (progressBar == TRUE) {
            pb$tick()$print()
        }
        if (is.null(removeEverythingAfter_pre)==FALSE|is.null(removeEverythingBefore_pre)==FALSE) {
            temp <-  tryCatch(expr = paste(readLines(HtmlFiles[i]), collapse = "\n"),
                              error = function(e) {
                                  warning(paste("Could not read", HtmlFiles[i]))
                                  NA
                              })
            if (is.null(removeEverythingAfter_pre)==FALSE) {
                temp <- base::gsub(base::paste0(removeEverythingAfter_pre, ".*"), "", temp, fixed = FALSE)
            }
            if (is.null(removeEverythingBefore_pre)==FALSE){
                temp <- base::gsub(base::paste0(".*", removeEverythingBefore_pre), "", temp, fixed = FALSE)
            }
            temp <- tryCatch(expr = xml2::read_html(temp, encoding = encoding),
                             error = function(e) {
                                 warning(paste("Could not read", HtmlFiles[i]))
                                 NA
                             })
        } else {
            temp <-  tryCatch(expr = xml2::read_html(HtmlFiles[i], encoding = encoding),
                              error = function(e) {
                                  warning(paste("Could not read", HtmlFiles[i]))
                                  NA
                              })
        }
        if (is.element("xml_node", set = class(temp))==TRUE) {
            if (keepEverything == TRUE) {
                temp <- temp %>% rvest::html_text()
            } else if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE) {
                if (is.null(subElement)==TRUE) {
                    temp <- temp %>%
                        rvest::html_nodes(container) %>%
                        rvest::html_text()
                } else {
                    temp <- temp %>%
                        rvest::html_nodes(container) %>%
                        rvest::html_nodes(subElement) %>%
                        rvest::html_text()
                }
            } else if (is.null(containerClass)==FALSE&is.null(containerId)==TRUE) {
                if (is.null(subElement)==TRUE) {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']")) %>%
                        rvest::html_text()
                } else {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']")) %>%
                        rvest::html_nodes(subElement) %>%
                        rvest::html_text() %>%
                        paste(collapse = "\n")
                }

            } else if (is.null(containerClass)==TRUE&is.null(containerId)==FALSE) {
                if (is.null(subElement)==TRUE) {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@id='", containerId, "']")) %>%
                        rvest::html_text()
                } else {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@id='", containerId, "']")) %>%
                        rvest::html_nodes(subElement) %>%
                        rvest::html_text()
                }
            }
            if (length(temp)>1) {
                if (is.null(subElement)==TRUE) {
                    if (is.null(containerInstance)==TRUE) {
                        text[i] <- temp[containerInstance]
                    } else {
                        text[i] <- paste(temp, collapse = "\n")
                        warning(paste0("ID", stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L), ": Multiple strings matching crieria have been collapsed."))
                    }

                } else {
                    text[i] <- paste(temp, collapse = "\n")
                }
            } else if (length(temp)==0) {
                text[i] <- NA
            } else {
                text[i] <- temp
            }
        }
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
        text <- stringr::str_replace(string = text, pattern = stringr::regex(base::paste0(removeEverythingAfter, ".*"), dotall = TRUE), replacement = "")
    }
    if (is.null(removeEverythingBefore) == FALSE) {
        text <- stringr::str_replace(string = text, pattern = stringr::regex(base::paste0(".*", removeEverythingBefore), dotall = TRUE), replacement = "")
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
