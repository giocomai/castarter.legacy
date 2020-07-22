#' Extracts titles of individual pages
#'
#' Extracts titles of individual pages from a vector of html files or from a named vector of links.
#'
#' @param id Defaults to NULL. If provided, it should be a vector of integers. Only html files corresponding to given id in the relevant htmlLocation will be processed.
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links) are stored. If not given, it defaults to the Html folder inside project/website folders.
#' @param links A named character vector, typically created by the ExtractLinks function.
#' @param removeString A character vector of one or more strings to be removed from the extracted title.
#' @param container HTML element where the title is found. The title can usually be found in one of the following:
##' \itemize{
##'  \item{"links"}{: Extract the title from links (required). Titles are taken from the textual element of the link taken from the index pages. }
##'  \item{"title"}{: Default. Extract the title from the Html <title> field, usually shown on the top bar of web browsers.}
##'  \item{"h1"}{: Extract the title from the first occurence of text that has heading 1, the <h1> html tag, as its style, unless containerInstance is provided.}
##'  \item{"h2"}{: Extract the title from the first occurence of text that has heading 2, the <h2> html tag, as its style, unless containerInstance is provided.}
##' }
#' @param containerInstance Defaults to NULL. If given, it must be an integer. If a given element is found more than once in the same page, it keeps only the relevant occurrence for further extraction.
#' @param removeEverythingAfter Removes everything after given string.
#' @param maxCharacters An integer. Defines the maximum number of characters to be kept in the output for each title.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown (useful for example when including scripts in rmarkdown)
#' @param importParameters Defaults to NULL. If TRUE, ignores all parameters given in the function call, and imports them from parameters file stored in "project/website/Logs/parameters.rds".
#' @param exportParameters Defaults to TRUE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus. Requires parameters project/website.
#' @return A character vector of article titles.
#' @export
#' @examples
#' \dontrun{
#' titles <- ExtractTitles()
#' }
ExtractTitles <- function(container = "title",
                          containerClass = NULL,
                          containerId = NULL,
                          containerInstance = NULL,
                          htmlLocation = NULL,
                          id = NULL,
                          links = NULL,
                          removePunctuation = FALSE,
                          onlyStandardCharacters = FALSE,
                          removeString = NULL,
                          removeEverythingBefore = NULL,
                          removeEverythingAfter = NULL,
                          customXpath = NULL,
                          maxCharacters = NULL,
                          encoding = "UTF-8",
                          progressBar = TRUE,
                          exportParameters = TRUE,
                          importParameters = NULL,
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
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with SetCastarter(project = '...', website = '...').")
    }
    if (is.null(importParameters)==FALSE) {
        if (importParameters == TRUE) { # Import parameters
            if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
                params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
                params$ExtractTitles$exportParameters <- FALSE
                if (is.null(id)==FALSE) {
                    params$ExtractTitles$id <- id
                }
                for (i in seq_along(params$ExtractTitles)) {
                    assign(names(params$ExtractTitles)[i], params$ExtractTitles[[i]])
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
        extractTitlesParams <-  as.list(environment())
        if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
            params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
            params$ExtractTitles <- NULL
        } else {
            params <- list()
        }
        params$ExtractTitles <- extractTitlesParams
        saveRDS(object = params, file = base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
    }

    if (container == "indexLink"|container == "links") {
        titles <- names(links)
    } else {
        # define htmlLocation, if not given
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
        titles <- vector(mode = "character", length = length(HtmlFiles))
        if (progressBar == TRUE) {
            pb <- dplyr::progress_estimated(n = length(HtmlFiles), min_time = 1)
        }
        # if no div or such, get all links
        for (i in seq_along(HtmlFiles)) {
            if (progressBar == TRUE) {
                pb$tick()$print()
            }
            temp <-  tryCatch(expr = xml2::read_html(HtmlFiles[i], encoding = encoding),
                              error = function(e) {
                                  warning(paste("Could not read", HtmlFiles[i]))
                                  NA
                              })
            if (is.element("xml_node", set = class(temp))==TRUE) {
                if (is.null(customXpath)==FALSE) {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = customXpath)%>%
                        rvest::html_text()
                } else if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE) {
                     temp <- temp %>%
                        rvest::html_nodes(container) %>% rvest::html_text()
                } else if (is.null(containerClass)==FALSE) {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']")) %>%
                        rvest::html_text()
                } else if (is.null(containerId)==FALSE) {
                    temp <- temp %>%
                        rvest::html_nodes(xpath = paste0("//", container, "[@id='", containerId, "']")) %>%
                        rvest::html_text()
                }
                if (length(temp)>1) {
                    if (is.null(containerInstance)==FALSE) {
                        titles[i] <- temp[containerInstance]
                    } else {
                        titles[i] <- temp[1]
                        warning(paste0("ID", stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L), ": Found more than one string per page, keeping only first occurrence."))
                    }
                } else if (length(temp)==0) {
                    titles[i] <- NA
                } else {
                    titles[i] <- temp
                }
            }
        }
    }
    if (is.null(removeString) == FALSE) {
        titles <- gsub(removeString, replacement = "", x = titles, fixed = TRUE)
    }
    if (is.null(removeEverythingAfter) == FALSE) {
        titles <- gsub(paste0(removeEverythingAfter, ".*"), replacement = "", x = titles)
    }
    if (is.null(removeEverythingBefore) == FALSE) {
        titles <- gsub(paste0(".*", removeEverythingBefore), replacement = "", x = titles)
    }
    if (is.null(onlyStandardCharacters) == FALSE) {
        if (onlyStandardCharacters == TRUE) {
            titles <- gsub("[^A-Za-z0-9 ]", "-", titles)
            titles <- gsub("  ", " ", titles)
            titles <- gsub("--", "-", titles)
        }
    }
    if (is.null(removePunctuation == FALSE)) {
        if (removePunctuation == TRUE) {
            titles <- gsub("[[:punct:]]", "-", titles)
            titles <- gsub("  ", " ", titles)
            titles <- gsub("--", "-", titles)
        }
    }
    if (is.null(maxCharacters) == FALSE) {
        titles <- substring(titles, 1, maxCharacters)
    }
    return(titles)
}

#' Extracts id from filename
#'
#' Extracts id from filename
#'
#' @param sample Defaults to NULL. If given, it must be an integer corresponding to the desired sample size.
#' @param htmlLocation Path to folder where html files, tipically downloaded with DownloadContents(links) are stored. If not given, it defaults to the `Html` folder inside project/website folders.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no website is provided, exported files are saved in the project/Outputs folder.
#' @return A vector of the integer class.
#' @export
#' @examples
#' \dontrun{
#' id <- ExtractId(project, website)
#' }
ExtractId <- function(sample = NULL,
                      htmlLocation = NULL,
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
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(baseFolder, project, website, "Html")
    }
    htmlFilesList <- list.files(htmlLocation)
    if (is.null(sample)==FALSE) {
        sort.int(x = sample(x = as.integer(stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+")), size = sample, replace = FALSE))
    } else {
        sort.int(x = as.integer(stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+")))
    }
}


#' Exports metadata
#'
#' Exports metadata to a vector, csv or xlsx file.
#'
#' @param id A vector of integers, usually created with `ExtractId()`.
#' @param dates A vector of dates, usually created with `ExtractDates()`.
#' @param titles A chacter vector, usually created with `ExtractTitles()`.
#' @param language A chacter vector, usually of length 1 (e.g. \"english"). It can be of the same length of other vectors when multiple languages are included.
#' @param links A chacter vector, usually created with `ExtractLinks()`. Only links matching the given `id` are included in the metadata.
#' @param exportCsv If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @param exportXlsx If equal to TRUE, exports the metadata in the .xlsx file format in the Dataset sub-folder.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A vector of the data.frame class.
#' @export
#' @examples
#' \dontrun{
#' metadata <- ExportMetadata(id, dates, titles, language, links)
#' }
ExportMetadata <- function(id,
                           dates,
                           titles,
                           language,
                           links,
                           exportCsv = FALSE,
                           exportXlsx = FALSE,
                           accordingToDate = FALSE,
                           ignoreNAdates = FALSE,
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
    ignoreVector <- NULL
    if (ignoreNAdates == TRUE) {
        ignoreVector <- is.na(dates)
    }
    if (is.null(ignoreVector) == FALSE) {
        links <- links[ignoreVector]
    }
    metadata <- tibble::tibble(doc_id = paste(website, id, sep = "-"),
                               website = website,
                               id = id,
                               date = dates,
                               title = titles,
                               language = language,
                               link = links[id])
    if (accordingToDate == TRUE) {
        metadata <- metadata[order(metadata$date), ]
    }
    if (exportCsv == TRUE) {
        readr::write_csv(x = metadata,
                         path = file.path(baseFolder, project, website,
                                          "Dataset",
                                          paste(Sys.Date(), website, "metadata.csv", sep = "-")))
    }
    if (exportXlsx == TRUE) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("Package \"openxlsx\" needed for exporting to xlsx format. You can install it with `install.packages(\"openxlsx\")`.",
                 call. = FALSE)
        } else {
            openxlsx::write.xlsx(x = metadata,
                                 file = file.path(
                                     baseFolder, project, website,
                                     "Dataset",
                                     paste(Sys.Date(), website, "metadata.xlsx", sep = "-")))
        }
    }
    return(metadata)
}

#' Exports dataset
#'
#' Exports dataset to a data.frame, with options to save it as an R object, and export it as a .csv or .xlsx file.
#'
#' @param dataset A 'castarter' dataset.
#' @param text A character vector typically created with ExtractText().
#' @param metadata A data.frame typically created with ExportMetadata(). Number of rows must be the same as the number of text items in `text`.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportRdsIf equal to TRUE, exports the complete dataset in R's .rds file format in the Dataset sub-folder.
#' @param exportCsv If equal to TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return A data.frame, a 'castarter' dataset.
#' @export
#' @examples
#' \dontrun{
#' dataset <- ExportDataset(text, metadata, project, website)
#' }
ExportDataset <- function(dataset = NULL,
                          text = NULL,
                          metadata = NULL,
                          exportRds = FALSE,
                          exportRdata = FALSE,
                          exportCsv = FALSE,
                          exportXlsx = FALSE,
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
    if (is.null(dataset)==TRUE) {
        dataset <- dplyr::bind_cols(tibble::tibble(doc_id = metadata$doc_id, text = text),
                                    metadata %>% dplyr::select(-doc_id))
    }
    if (exportRds == TRUE) {
        saveRDS(object = dataset, file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds")))
        message(paste("Dataset saved in", file.path(baseFolder, project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds"))))
    }
    if (exportRdata == TRUE) {
        save(dataset, file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData")))
        print(paste("Dataset saved in", file.path(baseFolder, project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData"))))
    }
    if (exportCsv == TRUE) {
        utils::write.csv(dataset, file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".csv")))
        print(paste("Dataset saved as .csv file in", file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".csv"))))
    }
    if (exportXlsx == TRUE) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("Package \"openxlsx\" needed for exporting to xlsx format. You can install it with `install.packages(\"openxlsx\")`.",
                 call. = FALSE)
        } else {
            openxlsx::write.xlsx(x = metadata,
                                 file = file.path(baseFolder, project, website,
                                                  "Dataset",
                                                  paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx")))
            message(paste("Dataset saved as .xlsx file in",
                          file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx"))))
        }
    }
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(dataset, file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx")))

    }
    invisible(dataset)
}
