#' Extracts dates from a vector of html files
#'
#' Extracts dates from a vector of html files.
#'
#' @param id Defaults to NULL. If provided, it should be a vector of integers. Only html files corresponding to given id in the relevant htmlLocation will be processed.
#' @param dateFormat A string expressing the date format. In line with standards (see ?strptime), 'd' stands for day, 'm' stands for month in figures, 'b' for months spelled out as words, 'y' as year without the century, 'Y' as year with four digits. Standard separation marks among parts of the date (e.g. '-', '/', '.') should not be included. The following date formats are available :
##' \itemize{
##'  \item{"dmY"}{: Default.}
##'  \item{"Ymd"}{: }
##'  \item{"dbY"}{: }
##'  \item{"YBd"}{: }
##'  \item{"dB,Y"}{: }
##'  \item{"db.'y"}{: }
##'  \item{"Bd,Y"}{: }
##'  \item{"xdBY"}{: customString must be provided.}
##' }
#' @param customRegex Defaults to NULL. If provided, regex parsing pre-data extraction will follow this forumula, e.g. `[[:digit:]][[:digit:]][[:punct:]][[:space:]][[:digit:]][[:digit:]][[:punct:]][[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]`.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param language Provide a language in order to extract name of months. Defaults to the locale currently active in R (usually, the system language). Generic forms such as "english" or "russian", are usually accepted. See ?locales for more details. On linux, you can run system("locale -a", intern = TRUE) to see all available locales.
#' @param attribute Defaults to NULL. Can be specified only if customXpath is given, in order to extract a given attribute e.g. if customXpath = "//meta[@property='article:published_time']", and attribute = "content".
#' @param encoding Defaults to NULL. If source is not in UTF, encoding can be specified here for conversion. A list of valid values can be found using iconvlist().
#' @param keepAllString Logical, defaults to FALSE. If TRUE, it directly tries to parse the date with the given dateFormat, without trying to polish the string provided accordingly.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown.
#' @param importParameters Defaults to NULL. If TRUE, ignores all parameters given in the function call, and imports them from parameters file stored in "project/website/Logs/parameters.rds".
#' @param exportParameters Defaults to TRUE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus. Requires parameters project/website.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return A vector of the Date class.
#' @export
#' @examples
#' dates <- ExtractDates(articlesHtml)
ExtractDates <- function(dateFormat = "dmY",
                         container = NULL,
                         containerClass = NULL,
                         containerId = NULL,
                         htmlLocation = NULL,
                         id = NULL,
                         customXpath = NULL,
                         customRegex = NULL,
                         attribute = NULL,
                         language = Sys.getlocale(category = "LC_TIME"),
                         customString = "",
                         minDate = NULL,
                         maxDate = NULL,
                         encoding = "UTF-8",
                         keepAllString = FALSE,
                         removeEverythingBefore = NULL,
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
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with SetCastarter(project = '...', website = '...').")
    }
    paramsFile <- base::file.path(project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))
    if (is.null(importParameters)==FALSE) {
        if (importParameters == TRUE) { # Import parameters
            if (file.exists(paramsFile) == TRUE) {
                params <- readRDS(paramsFile)
                for (i in seq_along(params$ExtractDates)) {
                    assign(names(params$ExtractDates)[i], params$ExtractDates[[i]])
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
        params$ExtractDates <-  as.list(environment())
        saveRDS(object = params, file = paramsFile)
    }
    # define htmlLocation, if not given
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(project, website, "Html")
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
    datesTxt <- vector(mode = "character", length = length(HtmlFiles))
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = length(HtmlFiles), style = 3, title = "Extracting dates")
    }
    for (i in seq_along(HtmlFiles)) {
        temp <-  tryCatch(expr = xml2::read_html(HtmlFiles[i]),
                          error = function(e) {
                              warning(paste("Could not read", HtmlFiles[i]))
                              NA
                          })
        if (is.element("xml_node", set = class(temp))==TRUE) {
            if (is.null(customXpath)==FALSE) {
                temp <- temp %>%
                    rvest::html_nodes(xpath = customXpath)
                if (is.null(attribute)) {
                    temp <-  temp %>%
                        rvest::html_text()
                } else {
                    temp <-  temp %>%
                        rvest::html_attr(attribute)
                    }

            } else if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE&is.null(removeEverythingBefore) == FALSE) {
                temp <- as.character(temp)
            } else if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE) {
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
                datesTxt[i] <- temp[1]
                warning(paste0("ID", stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L), ": Found more than one string per page, keeping only first occurrence."))
            } else if (length(temp)==0) {
                datesTxt[i] <- NA
            } else {
                datesTxt[i] <- temp
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    }
    if (progressBar == TRUE) {
        close(pb)
    }
    if (keepAllString == FALSE) {
        if (is.null(customRegex) == FALSE) {
            for (i in seq_along(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr(customRegex, datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dby" | dateFormat == "dBy" | dateFormat == "dBY" | dateFormat == "dbY") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i],
                                      regexpr("[[:digit:]]?[[:digit:]][[:space:]]?[[:space:]][[:alpha:]]*[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]",
                                              datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "YBd" | dateFormat == "ybd") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]]?[[:digit:]]?[[:digit:]][[:digit:]][[:space:]]?[[:punct:]]?[[:alpha:]]*[[:space:]]?[[:punct:]]?[[:digit:]][[:digit:]]?", datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dB,Y") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*,[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?", datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "db.'y") {
            datesTxt <- rep(NA, length(datesTxt))
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*.'[[:digit:]][[:digit:]][[:digit:]]?", datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "Bd,Y" | dateFormat == "bd,Y") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:alpha:]]*[[:space:]][[:digit:]]?[[:digit:]],[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?",
                                                               datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "xdBY") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr(paste0(customString, "[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?"),
                                                               datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
            dateFormat <- "dBY"
        } else if (dateFormat == "ymd" | dateFormat == "Ymd") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:punct:]]?[[:digit:]]?[[:digit:]][[:punct:]]?[[:digit:]]?[[:digit:]]", datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dmy" | dateFormat == "mdy") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]][[:digit:]]",
                                                               datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dmY" | dateFormat == "mdY") {
            for (i in 1:length(datesTxt)) {
                dateTxt <- regmatches(datesTxt[i], regexpr("[[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]", datesTxt[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        }
    }
    if (language == "ru" | language == "russian") {
        monthsRu <- c("Января", "Февраля", "Марта", "Апреля", "Мая", "Июня", "Июля", "Августа", "Сентября",
                      "Октября", "Ноября", "Декабря")
        monthsEn <- month.name
        monthsRu <- tolower(monthsRu)
        datesTxt <- tolower(datesTxt)
        for (i in 1:12) {
            datesTxt <- gsub(monthsRu[i], monthsEn[i], datesTxt)
        }
        dates <- as.Date(lubridate::parse_date_time(datesTxt, dateFormat))
    } else {
        dates <- as.Date(lubridate::parse_date_time(datesTxt, dateFormat, locale = language))
    }
    if (is.null(minDate) == FALSE) {
        dates[dates < as.Date(minDate)] <- NA
    }
    if (is.null(maxDate) == FALSE) {
        dates[dates > as.Date(maxDate)] <- NA
    }
    return(dates)
}

#' Merge alternative sets of dates for a given dataset
#'
#' Merge alternative sets of dates for a given dataset.
#'
#' @param dates1, dates2, dates3 Vectors of the POSIXct class to be merged. Where a value in dates1 is NA, MergeDates looks for a corresponding value in dates2 and dates3.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param fillMissingDates Logical, defaults to FALSE. If TRUE fills missing NA dates with interpolated values. It is sensible to use this if dates are in a sequence, and if the corresponding 'contents' section includes actual contents (i.e. missing date is not effectively due to a missing article or a link included in the dataset by mistake)
#' @param maxgap Integer, defaults to 5. Maximum number of consecutive NAs to fill.
#' @return A vector of the POSIXct class.
#' @export
#' @examples
#' dates <- MergeDates(dates1, dates2)
MergeDates <- function(dates1, dates2, dates3 = NULL, minDate = NULL, maxDate = NULL, fillMissingDates = FALSE, maxgap = 5) {
    dates <- dates1
    for (i in 1:length(dates)) {
        if (is.na(dates[i])) {
            dates[i] <- dates2[i]
        }
        if (is.null(dates3)==FALSE) {
            if (is.na(dates[i])) {
                dates[i] <- dates3[i]
            }
        }
        if (is.null(minDate) == FALSE & is.null(maxDate) == FALSE) {
            if (is.na(dates[i]) == FALSE & dates[i] < as.POSIXct(minDate) | is.na(dates[i]) == FALSE & dates[i] > as.POSIXct(maxDate)) {
                dates[i] <- NA
            }
        }
    }
    if (fillMissingDates == TRUE) {
        dates <- zoo::na.approx(object = dates, na.rm = FALSE, maxgap = maxgap)
        dates <- as.POSIXct(x = dates, origin = "1970-01-01")
    }
    dates
}


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
##'  \item{"h1"}{: Extract the title from the first occurence of text that has heading 1, the <h1> html tag, as its style.}
##'  \item{"h2"}{: Extract the title from the first occurence of text that has heading 2, the <h2> html tag, as its style.}
##' }
#' @param removeEverythingAfter Removes everything after given string.
#' @param maxCharacters An integer. Defines the maximum number of characters to be kept in the output for each title.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown (useful for example when including scripts in rmarkdown)
#' @param importParameters Defaults to NULL. If TRUE, ignores all parameters given in the function call, and imports them from parameters file stored in "project/website/Logs/parameters.rds".
#' @param exportParameters Defaults to TRUE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus. Requires parameters project/website.
#' @return A character vector of article titles.
#' @export
#' @examples
#' titles <- ExtractTitles(articlesHtml)
ExtractTitles <- function(container = "title",
                          containerClass = NULL,
                          containerId = NULL,
                          htmlLocation = NULL,
                          id = NULL,
                          links = NULL,
                          removePunctuation = FALSE,
                          onlyStandardCharacters = FALSE,
                          removeString = NULL,
                          removeEverythingBefore = NULL,
                          removeEverythingAfter = NULL,
                          customXpath = "",
                          maxCharacters = NULL,
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
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with SetCastarter(project = '...', website = '...').")
    }
    paramsFile <- base::file.path(project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))
    if (is.null(importParameters)==FALSE) {
        if (importParameters == TRUE) { # Import parameters
            if (file.exists(paramsFile) == TRUE) {
                params <- readRDS(paramsFile)
                for (i in seq_along(params$ExtractTitles)) {
                    assign(names(params$ExtractTitles)[i], params$ExtractTitles[[i]])
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
        params$ExtractTitles <-  as.list(environment())
        saveRDS(object = params, file = paramsFile)
    }
    if (container == "indexLink"|container == "links") {
        titles <- names(links)
    } else {
        # define htmlLocation, if not given
        if (is.null(htmlLocation)) {
            htmlLocation <- file.path(project, website, "Html")
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
            pb <- txtProgressBar(min = 0, max = length(HtmlFiles), style = 3, title = "Extracting titles")
        }
        # if no div or such, get all links
        for (i in seq_along(HtmlFiles)) {
            temp <-  tryCatch(expr = xml2::read_html(HtmlFiles[i]),
                              error = function(e) {
                                  warning(paste("Could not read", HtmlFiles[i]))
                                  NA
                              })
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
                    titles[i] <- temp[1]
                    warning(paste0("ID", stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L), ": Found more than one string per page, keeping only first occurrence."))
                } else if (length(temp)==0) {
                    titles[i] <- NA
                } else {
                    titles[i] <- temp
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
        if (progressBar == TRUE) {
            close(pb)
        }
    }
    if (is.null(removeString) == FALSE) {
        titles <- gsub(removeString, replacement = "", x = titles, fixed = TRUE)
    }
    if (is.null(removeEverythingAfter) == FALSE) {
        titles <- gsub(paste0(removeEverythingAfter, ".*"), replacement = "", x = titles)
    }
    if (is.null(removeEverythingBefore) == FALSE) {
        titles <- gsub(paste0(removeEverythingBefore, "*."), replacement = "", x = titles)
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
#' id <- ExtractId(project, website)
ExtractId <- function(sample = NULL, htmlLocation = NULL, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(htmlLocation)) {
        htmlLocation <- file.path(project, website, "Html")
    }
    htmlFilesList <- list.files(file.path(project, website, "Html"))
    if (is.null(sample)==FALSE) {
        sort.int(x = sample(x = as.integer(stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+")), size = sample, replace = FALSE))
    } else {
        sort.int(x = as.integer(stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+")))
    }
}

#' Extracts metadata and text from local html files
#'
#' Extracts metadata and text from local html files.
#'
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no website is provided, exported files are saved in the project/Outputs folder.
#' @return A data.frame with the complete dataset.
#' @export
#' @examples
#' dataset <- CreateDatasetFromHtml()
CreateDatasetFromHtml <- function(links = NULL,
                                  dateFormat = NULL, divClass_ExtractDates = NULL, spanClass_ExtractDates = NULL, customXpath_ExtractDates = NULL, language_ExtractDates = Sys.getlocale(category = "LC_TIME"), removeEverythingBefore_ExtractDates = NULL,
                                  method_ExtractTitles = "htmlTitle", removeString_ExtractTitles = NULL, removeEverythingAfter_ExtractTitles = NULL, removePunctuation_ExtractTitles = NULL,
                                  divClass_ExtractTxt = NULL, divId_ExtractTxt = NULL, removeString_ExtractTxt = NULL, removeEverythingAfter_ExtractTxt = NULL, removeEverythingBefore_ExtractTxt = NULL, removeTitleFromTxt = FALSE, customXpath_ExtractTxt = NULL,
                                  language = "English", encoding = NULL,
                                  logProgress = FALSE,
                                  exportParameters = TRUE, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with .")
    }
    if (exportParameters == TRUE) {
        args <- c("dateFormat_ExtractDates", "divClass_ExtractDates", "spanClass_ExtractDates", "customXpath_ExtractDates", "language_ExtractDates", "removeEverythingBefore_ExtractDates",
                  "method_ExtractTitles", "removeString_ExtractTitles", "removeEverythingAfter_ExtractTitles", "removePunctuation_ExtractTitles",
                  "divClass_ExtractTxt", "divId_ExtractTxt", "removeString_ExtractTxt", "removeEverythingAfter_ExtractTxt", "removeEverythingBefore_ExtractTxt",
                  "language", "encoding")
        param <- list(dateFormat, divClass_ExtractDates, spanClass_ExtractDates, customXpath_ExtractDates, language_ExtractDates, removeEverythingBefore_ExtractDates,
                      method_ExtractTitles, removeString_ExtractTitles, removeEverythingAfter_ExtractTitles, removePunctuation_ExtractTitles,
                      divClass_ExtractTxt, divId_ExtractTxt, removeString_ExtractTxt, removeEverythingAfter_ExtractTxt, removeEverythingBefore_ExtractTxt,
                      language, encoding)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - "))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp
        }
        write.table(updateParameters, file = base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")))
    }
    castarter::CreateFolders(project = project, website = website)
    htmlFilesList <- gtools::mixedsort(list.files(file.path(project, website, "Html"), pattern = "\\.html$", full.names = TRUE))
    numberOfArticles <- length(htmlFilesList)
    id <- as.integer(stringr::str_extract(string = stringr::str_extract(string = htmlFilesList, pattern = "/[[:digit:]]+\\.html$"), pattern = "[[:digit:]]+"))
    date <- as.POSIXct(rep(NA, numberOfArticles))
    contents <- rep(NA, numberOfArticles)
    if (method_ExtractTitles == "indexLink") {
        title <- ExtractTitles(articlesHtml = NULL, links = links, method = "indexLink", removeString = removeString_ExtractTitles, removeEverythingAfter = removeEverythingAfter_ExtractTitles, exportParameters = FALSE)
    } else {
        title <- rep(NA, numberOfArticles)
    }
    x <- 1
    xlist <- seq(0,numberOfArticles,by=100)
    for (i in 1:numberOfArticles) {
        htmlFile <- readLines(htmlFilesList[i])
        if (length(htmlFile)==0) {
            htmlFile <- "<head></head><body><p></p></body>"
        }
        htmlFile <- paste(htmlFile, collapse = "\n")
        if (is.null(encoding) == FALSE) {
            htmlFile <- iconv(htmlFile, from = encoding, to = "utf8")
        }
        dateTemp <- ExtractDates(articlesHtml = htmlFile, dateFormat = dateFormat, divClass = divClass_ExtractDates, spanClass = spanClass_ExtractDates, customXpath = customXpath_ExtractDates, language = language_ExtractDates, removeEverythingBefore = removeEverythingBefore_ExtractDates, exportParameters = FALSE)
        if (length(dateTemp) == 1) {
            date[i] <- dateTemp
        } else {
            date[i] <- NA
        }
        contents[i] <- ExtractTxt(articlesHtml = htmlFile, export = FALSE, removeEverythingAfter = removeEverythingAfter_ExtractTxt, removeEverythingBefore = removeEverythingBefore_ExtractTxt, divClass = divClass_ExtractTxt, divId = divId_ExtractTxt, removeString = removeString_ExtractTxt, customXpath = customXpath_ExtractTxt, progressBar = FALSE, exportParameters = FALSE)
        if (method_ExtractTitles != "indexLink") {
            titleTemp <- ExtractTitles(articlesHtml = htmlFile, method = method_ExtractTitles, removeString = removeString_ExtractTitles, progressBar = FALSE)
            if (length(titleTemp) == 1) {
                title[i] <- titleTemp
            } else {
                title[i] <- NA
            }
        }
        x <- x + 1
        if (logProgress == TRUE) {
            write(paste("Html file", x, "processed."), file = file.path(project, website,"Logs", "CreateDatasetFromHtmlProgress.log"))
        }
        if (is.element(x, xlist)) {
            print(paste("Processed", x, "of", numberOfArticles, "articles."))
        }
    }
    if (is.null(links) == FALSE) {
        dataset <- data.frame(project, website, date, id, title, language, link = links, contents, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
        dataset <- data.frame(project, website, date, id, title, language, link = NA, contents, check.names = FALSE, stringsAsFactors = FALSE)
    }
    save(dataset, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData")))
    print(paste("Dataset saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData"))))
    invisible(dataset)
}

#' Exports metadata
#'
#' Exports metadata to a vector, csv or xlsx file.
#'
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return A vector of the data.frame class.
#' @export
#' @examples
#' metadata <- ExportMetadata(project, website, dates, id, titles, language, links)
ExportMetadata <- function(dates, id, titles, language, links, exportXlsx = FALSE, accordingToDate = FALSE, ignoreNAdates = FALSE, onlyExistingHtmlFiles = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    ignoreVector <- NULL
    if (onlyExistingHtmlFiles == TRUE) {
        htmlFilesList <- gtools::mixedsort(list.files(file.path(project, website, "Html"), full.names = TRUE))
        id <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
        idInTheory <- 1:length(links)
        ignoreVector <- !is.element(id, idInTheory)
    }
    if (ignoreNAdates == TRUE) {
        ignoreVector <- is.na(dates)
    }
    if (is.null(ignoreVector) == FALSE) {
        links <- links[ignoreVector]
    }
    metadata <- tibble::data_frame(doc_id = paste(website, id, sep = "-"), website = website, id = id, date = dates, title = titles, language = language, link = links, check.names = FALSE, stringsAsFactors = FALSE)
    if (accordingToDate == TRUE) {
        metadata <- metadata[order(metadata$date), ]
    }
    write.csv(metadata, file = file.path(project, website, "Dataset", paste(Sys.Date(), website, "metadata.csv", sep = " - ")), row.names = FALSE)
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(metadata, file = file.path(project, website, "Dataset", paste(Sys.Date(), website, "metadata.xlsx", sep = " - ")), row.names = FALSE)
    }
    metadata
}

#' Exports dataset
#'
#' Exports dataset to a data.frame, with options to save it as an R object, and export it as a .csv or .xlsx file.
#'
#' @param text A character vector.
#' @param metadata A data.frame typically created with ExportMetadata(). Number of rows must be the same as the number of text items in `text`.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportCsv If equal to TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return A data.frame, a 'castarter' dataset.
#' @export
#' @examples
#' dataset <- ExportDataset(contents, metadata, project, website)
ExportDataset <- function(text, metadata, exportRds = TRUE, exportRdata = FALSE, exportCsv = FALSE, exportXlsx = FALSE, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        project <- CastarterOptions("website")
    }
    dataset <- dplyr::bind_cols(tibble::data_frame(doc_id = metadata$doc_id, text = text), metadata %>% select(-doc_id))
    if (exportRds == TRUE) {
        saveRDS(object = dataset, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds")))
        message(paste("Dataset saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds"))))
    }
    if (exportRdata == TRUE) {
        save(dataset, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData")))
        print(paste("Dataset saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData"))))
    }
    if (exportCsv == TRUE) {
        utils::write.csv(dataset, file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".csv")))
        print(paste("Dataset saved as .csv file in", file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".csv"))))
    }
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(dataset, file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx")))
        print(paste("Dataset saved as .xlsx file in", file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx"))))
    }
    dataset
}

#' Extracts string included in an html element
#'
#' Extracts string included in an html element
#'
#' @param articlesHtml A character vector of html files.
#' @return A character vector with the contents of the given div or custom Xpath.
#' @export
#' @examples
#' string <- ExtractXpath(articlesHtml, divClass = "nameOfDiv")
ExtractXpath <- function(articlesHtml, divClass = NULL, spanClass = NULL, customXpath = NULL) {
    txt <- rep(NA, length(articlesHtml))
    pb <- txtProgressBar(min = 0, max = length(articlesHtml), style = 3)
    if (gtools::invalid(divClass) == FALSE) {
        for (i in seq_along(articlesHtml)) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)) == 0) {
                    txt[i] <- NA
                    print(paste("String in article with ID", i, "could not be extracted."))
                } else {
                    txt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)
                }
            }
            setTxtProgressBar(pb, i)
        }
    }
    if (gtools::invalid(spanClass)==FALSE) {
        for (i in seq_along(articlesHtml)) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
            tempStringXml <- XML::xpathSApply(articleHtmlParsed, paste0("//span[@class='", spanClass, "']"), XML::xmlValue)
            if (length(tempStringXml) == 0) {
                txt[i] <- NA
                print(paste("String in article with ID", i, "could not be extracted."))
            } else {
                txt[i] <- tempStringXml
            }
            setTxtProgressBar(pb, i)
        }
    }
    if (gtools::invalid(customXpath) == FALSE) {
        for (i in seq_along(articlesHtml)) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
                tempStringXml <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
                if (length(tempStringXml) == 0) {
                    txt[i] <- NA
                    print(paste("String in article with ID", i, "could not be extracted."))
                } else {
                    txt[i] <- tempStringXml
                }
            }
            setTxtProgressBar(pb, i)
        }
    }
    close(pb)
    return(txt)
}
