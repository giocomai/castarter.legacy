#' Extracts dates from a vector of html files
#'
#' Extracts dates from a vector of html files.
#'
#' @param containerInstance Defaults to NULL. If given, it must be an integer. If a given element is found more than once in the same page, it keeps only the relevant occurrence for further extraction.
#' @param inputVector Defaults to NULL. If provided, instead of looking for downloaded Html files it parses the given character vector.
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
#' @param encoding Defaults to 'UTF-8'. If source is not in UTF, encoding can be specified here. A list of valid values can be found using iconvlist().
#' @param keepAllString Logical, defaults to FALSE. If TRUE, it directly tries to parse the date with the given dateFormat, without trying to polish the string provided accordingly.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown.
#' @param importParameters Defaults to NULL. If TRUE, ignores all parameters given in the function call, and imports them from parameters file stored in "project/website/Logs/parameters.rds".
#' @param exportParameters Defaults to TRUE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus. Requires parameters project/website.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return A vector of the Date class.
#' @export
#' @examples
#' \dontrun{
#' dates <- ExtractDates()
#' }
ExtractDates <- function(dateFormat = "dmY",
                         container = NULL,
                         containerClass = NULL,
                         containerId = NULL,
                         containerInstance = NULL,
                         htmlLocation = NULL,
                         inputVector = NULL,
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
                params$ExtractDates$exportParameters <- FALSE
                if (is.null(id)==FALSE) {
                    params$ExtractDates$id <- id
                }
                for (i in seq_along(params$ExtractDates)) {
                    assign(names(params$ExtractDates)[i], params$ExtractDates[[i]])
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
        ExtractDatesParams <-  as.list(environment())
        if (file.exists(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))) == TRUE) {
            params <- readRDS(base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
            params$ExtractDates <- NULL
        } else {
            params <- list()
        }
        params$ExtractDates <- ExtractDatesParams
        saveRDS(object = params, file = base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-")))
    }

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
    datesTxt <- vector(mode = "character", length = length(HtmlFiles))
    if (progressBar == TRUE) {
        pb <- dplyr::progress_estimated(n = length(HtmlFiles), min_time = 1)
    }
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
                    rvest::html_nodes(xpath = customXpath)
                if (is.null(attribute)) {
                    temp <-  temp %>%
                        rvest::html_text()
                } else {
                    temp <-  temp %>%
                        rvest::html_attr(attribute) %>%
                        rvest::html_text()
                }
            } else if (is.null(container)==FALSE&is.null(containerClass)==FALSE&is.null(attribute)==FALSE) {
                temp <- tryCatch(expr = temp %>%
                             rvest::html_nodes(container) %>%
                             rvest::html_nodes(xpath = paste0("//", container, "[@class='", containerClass, "']")) %>%
                             rvest::html_attr(attribute),
                         error = function(e) {
                             warning(paste("Could not find attribute in", HtmlFiles[i]))
                             NA
                         })
            } else if (is.null(container)==FALSE&is.null(containerClass)==TRUE&is.null(attribute)==FALSE) {
                temp <- tryCatch(expr = temp %>%
                             rvest::html_nodes(container) %>%
                             rvest::html_attr(attribute),
                         error = function(e) {
                             warning(paste("Could not find attribute in", HtmlFiles[i]))
                             NA
                         })
            } else if (is.null(containerClass)==TRUE&is.null(containerId)==TRUE) {
                temp <- as.character(temp)
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
            if (is.null(removeEverythingBefore) == FALSE) {
                temp <- stringr::str_replace(string = temp, pattern = stringr::regex(paste0(".*", removeEverythingBefore)), replacement = "")
            }
            if (length(temp)>1) {

                if (is.null(containerInstance)==FALSE) {
                    datesTxt[i] <- temp[containerInstance]
                } else {
                    datesTxt[i] <- temp[1]
                    warning(paste0("ID",
                                   stringr::str_extract(string = HtmlFiles[i], pattern = "[[:digit:]]+[[:punct:]]html") %>%
                                       stringr::str_sub(start = 1L, end = -6L), ": Found more than one string per page, keeping only first occurrence. You can specify which occurrence to keep with the param `containerInstance`.\n"))
                }
            } else if (length(temp)==0) {
                datesTxt[i] <- NA
            } else {
                datesTxt[i] <- temp
            }
        }
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
        monthsRu <- tolower(c("Января", "Февраля", "Марта", "Апреля", "Мая", "Июня", "Июля", "Августа", "Сентября",
                      "Октября", "Ноября", "Декабря"))
        monthsRu_2 <- c("январь", "февраль", "март", "апрель", "май", "июнь", "июль", "август",
                        "сентябрь", "октябрь", "ноябрь", "декабрь")
        monthsRu_3 <- c("янв", "фев", "мар", "апр", "май", "июн", "июл", "авг",
                        "сен", "окт", "ноя", "дек")
        monthsLocale <- month.name
        datesTxt <- tolower(datesTxt)
        for (i in 1:12) {
            datesTxt <- gsub(monthsRu[i], monthsLocale[i], datesTxt)
        }
        for (i in 1:12) {
            datesTxt <- gsub(monthsRu_2[i], monthsLocale[i], datesTxt)
        }
        for (i in 1:12) {
            datesTxt <- gsub(monthsRu_3[i], monthsLocale[i], datesTxt)
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
#' \dontrun{
#' dates <- MergeDates(dates1, dates2)
#' }
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
