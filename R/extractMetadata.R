#' Extracts dates from a vector of html files
#' 
#' Extracts dates from a vector of html files.
#'  
#' @param articlesHtml A character vector of html files.
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
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param language Provide a language in order to extract name of months. Defaults to the locale currently active in R (usually, the system language). Generic forms such as "english" or "russian", are usually accepted. See ?locales for more details. On linux, you can run system("locale -a", intern = TRUE) to see all available locales.
#' @param encoding Defaults to NULL. If source is not in UTF, encoding can be specified here for conversion. A list of valid values can be found using iconvlist().
#' @param keepAllString Logical, defaults to FALSE. If TRUE, it directly tries to parse the date with the given dateFormat, without trying to polish the string provided accordingly.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' dates <- ExtractDates(articlesHtml)
ExtractDates <- function(articlesHtml, dateFormat = "dmY", divClass = NULL, divId = NULL, spanClass = NULL, customXpath = NULL, language = Sys.getlocale(category = "LC_TIME"), customString = "", minDate = NULL, maxDate = NULL, encoding = "UTF-8", keepAllString = FALSE, removeEverythingBefore = NULL, exportParameters = TRUE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (exportParameters == TRUE && exists("project") == FALSE | exportParameters == TRUE && exists("website") == FALSE) {
        stop("If exportParameters == TRUE, both project and website must be defined either as parameters or previously with .")    
    }
    if (exportParameters == TRUE) {
        args <- c("dateFormat_ExtractDates", "divClass_ExtractDates", "divId_ExtractDates", "spanClass_ExtractDates", "customXpath_ExtractDates", "customString_ExtractDates", "minDate", "maxDate", "keepAllString_ExtractDates", "removeEverythingBefore_ExtractDates")
        param <- list(dateFormat, divClass, divId, spanClass, customXpath, customString, minDate, maxDate, keepAllString, removeEverythingBefore)
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
    numberOfArticles <- length(articlesHtml)
    if (gtools::invalid(encoding) == FALSE) {
        articlesHtml <- iconv(articlesHtml, from = encoding, to = "UTF-8")
    } else {
        articlesHtml <- iconv(articlesHtml, to = "UTF-8")
    }
    if (gtools::invalid(divId) == FALSE) {
        datesTxt <- rep(NA, numberOfArticles)
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divId, "']"), XML::xmlValue)) == 0) {
                    datesTxt[i] <- NA
                    print(paste("Date in article with ID", i, "could not be extracted."))
                } else {
                    datesTxt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divId, "']"), XML::xmlValue)
                }
            }
        }
    }
    if (gtools::invalid(divClass) == FALSE) {
        datesTxt <- rep(NA, numberOfArticles)
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)) == 0) {
                  datesTxt[i] <- NA
                  print(paste("Date in article with ID", i, "could not be extracted."))
                } else {
                  datesTxt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)
                }
            }
        }
    }
    if (gtools::invalid(spanClass)==FALSE) {
        datesTxt <- rep(NA, numberOfArticles)
        for (i in 1:numberOfArticles) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
            tempStringXml <- XML::xpathSApply(articleHtmlParsed, paste0("//span[@class='", spanClass, "']"), XML::xmlValue)
            if (length(tempStringXml) == 0) {
                datesTxt[i] <- NA
                print(paste("Date in article with ID", i, "could not be extracted."))
            } else {
                datesTxt[i] <- tempStringXml
            }
        }
    }
    if (gtools::invalid(customXpath) == FALSE) {
        datesTxt <- rep(NA, numberOfArticles)
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
                tempStringXml <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
                if (length(tempStringXml) == 0) {
                    datesTxt[i] <- NA
                    print(paste("Date in article with ID", i, "could not be extracted."))
                } else {
                    datesTxt[i] <- tempStringXml
                }
            }
        }
    }
    if (exists("datesTxt") == TRUE) {
        if (length(datesTxt) == 1 & is.na(datesTxt[1]) == TRUE) {
        } else {
            articlesHtml <- datesTxt
        }
    }
    numberOfArticles <- length(articlesHtml)
    datesTxt <- rep(NA, numberOfArticles)
    if (is.null(removeEverythingBefore) == FALSE) {
        articlesHtml <- base::gsub(base::paste0(".*", removeEverythingBefore), "", articlesHtml, fixed = FALSE)
    }
    if (keepAllString == TRUE) {
        datesTxt <- articlesHtml
    } else {
        if (dateFormat == "dby" | dateFormat == "dBy" | dateFormat == "dBY" | dateFormat == "dbY") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]]?[[:space:]][[:alpha:]]*[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]", 
                                                               articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "YBd" | dateFormat == "ybd") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]]?[[:digit:]][[:digit:]][[:space:]]?[[:punct:]]?[[:alpha:]]*[[:space:]]?[[:punct:]]?[[:digit:]][[:digit:]]?", articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dB,Y") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*,[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?", articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "db.'y") {
            datesTxt <- rep(NA, numberOfArticles)
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*.'[[:digit:]][[:digit:]][[:digit:]]?", articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "Bd,Y" | dateFormat == "bd,Y") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:alpha:]]*[[:space:]][[:digit:]]?[[:digit:]],[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?", 
                                                               articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "xdBY") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr(paste0(customString, "[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?"), 
                                                               articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
            dateFormat <- "dBY"
        } else if (dateFormat == "ymd" | dateFormat == "Ymd") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:punct:]]?[[:digit:]]?[[:digit:]][[:punct:]]?[[:digit:]]?[[:digit:]]", articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dmy" | dateFormat == "mdy") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]][[:digit:]]", 
                                                               articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "dmY" | dateFormat == "mdY") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]", articlesHtml[i]))
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
        dates <- lubridate::parse_date_time(datesTxt, dateFormat, locale = "en_GB.UTF-8")
    } else {
        dates <- lubridate::parse_date_time(datesTxt, dateFormat, locale = language)
    }
    if (is.null(minDate) == FALSE) {
        dates[dates < as.POSIXct(minDate)] <- NA
    }
    if (is.null(maxDate) == FALSE) {
        dates[dates > as.POSIXct(maxDate)] <- NA
    }
    return(dates)
}

#' Merge alternative sets of dates for a given dataset
#' 
#' Merge alternative sets of dates for a given dataset.
#'  
#' @param dates1, dates2, dates3 Vectors of the POSIXct class to be merged. Where a value in dates1 is NA, MergeDates looks for a corresponding value in dates2 and dates3.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param fillMissingDates Logical, defaults to FALSE. If TRUE fills missing NA dates with interpolated values. It is sensible to use this if dates are in a sequence, and if the corresponding articlesTxt includes actual contents (i.e. missing date is not effectively due to a missing article or a link included in the dataset by mistake)
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
#' @param articlesHtml A character vector of html files.
#' @param articlesLinks A named character vector, typically created by the ExtractArticlesLinks function.
#' @param removeString A character vector of one or more strings to be removed from the extracted title.
#' @param method Title extract method, to be given as a text string. Available options are:
##' \itemize{
##'  \item{"indexLink"}{: Default. Extract the title from articlesLinks (required). Titles are taken from the textual element of the link taken from the index pages. }
##'  \item{"htmlTitle"}{: Extract the title from the Html <title> field, usually shown on the top bar of web browsers.}
##'  \item{"htmlH1"}{: Extract the title from the first occurence of text that has heading 1, the <h1> html tag, as its style.}
##'  \item{"htmlH2"}{: Extract the title from the first occurence of text that has heading 2, the <h2> html tag, as its style.}
##'  \item{"customXpath"}{: Allows to input a custom Xpath to extract the title.}
##'  \item{"beginning"}{: Outputs as title the first textual elements found in the html file. Title length can be defined with the 'maxCharacters' option.}
##' }
#' @param removeEverythingAfter Removes everything after given string. 
#' @param maxCharacters An integer. Defines the maximum number of characters to be kept in the output for each title. 
#' @return A character vector of article titles.
#' @export
#' @examples
#' titles <- ExtractTitles(articlesHtml)
ExtractTitles <- function(articlesHtml = NULL, articlesLinks = NULL, method = "htmlTitle", removePunctuation = FALSE, onlyStandardCharacters = FALSE, removeString = NULL, removeEverythingBefore = NULL, removeEverythingAfter = NULL, customXpath = "", maxCharacters = NULL, progressBar = TRUE, exportParameters = TRUE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (exportParameters == TRUE) {
        args <- c("method_ExtractTitles", "removePunctuation_ExtractTitles", "onlyStandardCharacters_ExtractTitles", "removeString_ExtractTitles", "removeEverythingBefore_ExtractTitles", "removeEverythingAfter_ExtractTitles", "customXpath_ExtractTitles", "maxCharacters_ExtractTitles")
        param <- list(method, removePunctuation, onlyStandardCharacters, paste0(removeString, collapse = "§§§"), removeEverythingBefore, removeEverythingAfter, customXpath, maxCharacters)
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
    titles <- vector()
    if (gtools::invalid(articlesHtml)==TRUE) {
        numberOfArticles <- length(articlesLinks)
    } else {
        numberOfArticles <- length(articlesHtml)
    }
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = numberOfArticles, style = 3, title = "Extracting titles")
    }
    if (method == "htmlTitle") {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i]!="") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                titles[i] <- XML::xpathSApply(articleHtmlParsed, "//title", XML::xmlValue)
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    } else if (method == "htmlH2") {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i]!="") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                titles[i] <- XML::xpathSApply(articleHtmlParsed, "//h2", XML::xmlValue)
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    } else if (method == "htmlH1") {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i]!="") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                titles[i] <- XML::xpathSApply(articleHtmlParsed, "//h1", XML::xmlValue)
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    } else if (method == "customXpath") {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i]!="") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                titles[i] <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
            }
            if (progressBar == TRUE) {
                setTxtProgressBar(pb, i)
            }
        }
    } else if (method == "indexLink") {
        titles <- names(articlesLinks)
    } else if (method == "beginning") {
        titles <- ExtractTxt(articlesHtml, export = FALSE, keepEverything = TRUE)
    }
    if (gtools::invalid(removeString) == FALSE) {
        titles <- gsub(removeString, replacement = "", x = titles, fixed = TRUE)
    }
    if (gtools::invalid(removeEverythingAfter) == FALSE) {
        titles <- gsub(paste0(removeEverythingAfter, ".*"), replacement = "", x = titles)
    }
    if (gtools::invalid(removeEverythingBefore) == FALSE) {
        titles <- gsub(paste0(removeEverythingBefore, "*."), replacement = "", x = titles)
    }
    if (gtools::invalid(onlyStandardCharacters) == FALSE) {
        if (onlyStandardCharacters == TRUE) {
            titles <- gsub("[^A-Za-z0-9 ]", "-", titles)
            titles <- gsub("  ", " ", titles)
            titles <- gsub("--", "-", titles)
        }
    }
    if (gtools::invalid(removePunctuation == FALSE)) {
        if (removePunctuation == TRUE) {
            titles <- gsub("[[:punct:]]", "-", titles)
            titles <- gsub("  ", " ", titles)
            titles <- gsub("--", "-", titles)
        }
    }
    if (gtools::invalid(maxCharacters) == FALSE) {
        titles <- substring(titles, 1, maxCharacters)
    }
    if (progressBar == TRUE) {
        close(pb)
    }
    return(titles)
}

#' Extracts articlesId from filename
#' 
#' Extracts articlesId from filename
#'  
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no website is provided, exported files are saved in the project/Outputs folder.
#' @return A vector of the integer class. 
#' @export
#' @examples
#' articlesId <- ExtractId(project, website)
ExtractId <- function(project = NULL, website = NULL, accordingToDate = FALSE, dates = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    htmlFilesList <- gtools::mixedsort(list.files(file.path(project, website, "Html")))
    if (accordingToDate == TRUE) {
        htmlFilesList <- htmlFilesList[order(dates)]
    }
    articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
    articlesId
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
CreateDatasetFromHtml <- function(articlesLinks = NULL,
                                  dateFormat = NULL, divClass_ExtractDates = NULL, spanClass_ExtractDates = NULL, customXpath_ExtractDates = NULL, language_ExtractDates = Sys.getlocale(category = "LC_TIME"), removeEverythingBefore_ExtractDates = NULL,
                                  method_ExtractTitles = "htmlTitle", removeString_ExtractTitles = NULL, removeEverythingAfter_ExtractTitles = NULL, removePunctuation_ExtractTitles = NULL,
                                  divClass_ExtractTxt = NULL, divId_ExtractTxt = NULL, removeString_ExtractTxt = NULL, removeEverythingAfter_ExtractTxt = NULL, removeEverythingBefore_ExtractTxt = NULL, removeTitleFromTxt = FALSE, customXpath_ExtractTxt = NULL,
                                  language = "English", encoding = NULL,
                                  logProgress = FALSE,
                                  exportParameters = TRUE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
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
    dates <- as.POSIXct(rep(NA, numberOfArticles))
    articlesTxt <- rep(NA, numberOfArticles)
    if (method_ExtractTitles == "indexLink") {
        titles <- ExtractTitles(articlesHtml = NULL, articlesLinks = articlesLinks, method = "indexLink", removeString = removeString_ExtractTitles, removeEverythingAfter = removeEverythingAfter_ExtractTitles, exportParameters = FALSE)
    } else {
        titles <- rep(NA, numberOfArticles)
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
            dates[i] <- dateTemp
        } else {
            dates[i] <- NA
        }
        articlesTxt[i] <- ExtractTxt(articlesHtml = htmlFile, export = FALSE, removeEverythingAfter = removeEverythingAfter_ExtractTxt, removeEverythingBefore = removeEverythingBefore_ExtractTxt, divClass = divClass_ExtractTxt, divId = divId_ExtractTxt, removeString = removeString_ExtractTxt, customXpath = customXpath_ExtractTxt, progressBar = FALSE, exportParameters = FALSE)
        if (method_ExtractTitles != "indexLink") {
            titleTemp <- ExtractTitles(articlesHtml = htmlFile, method = method_ExtractTitles, removeString = removeString_ExtractTitles, progressBar = FALSE)
            if (length(titleTemp) == 1) {
                titles[i] <- titleTemp
            } else {
                titles[i] <- NA
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
    articlesId <- ExtractId(project, website)
    if (is.null(articlesLinks) == FALSE) {
        dataset <- data.frame(project, website, dates, articlesId, titles, language, articlesLinks, articlesTxt, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
        dataset <- data.frame(project, website, dates, articlesId, titles, language, articlesTxt, check.names = FALSE, stringsAsFactors = FALSE)        
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
#' metadata <- ExportMetadata(project, website, dates, articlesId, titles, language, articlesLinks)
ExportMetadata <- function(dates, articlesId, titles, language, articlesLinks, exportXlsx = FALSE, accordingToDate = FALSE, ignoreNAdates = FALSE, onlyExistingHtmlFiles = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    ignoreVector <- NULL
    if (onlyExistingHtmlFiles == TRUE) {
        htmlFilesList <- gtools::mixedsort(list.files(file.path(project, website, "Html"), full.names = TRUE))
        articlesId <- as.integer(regmatches(htmlFilesList, regexpr("[[:digit:]]+", htmlFilesList)))
        articlesIdInTheory <- 1:length(articlesLinks)
        ignoreVector <- !is.element(articlesId, articlesIdInTheory)
    }
    if (ignoreNAdates == TRUE) {
        ignoreVector <- is.na(dates)
    }
    if (is.null(ignoreVector) == FALSE) {
        articlesLinks <- articlesLinks[ignoreVector]
    }
    metadata <- data.frame(project, website, dates, articlesId, titles, language, articlesLinks, check.names = FALSE, stringsAsFactors = FALSE)
    if (accordingToDate == TRUE) {
        metadata <- metadata[order(metadata$dates), ]
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
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportCsv If equal to TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return A data.frame, a 'castarter' dataset.
#' @export
#' @examples
#' dataset <- ExportDataset(articlesTxt, metadata, project, website)
ExportDataset <- function(articlesTxt, metadata, exportRdata = TRUE, exportCsv = FALSE, exportXlsx = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        project <- CastarterOptions("website")
    }
    dataset <- cbind(metadata, articlesTxt, stringsAsFactors = FALSE)
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
#' string <- ExtractDiv(articlesHtml, divClass = "nameOfDiv")
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