#' Extracts dates from a vector of html files
#' 
#' Extracts dates from a vector of html files.
#'  
#' @param articlesHtml A character vector of html files.
#' @param dateFormat A string used to extract the date. Available date formats options include dmY, dby, dBy, dBY, dbY, etc.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param keepAllString Logical, defaults to FALSE. If TRUE, it directly tries to parse the date with the given dateFormat, without trying to polish the string provided accordingly.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Required for storing export parameters (with exportParameters = TRUE).
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' dates <- ExtractDates(articlesHtml)
ExtractDates <- function(articlesHtml, dateFormat = "dmY", language = "english", customString = "", minDate = NULL, maxDate = NULL, removeEverythingBefore = NULL, keepAllString = FALSE, exportParameters = TRUE, nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite) {
    if (exportParameters == TRUE && exists("nameOfProject") == FALSE | exportParameters == TRUE && exists("nameOfWebsite") == FALSE) {
    stop("If exportParameters == TRUE, both nameOfProject and nameOfWebsite must be defined.")    
    }
    articlesHtml <- iconv(articlesHtml, to = "utf8")
    originalLocale <- base::Sys.getlocale(category = "LC_TIME")
    if (language == "en" | language == "english") {
        base::Sys.setlocale(category = "LC_TIME", locale = "en_GB.UTF-8")
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
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*,[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?", 
                                                               articlesHtml[i]))
                if (length(dateTxt) == 0) {
                    datesTxt[i] <- NA
                } else {
                    datesTxt[i] <- dateTxt
                }
            }
        } else if (dateFormat == "db.'y") {
            for (i in 1:numberOfArticles) {
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:space:]][[:alpha:]]*.'[[:digit:]][[:digit:]][[:digit:]]?", 
                                                               articlesHtml[i]))
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
                dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]", 
                                                               articlesHtml[i]))
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
        Sys.setlocale(category = "LC_TIME", locale = "en_GB.UTF-8")
    }
    dates <- lubridate::parse_date_time(datesTxt, dateFormat)
    if (is.null(minDate) == FALSE) {
    dates[dates < as.POSIXct(minDate)] <- NA
    }
    if (is.null(maxDate) == FALSE) {
        dates[dates > as.POSIXct(maxDate)] <- NA
    }
    if (exportParameters == TRUE) {
        args <- c("dateFormat", "customStringExtractDates", "minDate", "maxDate", "keepAllString", "removeEverythingBeforeExtractDates")
        param <- list(dateFormat, customString, minDate, maxDate, keepAllString, removeEverythingBefore)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    base::Sys.setlocale(category = "LC_TIME", locale = originalLocale)
    dates
}
#' Extracts dates from a vector of html files
#' 
#' Extracts dates from a vector of html files.
#'  
#' @param articlesHtml A character vector of html files.
#' @param dateFormat A string used to extract the date. Available date formats options include dmY, dby, dBy, dBY, dbY, etc.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @param encoding Defaults to NULL. If source is not in UTF, encoding can be specified here for conversion. A list of valid values can be found using iconvlist().
#' @param keepAllString Logical, defaults to FALSE. If TRUE, it directly tries to parse the date with the given dateFormat, without trying to polish the string provided accordingly.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Required for storing export parameters (with exportParameters = TRUE).
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' dates <- ExtractDatesXpath(articlesHtml)
ExtractDatesXpath <- function(articlesHtml, dateFormat = "dmy", divClass = NULL, spanClass = NULL, customXpath = "", language = "english", customString = "", minDate = NULL, maxDate = NULL, encoding = NULL, keepAllString = FALSE, exportParameters = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (exportParameters == TRUE) {
        args <- c("dateFormat", "divClassDatesXpath", "spanClassDatesXpath", "customXpathDates", "customStringDates", "minDate", "maxDate", "keepAllString")
        param <- list(dateFormat, divClass, spanClass, customXpath, customString, minDate, maxDate, keepAllString)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    numberOfArticles <- length(articlesHtml)
    datesTxt <- rep(NA, numberOfArticles)
    if (is.null(encoding) == FALSE) {
        articlesHtml <- iconv(articlesHtml, from = encoding, to = "utf8")
    }
    if (is.null(divClass) == FALSE) {
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
    if (is.null(spanClass)==FALSE) {
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
    if (customXpath != "") {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
                datesTxt[i] <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
            }
        }
    }
    if (length(datesTxt) == 1 & is.na(datesTxt[1]) == TRUE) {
    } else {
    ExtractDates(articlesHtml = datesTxt, dateFormat = dateFormat, language = language, keepAllString = TRUE, nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite)
    }
}

#' Merge alternative sets of dates for a given dataset
#' 
#' Merge alternative sets of dates for a given dataset.
#'  
#' @param dates1, dates2, dates3 Vectors of the POSIXct class to be merged. Where a value in dates1 is NA, MergeDates looks for a corresponding value in dates2 and dates3.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @return A vector of the POSIXct class. . 
#' @export
#' @examples
#' dates <- MergeDates(dates1, dates2)
MergeDates <- function(dates1, dates2, dates3 = "", minDate = "", maxDate = "") {
    dates <- dates1
    for (i in 1:length(dates)) {
        if (is.na(dates[i])) {
            dates[i] <- dates2[i]
        }
        if (is.na(dates[i])) {
            dates[i] <- dates3[i]
        }
        if (is.null(minDate) == FALSE & is.null(maxDate) == FALSE) {
            if (is.na(dates[i]) == FALSE & dates[i] < minDate | is.na(dates[i]) == FALSE & dates[i] > maxDate) {
                dates[i] <- NA
            }
        }
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
#' @param titlesExtractMethod Title extract method, to be given as a text string. Available options are:
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
ExtractTitles <- function(articlesHtml = NULL, articlesLinks = "", titlesExtractMethod = "indexLink", removePunctuation = FALSE, onlyStandardCharacters = FALSE, removeString = NULL, removeEverythingAfter = NULL, customXpath = "", maxCharacters = NULL, exportParameters = TRUE, nameOfProject = NULL, nameOfWebsite = NULL) {
    titles <- vector()
    numberOfArticles <- length(articlesHtml)
    if (titlesExtractMethod == "htmlTitle") {
        for (i in 1:numberOfArticles) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
            titles[i] <- XML::xpathSApply(articleHtmlParsed, "//title", XML::xmlValue)
        }
    } else if (titlesExtractMethod == "htmlH2") {
        for (i in 1:numberOfArticles) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
            titles[i] <- XML::xpathSApply(articleHtmlParsed, "//h2", XML::xmlValue)
        }
    } else if (titlesExtractMethod == "htmlH1") {
        for (i in 1:numberOfArticles) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
            titles[i] <- XML::xpathSApply(articleHtmlParsed, "//h1", XML::xmlValue)
        }
    } else if (titlesExtractMethod == "customXpath") {
        for (i in 1:numberOfArticles) {
        articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
        titles[i] <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
        }
    } else if (titlesExtractMethod == "indexLink") {
        titles <- names(articlesLinks)
    } else if (titlesExtractMethod == "beginning") {
        titles <- ExtractTxt(articlesHtml, export = FALSE, keepEverything = TRUE)
    }
    if (is.null(removeString) == FALSE) {
        titles <- gsub(removeString, replacement = "", x = titles, fixed = TRUE)
    }
    if (is.null(removeEverythingAfter) == FALSE) {
        titles <- gsub(paste0(removeEverythingAfter, ".*"), replacement = "", x = titles)
    }
    if (onlyStandardCharacters == TRUE) {
        titles <- gsub("[^A-Za-z0-9 ]", "-", titles)
        titles <- gsub("  ", " ", titles)
        titles <- gsub("--", "-", titles)
    }
    if (removePunctuation == TRUE) {
        titles <- gsub("[[:punct:]]", "-", titles)
        titles <- gsub("  ", " ", titles)
        titles <- gsub("--", "-", titles)
    }
    if (is.null(maxCharacters) == FALSE) {
    titles <- substring(titles, 1, maxCharacters)
    }
    if (exportParameters == TRUE) {
        args <- c("titlesExtractMethod", "removePunctuationExtractTitles", "onlyStandardCharactersExtractTitles", "removeStringExtractTitles", "removeEverythingAfterExtractTitles", "customXpathExtractTitles", "maxCharactersExtractTitles")
        param <- list(titlesExtractMethod, removePunctuation, onlyStandardCharacters, removeString, removeEverythingAfter, customXpath, maxCharacters)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    titles
}

#' Extracts articlesId from filename
#' 
#' Extracts articlesId from filename
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no nameOfWebsite is provided, exported files are saved in the nameOfProject/Outputs folder.
#' @return A vector of the integer class. 
#' @export
#' @examples
#' articlesId <- ExtractArticleId(nameOfProject, nameOfWebsite)
ExtractArticleId <- function(nameOfProject, nameOfWebsite, accordingToDate = FALSE, dates = NULL) {
    htmlFilesList <- gtools::mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "Html")))
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
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no nameOfWebsite is provided, exported files are saved in the nameOfProject/Outputs folder.
#' @return A data.frame with the complete dataset. 
#' @export
#' @examples
#' dataset <- CreateDatasetFromHtml(nameOfProject, nameOfWebsite)
CreateDatasetFromHtml <- function(nameOfProject, nameOfWebsite, articlesLinks = NULL, dateFormat = "dBY", ExtractDatesXpath = FALSE, titlesExtractMethod = "htmlTitle", divClass = NULL, spanClass = NULL, language = NULL, removeString = NULL, encoding = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL, removeEverythingBeforeDate = NULL) {
    htmlFilesList <- gtools::mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "Html"), pattern = "\\.html$", full.names = TRUE))
    numberOfArticles <- length(htmlFilesList)
    dates <- as.POSIXct(rep(NA, numberOfArticles))
    articlesTxt <- rep(NA, numberOfArticles)
    if (titlesExtractMethod == "indexLink") {
        titles <- ExtractTitles(articlesHtml = NULL, articlesLinks = articlesLinks, titlesExtractMethod = "indexLink", removeString = removeString)
    } else {
        titles <- rep(NA, numberOfArticles)
    }
    x <- 1
    xlist <- seq(0,numberOfArticles,by=100)
    for (i in 1:numberOfArticles) {
        htmlFile <- readLines(htmlFilesList[i])
        htmlFile <- paste(htmlFile, collapse = "\n")
        if (is.null(encoding) == FALSE) {
            htmlFile <- iconv(htmlFile, from = encoding, to = "utf8")
        }
        if (is.null(removeEverythingBeforeDate)==TRUE) {
            dates[i] <- ExtractDates(htmlFile, dateFormat = dateFormat, language = language)
        } else {
            dates[i] <- ExtractDates(htmlFile, dateFormat = dateFormat, language = language, removeEverythingBefore = removeEverythingBeforeDate)
        }
        if (ExtractDatesXpath == TRUE) {
            dateTemp <- ExtractDatesXpath(articlesHtml = htmlFile, dateFormat = dateFormat, divClass = divClass, spanClass = spanClass, language = language)
            if (length(dateTemp) == 1) {
                dates[i] <- dateTemp
            } else {
                dates[i] <- NA
            }
        }
        articlesTxt[i] <- ExtractTxt(articlesHtml = htmlFile, export = FALSE, removeEverythingAfter = removeEverythingAfter, removeEverythingBefore = removeEverythingBefore)
        if (titlesExtractMethod != "indexLink") {
            titleTemp <- ExtractTitles(articlesHtml = htmlFile, titlesExtractMethod = titlesExtractMethod, removeString = removeString)
            if (length(titleTemp) == 1) {
                titles[i] <- titleTemp
            } else {
                titles[i] <- NA
            }
        }
        x <- x + 1
        if (is.element(x, xlist)) {
            print(paste("Processed", x, "of", numberOfArticles, "articles."))
        }
    }
    articlesId <- ExtractArticleId(nameOfProject, nameOfWebsite)
    if (is.null(articlesLinks) == FALSE) {
        dataset <- data.frame(nameOfProject, nameOfWebsite, dates, articlesId, titles, language, articlesLinks, articlesTxt, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
        dataset <- data.frame(nameOfProject, nameOfWebsite, dates, articlesId, titles, language, articlesTxt, check.names = FALSE, stringsAsFactors = FALSE)        
    }
    save(dataset, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData")))
    print(paste("Dataset saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData"))))
    dataset
}

#' Exports metadata
#' 
#' Exports metadata to a csv file.
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' metadata <- ExportMetadata(nameOfProject, nameOfWebsite, dates, articlesId, titles, language, articlesLinks)
ExportMetadata <- function(nameOfProject, nameOfWebsite, dates, articlesId, titles, language, articlesLinks, exportXlsx = FALSE, accordingToDate = FALSE, ignoreNAdates = FALSE, onlyExistingHtmlFiles = FALSE) {
    ignoreVector <- NULL
    if (onlyExistingHtmlFiles == TRUE) {
        htmlFilesList <- gtools::mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "Html"), full.names = TRUE))
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
    metadata <- data.frame(nameOfProject, nameOfWebsite, dates, articlesId, titles, language, articlesLinks, check.names = FALSE, stringsAsFactors = FALSE)
    if (accordingToDate == TRUE) {
        metadata <- metadata[order(metadata$dates), ]
    }
    write.csv(metadata, file = file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "metadata.csv")), row.names = FALSE)
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(metadata, file = file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "metadata.xlsx")), row.names = FALSE)
    }
    metadata
} 

