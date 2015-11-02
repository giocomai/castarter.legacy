#' Extracts dates from a vector of html files
#' 
#' Extracts dates from a vector of html files.
#'  
#' @param articlesHtml A character vector of html files.
#' @param dateFormat A string used to extract the date. Available date formats options include dmY, dby, dBy, dBY, dbY, etc.
#' @param minDate, maxDate Minimum and maximum possible dates in the format year-month-date, e.g. "2007-06-24". Introduces NA in the place of impossibly high or low dates.
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' dates <- ExtractDates(articlesHtml)
ExtractDates <- function(articlesHtml, dateFormat = "dmY", language = "en", customString = "", minDate = NULL, maxDate = NULL, removeEverythingBefore = NULL) {
    originalLocale <- base::Sys.getlocale(category = "LC_TIME")
    if (language == "en") {
        base::Sys.setlocale(category = "LC_TIME", locale = "en_GB.UTF-8")
    }
    numberOfArticles <- length(articlesHtml)
    datesTxt <- rep(NA, numberOfArticles)
    if (is.null(removeEverythingBefore) == FALSE) {
        articlesHtml <- base::gsub(base::paste0(".*", removeEverythingBefore), "", articlesHtml, fixed = FALSE)
    }
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
    } else if (dateFormat == "Bd,Y") {
        for (i in 1:numberOfArticles) {
            dateTxt <- regmatches(articlesHtml[i], regexpr("[[:space:]][[:alpha:]]*[[:space:]][[:digit:]]?[[:digit:]],[[:space:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]?", 
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
            dateTxt <- regmatches(articlesHtml[i], regexpr("[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:punct:]][[:digit:]]?[[:digit:]][[:punct:]][[:digit:]]?[[:digit:]]", articlesHtml[i]))
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
    if (language == "ru") {
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
#' @return A vector of the POSIXct class. 
#' @export
#' @examples
#' dates <- ExtractDatesXpath(articlesHtml)
ExtractDatesXpath <- function(articlesHtml, dateFormat = "dmy", divClass = NULL, spanClass = "", customXpath = "", language = "en", customString = "", minDate = NULL, maxDate = NULL, encoding = NULL) {
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
    if (spanClass != "") {
        for (i in 1:numberOfArticles) {
            articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T)
            datesTxt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//span[@class='", spanClass, "']"), XML::xmlValue)
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
    ExtractDates(datesTxt, dateFormat, language)
    }
}

MergeDates <- function(dates1, dates2, dates3 = "", firstPossibleDate = "", lastPossibleDate = "") {
    dates <- dates1
    for (i in 1:length(dates)) {
        if (is.na(dates[i])) {
            dates[i] <- dates2[i]
        }
        if (is.na(dates[i])) {
            dates[i] <- dates3[i]
        }
        if (firstPossibleDate != "" & lastPossibleDate != "") {
            if (is.na(dates[i]) == FALSE & dates[i] < firstPossibleDate | is.na(dates[i]) == FALSE & dates[i] > lastPossibleDate) {
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
ExtractTitles <- function(articlesHtml = NULL, articlesLinks = "", titlesExtractMethod = "indexLink", removePunctuation = FALSE, onlyStandardCharacters = FALSE, removeString = "", removeEverythingAfter = NULL, customXpath = "", maxCharacters = NULL) {
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
    if (removeString[1] != "") {
        titles <- gsub(paste(removeString, collapse = "|"), replacement = "", x = titles)
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
CreateDatasetFromHtml <- function(nameOfProject, nameOfWebsite, articlesLinks = NULL, dateFormat = "dBY", ExtractDatesXpath = FALSE, titlesExtractMethod = "htmlTitle", divClass = NULL, language = NULL, removeString = NULL, encoding = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL) {
    htmlFilesList <- gtools::mixedsort(list.files(file.path(nameOfProject, nameOfWebsite, "Html"), pattern = "\\.html$", full.names = TRUE))
    numberOfArticles <- length(htmlFilesList)
    dates <- as.POSIXct(rep(NA, numberOfArticles))
    articlesTxt <- rep(NA, numberOfArticles)
    titles <- rep(NA, numberOfArticles)
    x <- 1
    xlist <- seq(0,numberOfArticles,by=100)
    for (i in 1:numberOfArticles) {
        htmlFile <- readLines(htmlFilesList[i])
        htmlFile <- paste(htmlFile, collapse = "\n")
        if (is.null(encoding) == FALSE) {
            htmlFile <- iconv(htmlFile, from = encoding, to = "utf8")
        }
        dates[i] <- ExtractDates(htmlFile, dateFormat = dateFormat, language = language)
        if (ExtractDatesXpath == TRUE) {
        dates[i] <- ExtractDatesXpath(articlesHtml = htmlFile, dateFormat = dateFormat, divClass = divClass, language = language)
        }
        articlesTxt[i] <- ExtractTxt(articlesHtml = htmlFile, export = FALSE, removeEverythingAfter = removeEverythingAfter, removeEverythingBefore = removeEverythingBefore)
        titles[i] <- ExtractTitles(articlesHtml = htmlFile, titlesExtractMethod = titlesExtractMethod, removeString = removeString)
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

