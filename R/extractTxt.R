#' Extracts textual contents from a vector of html files
#'
#' Extracts textual contents from a vector of html files.
#'
#' @param articlesHtml A character vector of html files.
#' @param metadata Defaults to NULL. A data.frame presumably created with ExportMetadata() including information on all articles. Number of rows must correspond to the number of articles to be elaborated. This is required when export == TRUE, in order to provide meaningful filenames.
#' @param keepEverything Logical. If TRUE, the functions calls the boilerpipeR::KeepEverythingExtractor from boilerpipeR, instead of boilerpipeR::ArticleExtractor.
#' @param removeString A character vector of one or more strings. Provided strings are removed from each article.
#' @param export Logical, defaults to TRUE. If TRUE, textual contents are saved as individual txt files in a dedicated folder. Filename is based on the medatadata.
#' @param maxTitleCharacters Maximum number of characters allowed in the title. Defaults to 80.
#' @param divClass If provided, all text included in the chosen div is outputted.
#' @param removePunctuationInFilename Logical, defaults to TRUE. If TRUE (and export == TRUE), it removes punctuation signs from filemanes to prevent errors in saving files.
#' @return A character vector of txt files, and individual articles saved as txt files in a dedicated folder if 'export' is set to TRUE.
#' @export
#' @examples
#' contents <- ExtractTxt(articlesHtml, metadata)
ExtractTxt <- function(articlesHtml, metadata = NULL, export = FALSE, maxTitleCharacters = 80, removeString = NULL, divClass = NULL, divId = NULL, customXpath = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL, removePunctuationInFilename = TRUE, removeTitleFromTxt = FALSE, titles = NULL, keepEverything = FALSE, exportParameters = TRUE, progressBar = TRUE, project = NULL, website = NULL) {
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
        args <- c("export_ExtractTxt", "maxTitleCharacters", "removeString_ExtractTxt", "divClass_ExtractTxt", "divId_ExtractTxt", "customXpath_ExtractTxt", "removeEverythingAfter_ExtractTxt", "removeEverythingBefore_ExtractTxt", "removePunctuationInFilename", "keepEverything_ExtractTxt", "removeTitleFromTxt")
        param <- list(export, maxTitleCharacters, paste0(removeString, collapse = "§§§"), divClass, divId, customXpath, removeEverythingAfter, removeEverythingBefore, removePunctuationInFilename, keepEverything, removeTitleFromTxt)
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
    contents <- rep(NA, numberOfArticles)
    if (export == TRUE) {
        titles <- metadata$titles
        if (removePunctuationInFilename == TRUE) {
            titles <- gsub("[[:punct:]]", ' ', titles)
        }
        txtFilenames <- paste0(file.path(project, website, "Txt", paste0(paste(metadata$dates, metadata$website, metadata$id, substring(titles, 1, maxTitleCharacters), sep = " - "), ".txt")))
    }
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = numberOfArticles, style = 3, title = "Extracting titles")
    }
    if (gtools::invalid(divClass) == FALSE) {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)) == 0) {
                    articlesHtml[i] <- NA
                    print(paste("Text div in article with ID", i, "could not be extracted."))
                } else {
                    contents[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    } else if (gtools::invalid(divId) == FALSE) {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divId, "']"), XML::xmlValue)) == 0) {
                    articlesHtml[i] <- NA
                    print(paste("Text div in article with ID", i, "could not be extracted."))
                } else {
                    contents[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divId, "']"), XML::xmlValue)
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    } else if (gtools::invalid(customXpath) == FALSE) {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                extractedTemp <- XML::xpathSApply(articleHtmlParsed, customXpath, XML::xmlValue)
                if (length(extractedTemp) == 0) {
                    articlesHtml[i] <- NA
                    print(paste("Text in article with ID", i, "could not be extracted."))
                } else {
                    contents[i] <- extractedTemp
                }
                if (progressBar == TRUE) {
                    setTxtProgressBar(pb, i)
                }
            }
        }
    } else {
        for (i in 1:numberOfArticles) {
            if (keepEverything == TRUE) {
                articleTxt <- boilerpipeR::KeepEverythingExtractor(articlesHtml[i])
            } else {
                articleTxt <- boilerpipeR::ArticleExtractor(articlesHtml[i])
            }
            contents[i] <- articleTxt
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
        for (i in seq_along(contents)) {
            if (is.null(titles[i])==FALSE) {
                if (titles[i]!="") {
                    contents[i] <- base::sub(pattern = titles[i], replacement = "", x = contents[i], fixed = TRUE)
                }
            }
        }
    }
    if (gtools::invalid(removeEverythingAfter) == FALSE) {
        contents <- base::gsub(base::paste0(removeEverythingAfter, ".*"), "", contents, fixed = FALSE)
    }
    if (gtools::invalid(removeEverythingBefore) == FALSE) {
        contents <- base::gsub(base::paste0(".*", removeEverythingBefore), "", contents, fixed = FALSE)
    }
    if (gtools::invalid(removeString) == FALSE) {
        for (i in 1:length(removeString)) {
            contents <- gsub(removeString[i], "", contents, fixed = TRUE)
        }
    }
    if (export == TRUE) {
        for (i in 1:length(contents)) {
            base::write(contents[i], file = txtFilenames[i])
        }
    }
    return(contents)
}
