#' Extracts textual contents from a vector of html files
#' 
#' Extracts textual contents from a vector of html files. 
#'  
#' @param articlesHtml A character vector of html files.
#' @param metadata Defaults to NULL. A data.frame presumably created with ExportMetadata() including information on all articles. Number of rows must correspond to the number of articles to be elaborated. This is required when export == TRUE, in order to provide meaningful filenames.
#' @param keepEverything Logical. If TRUE, the functions calls the boilerpipeR::KeepEverythingExtractor from boilerpipeR, instead of boilerpipeR::ArticleExtractor.
#' @param textToBeRemoved A character vector of one or more strings. Provided strings are removed from each article. 
#' @param export Logical, defaults to TRUE. If TRUE, textual contents are saved as individual txt files in a dedicated folder. Filename is based on the medatadata.
#' @param maxTitleCharacters Maximum number of characters allowed in the title. Defaults to 80. 
#' @param divClass If provided, all text included in the chosen div is outputted. 
#' @param removePunctuationInFilename Logical, defaults to TRUE. If TRUE (and export == TRUE), it removes punctuation signs from filemanes to prevent errors in saving files.
#' @return A character vector of txt files, and individual articles saved as txt files in a dedicated folder if 'export' is set to TRUE.
#' @export
#' @examples
#' articlesTxt <- ExtractTxt(articlesHtml, metadata)
ExtractTxt <- function(articlesHtml, metadata = NULL, export = FALSE, maxTitleCharacters = 80, textToBeRemoved = NULL, divClass = NULL, divID = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL, removePunctuationInFilename = TRUE, keepEverything = FALSE, exportParameters = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    numberOfArticles <- length(articlesHtml)
    articlesTxt <- rep(NA, numberOfArticles)
    if (export == TRUE) {
        titles <- metadata$titles
        if (removePunctuationInFilename == TRUE) {
            titles <- gsub("[[:punct:]]", ' ', titles)
        }
        txtFilenames <- paste0(file.path(nameOfProject, nameOfWebsite, "Txt", paste0(paste(metadata$dates, metadata$nameOfWebsite, metadata$articlesId, substring(titles, 1, maxTitleCharacters), sep = " - "), ".txt")))
    }
    if (is.null(divClass) == FALSE) {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)) == 0) {
                    articlesHtml[i] <- NA
                    print(paste("Text div in article with ID", i, "could not be extracted."))
                } else {
                    articlesTxt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@class='", divClass, "']"), XML::xmlValue)
                }
            }
        }
    } else if (is.null(divID) == FALSE) {
        for (i in 1:numberOfArticles) {
            if (articlesHtml[i] != "") {
                articleHtmlParsed <- XML::htmlTreeParse(articlesHtml[i], useInternalNodes = T, encoding = "UTF-8")
                if (length(XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divID, "']"), XML::xmlValue)) == 0) {
                    articlesHtml[i] <- NA
                    print(paste("Text div in article with ID", i, "could not be extracted."))
                } else {
                    articlesTxt[i] <- XML::xpathSApply(articleHtmlParsed, paste0("//div[@id='", divID, "']"), XML::xmlValue)
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
            articlesTxt[i] <- articleTxt
        }
    }
    if (is.null(removeEverythingAfter) == FALSE) {
        articlesTxt <- base::gsub(base::paste0(removeEverythingAfter, ".*"), "", articlesTxt, fixed = FALSE)
    }
    if (is.null(removeEverythingBefore) == FALSE) {
        articlesTxt <- base::gsub(base::paste0(".*", removeEverythingBefore), "", articlesTxt, fixed = FALSE)
    }
    if (is.null(textToBeRemoved) == FALSE) {
        for (i in 1:length(textToBeRemoved)) {
            articlesTxt <- gsub(textToBeRemoved[i], "", articlesTxt, fixed = TRUE)
        }
    }
    if (export == TRUE) {
        for (i in 1:length(articlesTxt)) {
            base::write(articlesTxt[i], file = txtFilenames[i])
        }
    }
    if (exportParameters == TRUE) {
        args <- c("exportExtractTxt", "maxTitleCharacters", "textToBeRemovedExtractTxt", "divClassExtractTxt", "divIDExtractTxt", "removeEverythingAfterExtractTxt", "removeEverythingBeforeExtractTxt", "removePunctuationInFilename", "keepEverything")
        param <- list(export, maxTitleCharacters, paste0(textToBeRemoved, collapse = "§§§"), divClass, divID, removeEverythingAfter, removeEverythingBefore, removePunctuationInFilename, keepEverything)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, "Logs", paste(nameOfWebsite, "updateParameters.csv", sep = " - "))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, "Logs", paste(nameOfWebsite, "updateParameters.csv", sep = " - ")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, "Logs", paste(nameOfWebsite, "updateParameters.csv", sep = " - ")))
    }
    articlesTxt
} 
