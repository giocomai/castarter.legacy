#' Extracts textual contents from a vector of html files
#' 
#' Extracts textual contents from a vector of html files. 
#'  
#' @param articlesHtml A character vector of html files.
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
ExtractTxt <- function(articlesHtml, metadata = "", export = TRUE, maxTitleCharacters = 80, textToBeRemoved = NULL, divClass = NULL, divID = NULL, removeEverythingAfter = NULL, removeEverythingBefore = NULL, removePunctuationInFilename = TRUE, keepEverything = FALSE) {
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
    articlesTxt
} 
