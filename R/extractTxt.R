#' Extracts textual contents from a vector of html files
#' 
#' Extracts textual contents from a vector of html files. 
#'  
#' @param articlesHtml A character vector of html files.
#' @param keepEverything Logical. If TRUE, the functions calls the KeepEverythingExtractor from boilerpipeR, instead of ArticleExtractor.
#' @param export Logical, defaults to TRUE. If TRUE, textual contents are saved as individual txt files in a dedicated folder. Filename is based on the medatadata.
#' @return A character vector of txt files, and individual articles saved as txt files in a dedicated folder if 'export' is set to TRUE.
#' @export
#' @examples
#' articlesTxt <- ExtractTxt(articlesHtml, metadata)
ExtractTxt <- function(articlesHtml, metadata = "", export = TRUE, maxTitleCharacters = 80, textToBeRemoved = "", removeEverythingAfter = NULL, removeEverythingBefore = NULL, removePunctuationInFilename = TRUE, keepEverything = FALSE) {
    numberOfArticles <- length(articlesHtml)
    articlesTxt <- rep(NA, numberOfArticles)
    if (export == TRUE) {
        titles <- metadata$titles
        txtFilenames <- paste0(file.path(nameOfProject, nameOfWebsite, "Txt", paste0(paste(metadata$dates, metadata$nameOfWebsite, metadata$articlesId, substring(titles, 1, maxTitleCharacters), sep = " - "), ".txt")))
    }
    for (i in 1:numberOfArticles) {
        if (keepEverything == TRUE) {
            articleTxt <- KeepEverythingExtractor(articlesHtml[i])
        } else {
            articleTxt <- ArticleExtractor(articlesHtml[i])
        }
        if (textToBeRemoved != "") {
            for (j in 1:length(textToBeRemoved)) {
                articleTxt <- gsub(textToBeRemoved[j], "", articleTxt, fixed = TRUE)
            }
        }
        if (is.null(removeEverythingAfter) == FALSE) {
            articleTxt <- base::gsub(base::paste0(removeEverythingAfter, ".*"), "", articleTxt, fixed = FALSE)
        }
        if (is.null(removeEverythingBefore) == FALSE) {
            articleTxt <- base::gsub(base::paste0(".*", removeEverythingBefore), "", articleTxt, fixed = FALSE)
        }
        articlesTxt[i] <- articleTxt
        if (export == TRUE) {
            base::write(articleTxt, file = txtFilenames[i])
        }
    }
    articlesTxt
} 
