#' Extracts textual contents from a vector of html files
#' 
#' Extracts textual contents from a vector of html files. 
#'  
#' @param articlesHtml A character vector of html files.
#' @param keepEverything Logical. If TRUE, the functions calls the KeepEverythingExtractor from boilerpipeR, instead of ArticleExtractor.
#' @return A character vector of txt files, and individual articles saves ad txt files in a dedicated folder. 
#' @export
#' @examples
#' articlesTxt <- ExtractTxt(articlesHtml, metadata)
ExtractTxt <- function(articlesHtml, metadata, numberOfTitleCharacters = 80, textToBeRemoved = "", removeEverythingAfter = "", removePunctuationInFilename = TRUE, keepEverything = FALSE) {
    numberOfArticles <- length(articlesHtml)
    articlesTxt <- rep(NA, numberOfArticles)
    titles <- metadata$titles
    txtFilenames <- paste0(file.path(nameOfProject, nameOfWebsite, "Txt", paste0(paste(metadata$dates, metadata$nameOfWebsite, metadata$articlesId, 
        substring(titles, 1, numberOfTitleCharacters), sep = " - "), ".txt")))
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
        if (removeEverythingAfter != "") {
            articleTxt <- sub(paste0(removeEverythingAfter, ".*"), "", articleTxt)
        }
        articlesTxt[i] <- articleTxt
        write(articleTxt, file = txtFilenames[i])
    }
    articlesTxt
} 
