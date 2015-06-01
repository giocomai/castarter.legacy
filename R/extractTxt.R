
ExtractTxt <- function(articlesHtml, metadata, numberOfTitleCharacters = 80, textToBeRemoved = "", removeEverythingAfter = "", removePunctuationInFilename = TRUE) {
    numberOfArticles <- length(articlesHtml)
    articlesTxt <- rep(NA, numberOfArticles)
    titles <- metadata$titles
    txtFilenames <- paste0(file.path(nameOfProject, nameOfWebsite, "Txt", paste0(paste(metadata$dates, metadata$nameOfWebsite, metadata$articlesId, 
        substring(titles, 1, numberOfTitleCharacters), sep = " - "), ".txt")))
    for (i in 1:numberOfArticles) {
        articleTxt <- ArticleExtractor(articlesHtml[i])
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
