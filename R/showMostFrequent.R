#' Shows most frequent terms in a corpus
#' 
#' Shows most frequent terms in a corpus starting from a document term matrix.
#'  
#' @param corpusDtm A document term matrix.
#' @param mode Defines the type of output. Can be
##' \itemize{
##'  \item{"vector"}{: Outputs a named character vector. This is the default option.}
##'  \item{"barchart"}{: Outputs a ggplot2 barchart.}
##'  \item{"wordcloud"}{: Outputs a wordcloud.}
##' }
##'
#' @return A vector, barchart, or wordcloud as defined with the 'mode' parameter.
#' @export
#' @examples
#' corpusDtm <- DocumentTermMatrix(corpus).
#' mostFrequent <- ShowMostFrequent(corpusDtm)

ShowMostFrequent <- function(corpusDtm, mode = "vector", number = 10, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0) {
    freq <- sort(col_sums(corpusDtm, na.rm = TRUE), decreasing = TRUE)
    if (number == "all") {
        number <- length(freq)
    }
    wordFrequency <- data.frame(word = names(freq), freq = freq)[1:number, ]
    if (stemCompletion == TRUE) {
        wordFrequency$word <- stemCompletion(wordFrequency$word, corpusOriginal)
        for (i in 1:length(wordFrequency$word)) {
            if (wordFrequency$word[i] == "") {
                wordFrequency$word[i] <- row.names(wordFrequency)[i]
            }
        }
    }
    if (mode == "barchart" | mode == "graph") {
        ggplot(data = wordFrequency, aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + coord_flip()
    } else if (mode == "wordcloud") {
        wordcloud(rownames(wordFrequency), wordFrequency$freq, min.freq = minFrequency, colors = "black")
    } else if (mode == "vector") {
        wordFrequency
    }
} 
