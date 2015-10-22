#' Shows most frequent terms in a corpus
#' 
#' Shows most frequent terms in a corpus starting from a document term matrix.
#'  
#' @param corpusDtm A document term matrix.
#' @param mode Defines the type of output. Can be
##' \itemize{
##'  \item{"data.frame"}{: Outputs a data.frame This is the default option.}
##'  \item{"barchart"}{: Outputs a ggplot2 barchart.}
##'  \item{"wordcloud"}{: Outputs a wordcloud.}
##' }
##'
#' @param specificTerms A character vector, defaults to NULL. If specificTerms is provided, only terms included in this vector will be included in the output.
#' @return A data.frame, barchart, or wordcloud as defined with the 'mode' parameter.
#' @export
#' @examples
#' corpusDtm <- DocumentTermMatrix(corpus).
#' mostFrequent <- ShowMostFrequent(corpusDtm)

ShowMostFrequent <- function(corpusDtm, mode = "data.frame", number = 10, specificTerms = NULL, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0) {
    freq <- sort(slam::col_sums(corpusDtm, na.rm = TRUE), decreasing = TRUE)
    if (is.null(specificTerms) == FALSE) {
        freq <- freq[base::match(specificTerms, names(freq))]
        freq <- freq[is.na(freq)==FALSE]
        freq <- sort(freq, decreasing = TRUE)
    }
    if (number == "all") {
        number <- length(freq)
    }
    wordFrequency <- data.frame(term = names(freq), freq = freq)[1:number, ]
    if (stemCompletion == TRUE) {
        wordFrequency$term <- tm::stemCompletion(wordFrequency$term, corpusOriginal)
        for (i in 1:length(wordFrequency$term)) {
            if (wordFrequency$term[i] == "") {
                wordFrequency$term[i] <- row.names(wordFrequency)[i]
            }
        }
    }
    if (mode == "barchart" | mode == "graph") {
        ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("")
    } else if (mode == "wordcloud") {
        wordcloud::wordcloud(rownames(wordFrequency), wordFrequency$freq, min.freq = minFrequency, colors = "black")
    } else if (mode == "data.frame") {
        wordFrequency
    }
} 
