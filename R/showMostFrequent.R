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
#' @param corpusDtm A document-term matrix. Can be obtained with 'DocumentTermMatrix(corpus)'
#' @param specificTerms A character vector, defaults to NULL. If specificTerms is provided, only terms included in this vector will be included in the output.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the barchart in the "Outputs" subfolder. 
#' @param filename A character vector, defaults to NULL.It allows to customize the filename of the exported barchart file. 
#' @return A data.frame, barchart, or wordcloud as defined with the 'mode' parameter.
#' @export
#' @examples
#' corpusDtm <- DocumentTermMatrix(corpus).
#' mostFrequent <- ShowMostFrequent(corpusDtm)

ShowMostFrequent <- function(corpusDtm, mode = "data.frame", number = 10, specificTerms = NULL, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0, export = FALSE, filename = NULL) {
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
    wordFrequency <- wordFrequency[wordFrequency$freq>minFrequency,]
    if (stemCompletion == TRUE) {
        wordFrequency$term <- tm::stemCompletion(wordFrequency$term, corpusOriginal)
        for (i in 1:length(wordFrequency$term)) {
            if (wordFrequency$term[i] == "") {
                wordFrequency$term[i] <- row.names(wordFrequency)[i]
            }
        }
    }
    if (mode == "barchart" | mode == "graph") {
        barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("")
        barchart
        if (export == TRUE) {
            if (is.null(filename) == TRUE) {
                filename <- "word frequency barchart"
            }
                if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == FALSE) {
                if (file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs")) == FALSE) {
                    dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
                }
                ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(filename, nameOfProject, nameOfWebsite, sep = " - "), ".png")))
                print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(filename, nameOfProject, nameOfWebsite, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(filename, nameOfProject, nameOfWebsite, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(filename, nameOfProject, nameOfWebsite, sep = " - "), ".pdf"))))
            } else if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste(filename, nameOfProject, sep = " - "), ".png")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(filename, nameOfProject, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(filename, nameOfProject, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(filename, nameOfProject, sep = " - "), ".pdf"))))
            } else {
                if (!file.exists(file.path("Outputs"))) {
                    dir.create(file.path("Outputs"))
                }
                ggplot2::ggsave(file.path("Outputs", paste0(paste(filename, sep = " - "), ".png")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(filename, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(filename, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(filename, sep = " - "), ".pdf"))))
            }
        }
        barchart
    } else if (mode == "wordcloud") {
        wordcloud::wordcloud(rownames(wordFrequency), wordFrequency$freq, min.freq = minFrequency, colors = "black")
    } else if (mode == "data.frame") {
        wordFrequency
    }
} 
