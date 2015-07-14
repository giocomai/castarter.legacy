#' Creates time series of the frequency of specific terms.
#'
#' It creates time series with the frequency of one or more terms, in one or more websites. 
#' @param corpusDtm A document term matrix.
#' @param specificTerms Character vector with one or more words to be analysed. 
#' @export
#' @examples
#' CreateTimeSeries(corpus, specificTerms = c("word1", "word2"))

CreateTimeSeries <- function(corpus, specificTerms, specificWebsites = "", startDate = NULL, rollingAverage = 30) {
    if (is.null(startDate)==FALSE) {
        corpus <- corpus[meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
    }
    time <- as.character(strptime(as.POSIXct(unlist(meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
    nameOfWebsite <- as.character(unlist(meta(corpus, "author")))
    corpusDtm <- DocumentTermMatrix(corpus)
    if (length(specificTerms>1)) {
        frequencyOfSpecificTerms <- as.table(rollup(corpusDtm[, specificTerms], 1, time))
    } else {
        frequencyOfSpecificTerms <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, specificTerms])), list(time, nameOfWebsite), sum))
    }
    # to filter specific websites
    if (specificWebsites != "") {
        frequencyOfSpecificTerms <- as.table(frequencyOfSpecificTerms[, specificWebsites])
    }
    names(dimnames(frequencyOfSpecificTerms)) <- NULL
    termSeries <- zoo(frequencyOfSpecificTerms/c(tapply(row_sums(corpusDtm), time, sum)), order.by = as.POSIXct(rownames(frequencyOfSpecificTerms)))
    termSeries <- merge(termSeries, zoo(, seq(start(termSeries), end(termSeries), "DSTday")), fill = NaN)
    if (rollingAverage != "") {
        termSeries <- rollapply(termSeries, rollingAverage, align = "left", mean, na.rm = TRUE)
    }
    autoplot(termSeries, facets = NULL) +
        ggtitle(paste("Time series of references to", dQuote(specificTerms))) +
        scale_x_datetime("Date") +
        theme(plot.title = element_text(size = rel(1.8)),
              legend.title = element_text(size = rel(1.5)),
              legend.text = element_text(size = rel(1))) +
        scale_colour_brewer(type = "qual", palette = 6)
} 
