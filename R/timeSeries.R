CreateTimeSeries <- function(allDatasets, corpus, specificTerm, specificWebsites = "", startDate = "", rollingAverage = 30) {
    time <- as.character(strptime(allDatasets$dates, "%Y-%m-%d"))
    nameOfWebsite <- as.character(allDatasets$nameOfWebsite)
    corpusDtm <- DocumentTermMatrix(corpus)
    frequencyOfSpecificTerm <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, specificTerm])), list(time, nameOfWebsite), sum))
    # to filter specific websites
    if (specificWebsites != "") {
        frequencyOfSpecificTerm <- as.table(frequencyOfSpecificTerm[, specificWebsites])
    }
    names(dimnames(frequencyOfSpecificTerm)) <- NULL
    termSeries <- zoo(frequencyOfSpecificTerm/c(tapply(row_sums(corpusDtm), time, sum)), order.by = as.POSIXct(rownames(frequencyOfSpecificTerm)))
    termSeries <- merge(termSeries, zoo(, seq(start(termSeries), end(termSeries), "DSTday")), fill = NaN)
    if (rollingAverage != "") {
        termSeries <- rollapply(termSeries, rollingAverage, align = "left", mean, na.rm = TRUE)
    }
    autoplot(termSeries, facets = NULL) + ggtitle(paste("Time series of references to", dQuote(specificTerm))) + scale_x_datetime("Date") + theme(plot.title = element_text(size = rel(1.8)), 
        legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1))) + scale_colour_brewer(type = "qual", palette = 6)
} 
