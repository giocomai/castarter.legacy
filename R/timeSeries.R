#' Creates time series of the frequency of specific terms.
#'
#' It creates time series with the frequency of one or more terms, in one or more websites. 
#' @param corpus A corpus created by the TM package..
#' @param specificTerms Character vector with one or more words to be analysed. 
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the "Outputs" subfolder. 
#' @export
#' @examples
#' CreateTimeSeries(corpus, specificTerms = c("word1", "word2"))

CreateTimeSeries <- function(corpus, specificTerms, specificWebsites = "", startDate = NULL, endDate = NULL, rollingAverage = 30, export = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (is.null(startDate)==FALSE) {
        corpus <- corpus[meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
    }
    if (is.null(endDate)==FALSE) {
        corpus <- corpus[meta(corpus, "datetimestamp") < as.POSIXct(endDate)]
    }
    time <- as.character(strptime(as.POSIXct(unlist(meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
    nameOfWebsitesIncluded <- as.character(unlist(meta(corpus, "author")))
    corpusDtm <- DocumentTermMatrix(corpus)
    if (length(specificTerms>1)) {
        frequencyOfSpecificTerms <- as.table(rollup(corpusDtm[, specificTerms], 1, time))
    } else {
        frequencyOfSpecificTerms <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, specificTerms])), list(time, nameOfWebsitesIncluded), sum))
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
    timeSeries <- autoplot(termSeries, facets = NULL) +
        ggtitle(paste("Time series of references to", paste(dQuote(specificTerms), collapse = ", "))) +
        scale_x_datetime("Date") +
        theme(plot.title = element_text(size = rel(1.2)),
              legend.title = element_text(size = rel(1.2)),
              legend.text = element_text(size = rel(1))) +
        scale_colour_brewer(type = "qual", palette = 6)
    if (is.null(nameOfWebsite) == FALSE) {
        timeSeries <- timeSeries + ggtitle(paste("Time series of references to", paste(dQuote(specificTerms), collapse = ", "), "in", nameOfWebsite))
        }
    if (export == TRUE) {
        timeSeries
        if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == FALSE) {
            if (file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs")) == FALSE) {
                dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
            }
            ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(specificTerms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(specificTerms, collapse = " - "), sep = " - "), ".png"))))
            ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(specificTerms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(specificTerms, collapse = " - "), sep = " - "), ".pdf"))))
        } else if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == TRUE) {
            ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(specificTerms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(specificTerms, collapse = " - "), sep = " - "), ".png"))))
            ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(specificTerms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(specificTerms, collapse = " - "), sep = " - "), ".pdf"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggsave(file.path("Outputs", paste0(paste("timeseries", paste(specificTerms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(specificTerms, collapse = " - "), sep = " - "), ".png"))))
            ggsave(file.path("Outputs", paste0(paste("timeseries", paste(specificTerms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(specificTerms, collapse = " - "), sep = " - "), ".pdf"))))
        }
    }
    timeSeries
} 
