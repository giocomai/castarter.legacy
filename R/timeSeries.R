#' Creates time series of the frequency of specific terms.
#'
#' It creates time series with the frequency of one or more terms, in one or more websites. 
#' @param corpus A corpus created by the TM package..
#' @param terms Character vector with one or more words to be analysed. 
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the "Outputs" subfolder. 
#' @export
#' @examples
#' CreateTimeSeries(corpus, terms = c("word1", "word2"))

CreateTimeSeries <- function(corpus, terms, specificWebsites = NULL, startDate = NULL, endDate = NULL, rollingAverage = 30, export = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (is.null(startDate)==FALSE) {
        corpus <- corpus[NLP::meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
    }
    if (is.null(endDate)==FALSE) {
        corpus <- corpus[NLP::meta(corpus, "datetimestamp") < as.POSIXct(endDate)]
    }
    time <- as.character(strptime(as.POSIXct(unlist(NLP::meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
    nameOfWebsitesIncluded <- as.character(unlist(NLP::meta(corpus, "author")))
    corpusDtm <- tm::DocumentTermMatrix(corpus)
    if (length(terms)>1) {
        frequencyOfterms <- as.table(slam::rollup(corpusDtm[, terms], 1, time))
    } else {
        frequencyOfterms <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, terms])), list(time, nameOfWebsitesIncluded), sum))
    }
    # to filter specific websites
    if (is.null(specificWebsites) == FALSE) {
        frequencyOfterms <- as.table(frequencyOfterms[, specificWebsites])
    }
    names(dimnames(frequencyOfterms)) <- NULL
    termSeries <- zoo::zoo(frequencyOfterms/c(tapply(slam::row_sums(corpusDtm), time, sum)), order.by = as.POSIXct(rownames(frequencyOfterms)))
    termSeries <- merge(termSeries, zoo::zoo(, seq(start(termSeries), end(termSeries), "DSTday")), fill = NaN)
    if (rollingAverage != "") {
        termSeries <- zoo::rollapply(termSeries, rollingAverage, align = "left", mean, na.rm = TRUE)
    }
    timeSeries <- zoo::autoplot.zoo(termSeries, facets = NULL) +
        ggplot2::ggtitle(paste("Word frequency of", paste(dQuote(terms), collapse = ", "))) +
        ggplot2::scale_x_datetime("") +
        ggplot2::scale_y_continuous("") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
              legend.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
              legend.text = ggplot2::element_text(size = ggplot2::rel(1))) +
        ggplot2::scale_colour_brewer(type = "qual", palette = 6) +
        ggplot2::labs(color = "Terms") +
        ggplot2::geom_line(size = 1)
    if (is.null(nameOfWebsite) == FALSE) {
        timeSeries <- timeSeries + ggplot2::ggtitle(paste("Time series of references to", paste(dQuote(terms), collapse = ", "), "in", nameOfWebsite))
        }
    if (export == TRUE) {
        timeSeries
        if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == FALSE) {
            if (file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs")) == FALSE) {
                dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
            }
            ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".png"))))
            ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".pdf"))))
        } else if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == TRUE) {
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".pdf"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".pdf"))))
        }
    }
    timeSeries
} 

#' Creates a time series graph showing the distribution of documents by date
#' 
#' Creates a time series graph showing the distribution of documents by date.
#'  
#' @param dataset A dataset created with 'castarter'.
#' @param specificWebsites Character vector indicating which websites (defined by relative nameOfWebsite) have to be included in graph. If left to default, includes all websites present in the dataset.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param export Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the correspondent "Outputs" subfolder. 
#' @return A ggplot2 time series showing number of articles published each day. 
#' @export
#' @examples
#' CreateDistributionTimeSeries(dataset)

CreateDistributionTimeSeries <- function(dataset, specificWebsites = NULL, rollingAverage = 30, nameOfProject = NULL, nameOfWebsite = NULL) {
    tab <- base::table(dataset$dates, dataset$nameOfWebsite)
    dates <- base::as.POSIXct(base::rownames(tab))
    if (base::is.null(specificWebsites) == FALSE) {
        docSeries <- zoo::zoo(tab[,specificWebsites], order.by=dates)
    } else {
        docSeries <- zoo::zoo(tab, order.by=dates)
    }
    docSeries <- base::merge(docSeries, zoo::zoo(, seq(start(docSeries), end(docSeries), "DSTday")), fill=0)
    docSeries <- zoo::rollapply(docSeries, rollingAverage, align="right", mean, na.rm=TRUE)
    distributionOfCorpus <- zoo::autoplot.zoo(docSeries, facets = NULL)
    distributionOfCorpus + ggplot2::ggtitle("Number of publications per day") + ggplot2::scale_x_datetime("Date")
}