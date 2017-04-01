#' Creates time series of the frequency of specific terms.
#'
#' It creates time series with the frequency of one or more terms, in one or more websites.
#' @param corpus A corpus of the 'tm' or 'quanteda' type, presumably created with castarter's ConvertToCorpus() function.
#' @param terms A character vector with one or more words to be analysed, or a 'quanteda' dictionary if corpus/corpusDtm are also of the 'quanteda' type.
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param verticalLine Defaults to NULL. Draws a vertical dotted line at the date provided. If given, value must correspond to one or more dates in the YMD format, e.g. "2016-08-17".
#' @param smoothLine Logical, defaults to FALSE. If TRUE draws a smooth line over the time series.
#' @param customTitle A character vector, defaults to NULL. If provided, it overrides default graph title.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If project and website are provided, in saves the timeseries in the "Outputs" subfolder.
#' @param corpusDtm A document-term matrix or a document-feature matrix of the 'quanteda' type. If provided, other parameters (specificWebsites, startDate, endDate) are ignored, but computation is faster.
#' @export
#' @examples
#' ShowTS(corpus, terms = c("word1", "word2"))

ShowTS <- function(terms, corpusDtm = NULL, corpus = NULL, specificWebsites = NULL, startDate = NULL, endDate = NULL, rollingAverage = 30, align = "center", verticalLine = NULL, smoothLine = FALSE, customTitle = NULL, export = FALSE, allWebsitesAsOne = FALSE, dygraphs = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (quanteda::is.corpus(corpus)==TRUE) {
        time <- as.character(strptime(as.POSIXct(quanteda::docvars(corpus, "date"), origin = "1970-01-01"), "%Y-%m-%d"))
        websitesIncluded <- as.character(quanteda::docvars(corpus, "website"))
    } else {
        time <- as.character(strptime(as.POSIXct(unlist(NLP::meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
        websitesIncluded <- as.character(unlist(NLP::meta(corpus, "author")))
        if (is.null(corpusDtm) == FALSE) {
            if (quanteda::is.dfm(corpusDtm) == FALSE) {
                if (is.null(startDate)==FALSE) {
                    corpus <- corpus[NLP::meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
                }
                if (is.null(endDate)==FALSE) {
                    corpus <- corpus[NLP::meta(corpus, "datetimestamp") < as.POSIXct(endDate)]
                }
            }
        }
    }
    if (allWebsitesAsOne == TRUE) {
        websitesIncluded <- rep("all", length(websitesIncluded))
    }
    if (is.null(corpusDtm) == TRUE) {
        if (quanteda::is.corpus(corpus)==TRUE) {
            corpusDtm <- quanteda::dfm(x = corpus, groups=c("date", "website"))
            corpusDtm <- quanteda::dfm_weight(corpusDtm, "relFreq")
        } else {
            if (is.null(startDate)==FALSE) {
                corpus <- corpus[NLP::meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
            }
            if (is.null(endDate)==FALSE) {
                corpus <- corpus[NLP::meta(corpus, "datetimestamp") < as.POSIXct(endDate)]
            }
            corpusDtm <- tm::DocumentTermMatrix(corpus)
        }
    } else {
        if (quanteda::is.dfm(corpusDtm)==TRUE) {
            corpusDtm <- quanteda::dfm_weight(corpusDtm, "relFreq")
        }
    }
    if (quanteda::is.dfm(corpusDtm)==TRUE) {
        if (as.character(class(terms))=="dictionary") {
            termsDic <- terms
        } else {
            termsL <- as.list(terms)
            termsL <- setNames(object = termsL, nm = terms)
            termsDic <- quanteda::dictionary(x = termsL)
        }
        corpusDtmDic <- quanteda::dfm_lookup(x = corpusDtm, dictionary = termsDic)
        dailyFreq <- data.frame(docs = quanteda::docnames(corpusDtmDic), base::as.data.frame(corpusDtmDic))
        dailyFreq <- tidyr::separate(data = dailyFreq, col = docs, into = c("Date","website"), sep = "\\.")
        if (is.null(startDate)==FALSE) {
            dailyFreq <-dailyFreq[as.Date(dailyFreq$Date)>=as.Date(startDate),]
        }
        if (is.null(endDate)==FALSE) {
            dailyFreq <-dailyFreq[as.Date(dailyFreq$Date)<=as.Date(endDate),]
        }
    }
    if (length(terms)>1) {
        if (quanteda::is.dfm(corpusDtm)==TRUE) {
            dailyFreqL <- reshape2::melt(data = dailyFreq, id.vars = c("Date", "website"), variable.name="Term", value.name="Frequency")
            dailyFreqL$Date <- as.Date(dailyFreqL$Date)
            dailyFreqAgg <- stats::aggregate(Frequency ~ Date + Term, dailyFreqL, mean)
            dailyFreq <- reshape2::dcast(data = dailyFreqAgg, Date ~ Term, value.var = "Frequency")
        } else {
            frequencyOfterms <- as.table(slam::rollup(corpusDtm[, terms], 1, time))
        }
    } else {
        if (quanteda::is.dfm(corpusDtm)==TRUE) {
            dailyFreqL <- reshape2::melt(data = dailyFreq, id.vars = c("Date", "website"), variable.name="Term", value.name="Frequency")
            dailyFreqL$Date <- as.Date(dailyFreqL$Date)
            dailyFreqAgg <- stats::aggregate(Frequency ~ Date + website, dailyFreqL, mean)
            dailyFreq <- reshape2::dcast(data = dailyFreqAgg, Date ~ website, value.var = "Frequency")
        } else {
            frequencyOfterms <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, terms])), list(time, websitesIncluded), sum))
        }
    }
    # to filter specific websites
    if (is.null(specificWebsites) == FALSE) {
        frequencyOfterms <- as.table(frequencyOfterms[, specificWebsites])
    }
    if (quanteda::is.dfm(corpusDtm)==TRUE) {
        dailyFreqZoo <- zoo::zoo(dailyFreq[2:length(dailyFreq)], dailyFreq$Date)
        dailyFreqZoo <- merge(dailyFreqZoo, zoo::zoo(, seq(start(dailyFreqZoo), end(dailyFreqZoo), "day")), fill = NA)
    } else {
        names(dimnames(frequencyOfterms)) <- NULL
        termSeries <- zoo::zoo(frequencyOfterms/c(tapply(slam::row_sums(corpusDtm), time, sum)), order.by = as.POSIXct(rownames(frequencyOfterms)))
        dailyFreqZoo <- merge(termSeries, zoo::zoo(, seq(start(termSeries), end(termSeries), "DSTday")), fill = NA)
    }
    if (rollingAverage != "") {
        dailyFreqZoo <- zoo::rollapply(dailyFreqZoo, rollingAverage, align = align, mean, na.rm = TRUE)
    }
    if (as.character(class(terms))=="dictionary") {
        terms <- names(terms)
    }
    timeSeries <- zoo::autoplot.zoo(dailyFreqZoo, facets = NULL) +
        ggplot2::ggtitle(paste("Word frequency of", paste(dQuote(terms), collapse = ", "))) +
        ggplot2::scale_y_continuous("") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
              legend.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
              legend.text = ggplot2::element_text(size = ggplot2::rel(1))) +
        ggplot2::scale_colour_brewer(type = "qual", palette = 6) +
        ggplot2::geom_line(size = 1) +
        ggplot2::expand_limits(y=0) +
        ggplot2::theme_minimal()
    if (quanteda::is.dfm(corpusDtm) == TRUE) {
        timeSeries <- timeSeries + ggplot2::scale_x_date("")
    } else {
        timeSeries <- timeSeries + ggplot2::scale_x_datetime("")
    }
    if (length(terms)>1) {
        timeSeries <- timeSeries + ggplot2::labs(color = "Terms")
    } else {
        timeSeries <- timeSeries + ggplot2::labs(color = "Websites")
    }
    if (is.null(verticalLine)==FALSE) {
        timeSeries <- timeSeries + ggplot2::geom_vline(xintercept = c(as.numeric(as.Date(verticalLine))), linetype = 2)
    }
    if (is.null(website) == FALSE) {
        timeSeries <- timeSeries + ggplot2::ggtitle(paste("References to", paste(dQuote(terms), collapse = ", "), "in", website))
    }
    if (is.null(customTitle) == FALSE) {
        timeSeries <- timeSeries + ggplot2::ggtitle(customTitle)
    }
    if (smoothLine == TRUE) {
        timeSeries <- timeSeries + ggplot2::stat_smooth(size=1.2, se=FALSE)
    }
    if (export == TRUE) {
        timeSeries
        if (is.null(project) == FALSE & is.null(website) == FALSE) {
            if (file.exists(file.path(project, website, "Outputs")) == FALSE) {
                dir.create(file.path(project, website, "Outputs"))
            }
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg"))))
        } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg"))))
        }
    }
    if (dygraphs == TRUE) {
        dailyFreqXts <- xts::as.xts(dailyFreqZoo)
        timeSeriesDygraph <- dygraphs::dygraph(dailyFreqXts) %>% dygraphs::dyRangeSelector()
        timeSeriesDygraph
    } else {
        timeSeries
    }
}

#' Creates a time series graph showing the distribution of documents by date
#'
#' Creates a time series graph showing the distribution of documents by date.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param specificWebsites Character vector indicating which websites (defined by relative website) have to be included in graph. If left to default, includes all websites present in the dataset.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param export Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If project and website are provided, in saves the timeseries in the correspondent "Outputs" subfolder.
#' @param method Accepted values: "numberOfArticles" (default, creates time series based on number of publications per day); "numberOfCharacters" (creates time series based on number of charachters per day, currently does not work in conjunction with specificWebsites option).
#' @return A ggplot2 time series showing number of articles published each day.
#' @export
#' @examples
#' ShowDistribution(dataset)

ShowDistribution <- function(dataset, specificWebsites = NULL, rollingAverage = 30, align = "center", customTitle = NULL, method = "numberOfArticles", export = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    tab <- base::table(dataset$date, dataset$website)
    date <- base::as.POSIXct(base::rownames(tab))
    if (method == "numberOfArticles") {
        if (base::is.null(specificWebsites) == FALSE) {
            docSeries <- zoo::zoo(tab[,specificWebsites], order.by=date)
        } else {
            docSeries <- zoo::zoo(tab, order.by=date)
        }
    } else if (method == "numberOfCharacters") {
        numberOfArticles <- length(dataset$contents)
        ncharArticles <- data.frame(date = rep(NA, numberOfArticles), nchar = rep(NA, numberOfArticles))
        for (i in 1:numberOfArticles) {
            ncharArticles$nchar[i] <- nchar(dataset$contents[i])
            ncharArticles$date[i] <- dataset$date[i]
        }
        ncharPerDay <- plyr::ddply(ncharArticles,~date,plyr::summarise,ncharPerDay=sum(nchar))
        docSeries <- zoo::zoo(ncharPerDay$ncharPerDay, order.by = date)
    }
    docSeries <- base::merge(docSeries, zoo::zoo(, seq(start(docSeries), end(docSeries), "DSTday")), fill=0)
    docSeries <- zoo::rollapply(docSeries, rollingAverage, align=align, mean, na.rm=TRUE)
    distributionOfCorpus <- zoo::autoplot.zoo(docSeries, facets = NULL)
    if (method == "numberOfArticles") {
        distributionOfCorpus <- distributionOfCorpus + ggplot2::ggtitle(paste0("Number of publications per day in ", project)) + ggplot2::scale_x_datetime("") + ggplot2::scale_y_continuous("") + ggplot2::expand_limits(y=0)
    } else if (method == "numberOfCharacters") {
        distributionOfCorpus <- distributionOfCorpus + ggplot2::scale_y_continuous("") + ggplot2::ggtitle(paste0("Number of characters per day in ", project)) + ggplot2::scale_x_datetime("") + ggplot2::scale_y_continuous(name = "") +  ggplot2::expand_limits(y=0)
    }
    if (is.null(customTitle)==FALSE) {
        distributionOfCorpus <- distributionOfCorpus + ggplot2::ggtitle(customTitle)
    }
    if (export == TRUE) {
        distributionOfCorpus
        if (is.null(project) == FALSE & is.null(website) == FALSE) {
            if (file.exists(file.path(project, website, "Outputs")) == FALSE) {
                dir.create(file.path(project, website, "Outputs"))
            }
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".png")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".pdf")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".svg")))
            print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".svg"))))
        } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".svg"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".svg"))))
        }
    }
    distributionOfCorpus
}
