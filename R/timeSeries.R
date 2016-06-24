#' Creates time series of the frequency of specific terms.
#'
#' It creates time series with the frequency of one or more terms, in one or more websites. 
#' @param corpus A corpus of the 'tm' or 'quanteda' type, presumably created with castarter's ConvertToCorpus() function.
#' @param terms A character vector with one or more words to be analysed, or a 'quanteda' dictionary if corpus/corpusDtm are also of the 'quanteda' type.
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the "Outputs" subfolder. 
#' @param corpusDtm A document-term matrix or a document-feature matrix of the 'quanteda' type. If provided, other parameters (specificWebsites, startDate, endDate) are ignored, but computation is faster. 
#' @export
#' @examples
#' CreateTimeSeries(corpus, terms = c("word1", "word2"))

CreateTimeSeries <- function(terms, corpusDtm = NULL, corpus = NULL, specificWebsites = NULL, startDate = NULL, endDate = NULL, rollingAverage = 30, align = "center", export = FALSE, allWebsitesAsOne = FALSE, dygraphs = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (quanteda::is.corpus(corpus)==TRUE) {
        time <- as.character(strptime(as.POSIXct(quanteda::docvars(corpus, "date"), origin = "1970-01-01"), "%Y-%m-%d"))
        nameOfWebsitesIncluded <- as.character(quanteda::docvars(corpus, "nameOfWebsite"))
    } else {
        if (is.null(corpusDtm) == FALSE) {
            if (quanteda::is.dfm(corpusDtm) == FALSE) {
                if (is.null(startDate)==FALSE) {
                    corpus <- corpus[NLP::meta(corpus, "datetimestamp") > as.POSIXct(startDate)]
                }
                if (is.null(endDate)==FALSE) {
                    corpus <- corpus[NLP::meta(corpus, "datetimestamp") < as.POSIXct(endDate)]
                }
                time <- as.character(strptime(as.POSIXct(unlist(NLP::meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
                nameOfWebsitesIncluded <- as.character(unlist(NLP::meta(corpus, "author")))
            }
        }
    }
    if (allWebsitesAsOne == TRUE) {
        nameOfWebsitesIncluded <- rep("all", length(nameOfWebsitesIncluded))
    }
    if (is.null(corpusDtm) == TRUE) {
        if (quanteda::is.corpus(corpus)==TRUE) {
            corpusDtm <- quanteda::dfm(x = corpus, groups=c("date", "nameOfWebsite"))
            corpusDtm <- quanteda::weight(corpusDtm, "relFreq")
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
            corpusDtm <- quanteda::weight(corpusDtm, "relFreq")
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
        corpusDtmDic <- quanteda::applyDictionary(corpusDtm, termsDic)
        dailyFreq <- data.frame(docs = quanteda::docnames(corpusDtmDic), quanteda::as.data.frame(corpusDtmDic))
        dailyFreq <- tidyr::separate(data = dailyFreq, col = docs, into = c("Date","nameOfWebsite"), sep = "\\.")
        if (is.null(startDate)==FALSE) {
            dailyFreq <-dailyFreq[as.Date(dailyFreq$Date)>=as.Date(startDate),]
        }
        if (is.null(endDate)==FALSE) {
            dailyFreq <-dailyFreq[as.Date(dailyFreq$Date)<=as.Date(endDate),]
        }
    }
    if (length(terms)>1) {
        if (quanteda::is.dfm(corpusDtm)==TRUE) {
            dailyFreqL <- reshape2::melt(data = dailyFreq, id.vars = c("Date", "nameOfWebsite"), variable.name="Term", value.name="Frequency")
            dailyFreqL$Date <- as.Date(dailyFreqL$Date)
            dailyFreqAgg <- stats::aggregate(Frequency ~ Date + Term, dailyFreqL, mean)
            dailyFreq <- reshape2::dcast(data = dailyFreqAgg, Date ~ Term, value.var = "Frequency")
        } else {
            frequencyOfterms <- as.table(slam::rollup(corpusDtm[, terms], 1, time))
        }
    } else {
        if (quanteda::is.dfm(corpusDtm)==TRUE) {
            dailyFreqL <- reshape2::melt(data = dailyFreq, id.vars = c("Date", "nameOfWebsite"), variable.name="Term", value.name="Frequency")
            dailyFreqL$Date <- as.Date(dailyFreqL$Date)
            dailyFreqAgg <- stats::aggregate(Frequency ~ Date + nameOfWebsite, dailyFreqL, mean)
            dailyFreq <- reshape2::dcast(data = dailyFreqAgg, Date ~ nameOfWebsite, value.var = "Frequency")
        } else {
            frequencyOfterms <- as.table(tapply(as.numeric(as.matrix(corpusDtm[, terms])), list(time, nameOfWebsitesIncluded), sum))
        }
    }
    # to filter specific websites
    if (is.null(specificWebsites) == FALSE) {
        frequencyOfterms <- as.table(frequencyOfterms[, specificWebsites])
    }
    if (quanteda::is.dfm(corpusDtm)==TRUE) {
        dailyFreqZoo <- zoo::zoo(dailyFreq[2:length(dailyFreq)], dailyFreq$Date)
    } else {
        names(dimnames(frequencyOfterms)) <- NULL
        termSeries <- zoo::zoo(frequencyOfterms/c(tapply(slam::row_sums(corpusDtm), time, sum)), order.by = as.POSIXct(rownames(frequencyOfterms)))
        dailyFreqZoo <- merge(termSeries, zoo::zoo(, seq(start(termSeries), end(termSeries), "DSTday")), fill = NaN)
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
        ggplot2::geom_line(size = 1)
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
    if (is.null(nameOfWebsite) == FALSE) {
        timeSeries <- timeSeries + ggplot2::ggtitle(paste("References to", paste(dQuote(terms), collapse = ", "), "in", nameOfWebsite))
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
            ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".svg")))
            print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste("timeseries", nameOfProject, nameOfWebsite, paste(terms, collapse = " - "), sep = " - "), ".svg"))))
        } else if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == TRUE) {
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", nameOfProject, paste(terms, collapse = " - "), sep = " - "), ".svg"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".png")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".pdf")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".svg")))
            print(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = " - "), sep = " - "), ".svg"))))
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
#' @param specificWebsites Character vector indicating which websites (defined by relative nameOfWebsite) have to be included in graph. If left to default, includes all websites present in the dataset.
#' @param rollingAverage Integer, defaults to 30. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param export Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the correspondent "Outputs" subfolder. 
#' @param method Accepted values: "numberOfArticles" (default, creates time series based on number of publications per day); "numberOfCharacters" (creates time series based on number of charachters per day, currently does not work in conjunction with specificWebsites option). 
#' @return A ggplot2 time series showing number of articles published each day. 
#' @export
#' @examples
#' ShowDistribution(dataset)

ShowDistribution <- function(dataset, specificWebsites = NULL, rollingAverage = 30, align = "center", nameOfProject = NULL, nameOfWebsite = NULL, method = "numberOfArticles") {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    tab <- base::table(dataset$dates, dataset$nameOfWebsite)
    dates <- base::as.POSIXct(base::rownames(tab))
    if (method == "numberOfArticles") {
        if (base::is.null(specificWebsites) == FALSE) {
            docSeries <- zoo::zoo(tab[,specificWebsites], order.by=dates)
        } else {
            docSeries <- zoo::zoo(tab, order.by=dates)
        }
    } else if (method == "numberOfCharacters") {
        numberOfArticles <- length(dataset$articlesTxt)
        ncharArticles <- data.frame(date = rep(NA, numberOfArticles), nchar = rep(NA, numberOfArticles))
        for (i in 1:numberOfArticles) {
            ncharArticles$nchar[i] <- nchar(dataset$articlesTxt[i])
            ncharArticles$date[i] <- dataset$dates[i]
        }
        ncharPerDay <- plyr::ddply(ncharArticles,~date,plyr::summarise,ncharPerDay=sum(nchar))
        docSeries <- zoo::zoo(ncharPerDay$ncharPerDay, order.by = dates)
    }
    docSeries <- base::merge(docSeries, zoo::zoo(, seq(start(docSeries), end(docSeries), "DSTday")), fill=0)
    docSeries <- zoo::rollapply(docSeries, rollingAverage, align=align, mean, na.rm=TRUE)
    distributionOfCorpus <- zoo::autoplot.zoo(docSeries, facets = NULL)
    if (method == "numberOfArticles") {
        distributionOfCorpus <- distributionOfCorpus + ggplot2::ggtitle(paste0("Number of publications per day on ", dataset$nameOfWebsite[1], "'s website")) + ggplot2::scale_x_datetime("") + ggplot2::scale_y_continuous("")
    } else if (method == "numberOfCharacters") {
        distributionOfCorpus <- distributionOfCorpus + ggplot2::scale_y_continuous("") + ggplot2::ggtitle(paste0("Number of characters per day on ", dataset$nameOfWebsite[1], "'s website" )) + ggplot2::scale_x_datetime("") + ggplot2::scale_y_continuous("")
    }
    distributionOfCorpus
}