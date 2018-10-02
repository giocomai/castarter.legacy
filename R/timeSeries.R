#' Creates time series of the absolute frequency of specific terms.
#'
#' It creates time series with the absolute frequency of one or more terms, in one or more websites.
#' @param dataset A `castarter` dataset.
#' @param terms A character vector with one or more words to be analysed.
#' @param type Type of output: either "graph" (default) or "data.frame".
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param rollingAverage Integer, defaults to 31. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) around the given date. If align = "centre", and rollingAverage = 31, this means that the value for each day corresponds to the average value for the given day, the 15 days before it, and the 15 days after it.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param startDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param endDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param verticalLine Defaults to NULL. Draws a vertical dotted line at the date provided. If given, value must correspond to one or more dates in the YMD format, e.g. "2016-08-17".
#' @param smoothLine Logical, defaults to FALSE. If TRUE draws a smooth line over the time series.
#' @param customTitle A character vector, defaults to NULL. If provided, it overrides default graph title.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If project and website are provided, in saves the timeseries in the "Outputs" subfolder.
#' @export
#' @examples
#' ShowAbsoluteTS(terms = c("word1", "word2"), dataset)

ShowAbsoluteTS <- function(terms,
                           dataset,
                           type = "graph",
                           specificWebsites = NULL,
                           startDate = NULL,
                           endDate = NULL,
                           rollingAverage = 31,
                           align = "center",
                           verticalLine = NULL,
                           smoothLine = FALSE,
                           customTitle = NULL,
                           export = FALSE,
                           project = NULL,
                           website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (is.null(specificWebsites)==FALSE) {
        dataset <- dataset %>% dplyr::filter(website == paste(specificWebsites, collapse = "|"))
    }
    if (is.null(startDate)==FALSE) {
        dataset <- dataset %>% dplyr::filter(date >= as.Date(startDate))
    }
    if (is.null(endDate)==FALSE) {
        dataset <- dataset %>% dplyr::filter(date <= as.Date(endDate))
    }
    # Introduce compatibility with tidytext data frames
    if (is.element(el = "sentence", colnames(dataset))) {
        dataset <- dataset %>% dplyr::rename(text = sentence)
    } else if (is.element(el = "word", colnames(dataset))) {
        dataset <- dataset %>% dplyr::rename(text = word)
    }
    temp <-
        dplyr::bind_cols(dplyr::as_data_frame(sapply(terms, function(x) stringr::str_count(string = dataset$text, pattern = stringr::regex(x, ignore_case = TRUE)))),
                  tibble::data_frame(ItemDate = dataset$date)) %>%
        dplyr::arrange(ItemDate) %>%
        dplyr::full_join(tibble::tibble(ItemDate = seq.Date(from = min(dataset$date, na.rm = TRUE), to = max(dataset$date, na.rm = TRUE), by = "day")), by = "ItemDate") %>%
        dplyr::mutate_at(1:length(terms),dplyr::funs(dplyr::coalesce(., 0L)))  %>%
        tidyr::gather(word, n, 1:length(terms)) %>%
        dplyr::count(ItemDate, word, wt = n) %>%
        tidyr::spread(word, nn)
    for (i in 2:length(temp)) {
        temp[i] <- zoo::rollmean(x = temp[i], k = rollingAverage, align = align, fill = NA)
    }
    temp <- temp %>% tidyr::drop_na()
    if (type == "graph") {
        if (length(terms)>1) {
            graph <- temp %>% tidyr::gather(word, nRoll, 2:sum(length(terms),1)) %>%
                ggplot2::ggplot(mapping = ggplot2::aes(x = ItemDate, y = nRoll, color = word))
        } else {
            graph <- temp %>% tidyr::gather(word, nRoll, 2) %>%
                ggplot2::ggplot(mapping = ggplot2::aes(x = ItemDate, y = nRoll))
        }
        graph <- graph +
            ggplot2::geom_line(size = 1) +
            ggplot2::scale_y_continuous(name = "", labels = scales::comma) +
            ggplot2::scale_x_date(name = "") +
            ggplot2::expand_limits(y = 0) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.title=ggplot2::element_blank()) +
            ggplot2::scale_color_brewer(type = "qual", palette = 6) +
            if (is.null(website) == TRUE) {
                ggplot2::labs(title = paste("Number of occurrences per day of ", paste(sQuote(terms), collapse = ", ")),
                              caption = paste0("Calculated on a ", rollingAverage, "-days rolling average"))
            } else {
                ggplot2::labs(title = paste("Number of occurrences per day of", paste(sQuote(terms), collapse = ", "), "on", website),
                              caption = paste0("Calculated on a ", rollingAverage, "-days rolling average"))
            }
        if (length(terms)>1) {
            graph <- graph + ggplot2::labs(color = "Terms")
        }
        if (is.null(verticalLine)==FALSE) {
            graph <- graph + ggplot2::geom_vline(xintercept = c(as.numeric(as.Date(verticalLine))), linetype = 2)
        }
        if (is.null(customTitle) == FALSE) {
            graph <- graph + ggplot2::ggtitle(customTitle)
        }
        if (smoothLine == TRUE) {
            graph <- graph + ggplot2::stat_smooth(size=1.2, se=FALSE)
        }
        if (export == TRUE) {
            if (is.null(project) == FALSE & is.null(website) == FALSE) {
                if (file.exists(file.path(baseFolder, project, website, "Outputs")) == FALSE) {
                    dir.create(file.path(baseFolder, project, website, "Outputs"))
                }
                ggplot2::ggsave(filename = file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg"))), plot = graph)
            } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg"))))
            } else {
                if (!file.exists(file.path("Outputs"))) {
                    dir.create(file.path("Outputs"))
                }
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg"))))
            }
        }
        graph
    } else {
        temp %>% dplyr::rename(Date = ItemDate)
    }
}

#' Creates time series of the relative frequency of specific terms.
#'
#' It creates time series with the relative frequency of one or more terms, in one or more websites.
#' @param dataset A `castarter` dataset.
#' @param terms A character vector with one or more words to be analysed.
#' @param type Type of output: either "graph" (default) or "data.frame".
#' @param specificWebsites Character vector of the names of one or more websites included in the corpus. Only selected websites will be included in the analysis.
#' @param rollingAverage Integer, defaults to 31. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) around the given date. If align = "centre", and rollingAverage = 31, this means that the value for each day corresponds to the average value for the given day, the 15 days before it, and the 15 days after it.
#' @param align Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.
#' @param startDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param endDate Character vector with date in the format year-month-date, e.g. "2015-07-14". Given dates are included, e.g. if you wish to include all of 2015, set startDate="2015-01-01", endDate"2015-12-31")
#' @param verticalLine Defaults to NULL. Draws a vertical dotted line at the date provided. If given, value must correspond to one or more dates in the YMD format, e.g. "2016-08-17".
#' @param smoothLine Logical, defaults to FALSE. If TRUE draws a smooth line over the time series.
#' @param customTitle A character vector, defaults to NULL. If provided, it overrides default graph title.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If project and website are provided, in saves the timeseries in the "Outputs" subfolder.
#' @export
#' @examples
#' ShowRelativeTS(terms = c("word1", "word2"), dataset)

ShowRelativeTS <- function(terms,
                           dataset,
                           type = "graph",
                           specificWebsites = NULL,
                           startDate = NULL,
                           endDate = NULL,
                           rollingAverage = 31,
                           align = "center",
                           verticalLine = NULL,
                           smoothLine = FALSE,
                           customTitle = NULL,
                           export = FALSE,
                           project = NULL,
                           website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(specificWebsites)==FALSE) {
        dataset <- dataset %>% dplyr::filter(website == paste(specificWebsites, collapse = "|"))
    }
    if (is.null(startDate)==FALSE) {
        dataset <- dataset %>% dplyr::filter(date >= as.Date(startDate))
    }
    if (is.null(endDate)==FALSE) {
        dataset <- dataset %>% dplyr::filter(date <= as.Date(endDate))
    }
    # Introduce compatibility with tidytext data frames
    if (is.element(el = "sentence", colnames(dataset))) {
        dataset <- dataset %>% dplyr::rename(text = sentence)
    } else if (is.element(el = "word", colnames(dataset))) {
        dataset <- dataset %>% dplyr::rename(text = word)
    }
    temp <-
        dplyr::bind_cols(dplyr::as_data_frame(sapply(terms, function(x) stringr::str_count(string = dataset$text, pattern = stringr::regex(x, ignore_case = TRUE)))),
                                tibble::data_frame(nWords = stringr::str_count(string = dataset$text, pattern = "\\w+")),
                  tibble::data_frame(ItemDate = dataset$date)) %>%
        dplyr::arrange(ItemDate) %>%
        # Count all words per item
        dplyr::add_count(ItemDate, wt = nWords) %>%
        dplyr::rename(TotalWords = n) %>%
        dplyr::group_by(ItemDate, TotalWords) %>%
        dplyr::select(-nWords) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::ungroup() %>%
        # Calculate relative frequency
        dplyr::mutate_at(3:sum(2, length(terms)), dplyr::funs(./TotalWords)) %>%
        dplyr::select(-TotalWords) %>%
        # Include missing date, if no item on a given date
        dplyr::full_join(tibble::tibble(ItemDate = seq.Date(from = min(dataset$date, na.rm = TRUE), to = max(dataset$date, na.rm = TRUE), by = "day")), by = "ItemDate") %>%
        dplyr::arrange(ItemDate) %>%
        # Substitute NA values with 0 for all dates for which no item was present
        dplyr::mutate_at(2:sum(1, length(terms)),dplyr::funs(dplyr::coalesce(., 0))) %>%
        tidyr::gather(word, n, 2:sum(1, length(terms))) %>%
        dplyr::count(ItemDate, word, wt = n) %>%
        tidyr::spread(word, nn)
    # Calculate rolling average
    for (i in 2:length(temp)) {
        temp[i] <- zoo::rollmean(x = temp[i], k = rollingAverage, align = align, fill = NA)
    }
    # Drop leading and trailing NA introduced by rolling average to prevent ggplot warning
    temp <- temp %>% tidyr::drop_na()
    if (type == "graph") {
        if (length(terms)>1) {
            graph <- temp %>% tidyr::gather(word, nRoll, 2:sum(length(terms),1)) %>%
                ggplot2::ggplot(mapping = ggplot2::aes(x = ItemDate, y = nRoll, color = word))
        } else {
            graph <- temp %>% tidyr::gather(word, nRoll, 2) %>%
                ggplot2::ggplot(mapping = ggplot2::aes(x = ItemDate, y = nRoll))
        }
        graph <- graph +
            ggplot2::geom_line(size = 1) +
            ggplot2::scale_y_continuous(name = "", labels = scales::number_format(accuracy = NULL)) +
            ggplot2::scale_x_date(name = "") +
            ggplot2::expand_limits(y = 0) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.title=ggplot2::element_blank()) +
            ggplot2::scale_color_brewer(type = "qual", palette = 6) +
            if (is.null(website) == TRUE) {
                ggplot2::labs(title = paste("Frequency of", paste(sQuote(terms), collapse = ", ")),
                              caption = paste0("Calculated on a ", rollingAverage, "-days rolling average"))
            } else {
                ggplot2::labs(title = paste("Frequency of", paste(sQuote(terms), collapse = ", "), "on", website),
                              caption = paste0("Calculated on a ", rollingAverage, "-days rolling average"))
            }
        if (length(terms)>1) {
            graph <- graph + ggplot2::labs(color = "Terms")
        }
        if (is.null(verticalLine)==FALSE) {
            graph <- graph + ggplot2::geom_vline(xintercept = c(as.numeric(as.Date(verticalLine))), linetype = 2)
        }
        if (is.null(customTitle) == FALSE) {
            graph <- graph + ggplot2::ggtitle(customTitle)
        }
        if (smoothLine == TRUE) {
            graph <- graph + ggplot2::stat_smooth(size=1.2, se=FALSE)
        }
        if (export == TRUE) {
            if (is.null(project) == FALSE & is.null(website) == FALSE) {
                if (file.exists(file.path(baseFolder, project, website, "Outputs")) == FALSE) {
                    dir.create(file.path(baseFolder, project, website, "Outputs"))
                }
                ggplot2::ggsave(filename = file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, sep = " - "), ".svg"))), plot = graph)
            } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, sep = " - "), ".svg"))))
            } else {
                if (!file.exists(file.path("Outputs"))) {
                    dir.create(file.path("Outputs"))
                }
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg")), plot = graph)
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", sep = " - "), ".svg"))))
            }
        }
        graph
    } else {
        temp %>% dplyr::rename(Date = ItemDate)
    }
}

#' Creates a time series graph showing the distribution of documents by date
#'
#' Creates a time series graph showing the distribution of documents by date.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param specificWebsites Character vector indicating which websites (defined by relative website) have to be included in graph. If left to default, includes all websites present in the dataset.
#' @param rollingAverage Integer, defaults to 31. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingAverage) following the correspondent date.
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

ShowDistribution <- function(dataset, specificWebsites = NULL, rollingAverage = 31, align = "center", customTitle = NULL, method = "numberOfArticles", export = FALSE, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
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
        numberOfArticles <- length(dataset$text)
        ncharArticles <- data.frame(date = rep(NA, numberOfArticles), nchar = rep(NA, numberOfArticles))
        for (i in 1:numberOfArticles) {
            ncharArticles$nchar[i] <- nchar(dataset$text[i])
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
            if (file.exists(file.path(baseFolder, project, website, "Outputs")) == FALSE) {
                dir.create(file.path(baseFolder, project, website, "Outputs"))
            }
            ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".png")))
            message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".pdf")))
            message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".svg")))
            message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("distributionOfCorpus", project, website, sep = " - "), ".svg"))))
        } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".png")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".pdf")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".svg")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", project, sep = " - "), ".svg"))))
        } else {
            if (!file.exists(file.path("Outputs"))) {
                dir.create(file.path("Outputs"))
            }
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".png")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".png"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".pdf")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".pdf"))))
            ggplot2::ggsave(file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".svg")))
            message(paste("File saved in", file.path("Outputs", paste0(paste("distributionOfCorpus", sep = " - "), ".svg"))))
        }
    }
    distributionOfCorpus
}
