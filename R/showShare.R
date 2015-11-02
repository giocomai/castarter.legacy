#' Shows share of articles including reference to a specific terms 
#' 
#' Shows share of articles including reference to a specific terms.Ignores article that don't have a date in the metadata (dates=NA).
#'  
#' @param dataset A dataset created with 'castarter'.
#' @param terms The specific terms to be analysed. 
#' @param breaks Deafaults to "years". Parameter is passed to base::cut, and specifies how the dataset should be divided temporally. Available options include "years", "quarters", "monhts", but can also be a vector of dates of the POSIXct class.
#' @param startDate, endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param export Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the timeseries in the correspondent "Outputs" subfolder. 
#' @return A ggplot2 barchart showing shares of articles including reference to a specific terms.
#' @export
#' @examples
#' ShowShare(dataset, breaks = "months", nameOfProject, nameOfWebsite)

ShowShare <- function(dataset, terms, breaks = "years", startDate = NULL, export = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (length(terms) > 1) {
        terms <- paste(tolower(terms), collapse = "\\b|")
    }
    DTdataset <- data.table::as.data.table(dataset)
    DTdataset$dates <- base::cut(DTdataset$dates, breaks = breaks)
    DTdataset <- DTdataset[base::is.na(DTdataset$dates)==FALSE]
    DTdatasetFreq <- DTdataset[, .N, by=.(DTdataset$dates)]
    DTdatasetFreq <- stats::setNames(DTdatasetFreq, c("dates", "N"))
    DTterms <- DTdataset[base::regexpr(terms, DTdataset$articlesTxt, ignore.case = TRUE)>0, .N, by=.(dates)]
    data.table::setkey(DTdatasetFreq, dates)
    DTbyBreaks <- merge(DTdatasetFreq, DTterms, all = TRUE)
    DTbyBreaks$N.y[is.na(DTbyBreaks$N.y)==TRUE] <- 0
    DTbyBreaks$N.x <- DTbyBreaks$N.x-DTbyBreaks$N.y
    if (base::is.null(startDate) == FALSE) {
        DTbyBreaks <- DTbyBreaks[base::as.POSIXct(dates)>base::as.POSIXct(startDate)]
    }
    DTbyBreaks <- reshape2::melt(DTbyBreaks, id.vars = "dates")
    bm <- base::as.data.frame(DTbyBreaks, stringsAsFactors = FALSE)
    if (base::is.null(startDate) == FALSE) {
        bm <- bm[as.POSIXct(bm$dates)>as.POSIXct(startDate),]
    }
    bm$variable <- as.character(bm$variable)
    bm$variable[bm$variable=="N.x"] <- base::paste("Does not mention", base::sQuote(terms))
    bm$variable[bm$variable=="N.y"] <- base::paste("Mentions", base::sQuote(terms))
    bm$variable <- base::as.factor(bm$variable)
    bm$variable <- base::factor(bm$variable, levels=levels(bm$variable)[order(bm$variable, decreasing = TRUE)])
    bm <- bm[order(bm$variable, decreasing = TRUE), ]
    graph <- ggplot2::ggplot(bm,ggplot2::aes(x = dates, y = value, fill = variable)) + 
        ggplot2::scale_fill_discrete("") +
        ggplot2::geom_bar(position = "fill",stat = "identity") + 
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::ggtitle(paste("Share of articles including reference to", base::sQuote(terms))) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.4), angle = 90, hjust = 1, vjust = 0.5), 
                       plot.title = ggplot2::element_text(size = ggplot2::rel(1.2)), legend.text = ggplot2::element_text(size = ggplot2::rel(1)))
    if (breaks == "years") {
        graph <- graph + ggplot2::scale_x_discrete(labels = lubridate::year(DTterms$dates)[order(lubridate::year(DTterms$dates))])
    }
    if (breaks == "months") {
        graph <- graph + ggplot2::scale_x_discrete(labels = paste(lubridate::month(DTterms$dates, label = TRUE, abbr = TRUE)[order(lubridate::month(DTterms$dates)), ], lubridate::year(DTterms$dates)[order(lubridate::year(DTterms$dates))], sep = " "))
    }
    if (export == TRUE) {
        graph
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
    graph
}

