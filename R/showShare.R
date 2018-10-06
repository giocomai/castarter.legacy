#' Shows share of articles including reference to a specific terms
#'
#' Shows share of articles including reference to a specific terms.Ignores article that don't have a date in the metadata (date=NA).
#'
#' @param dataset A dataset created with 'castarter'.
#' @param terms The pattern to be found in the articles.
#' @param breaks Defaults to "year". Parameter is passed to base::cut, and specifies how the dataset should be divided temporally. Available options include "day", "week", "month", "quarter" and "year", but can also be a vector of dates of the POSIXct class.
#' @param output Defaults to "graph". "data.frame" is currently the only alternative option available.
#' @param startDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param export Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If project and website are provided, in saves the timeseries in the correspondent "Outputs" subfolder.
#' @return A ggplot2 barchart showing shares of articles including reference to a specific terms.
#' @export
#' @examples
#' \dontrun{
#' ShowShare(dataset, breaks = "months")
#' }

ShowShare <- function(dataset,
                      terms,
                      breaks = "year",
                      output = "graph",
                      startDate = NULL,
                      endDate = NULL,
                      customTitle = NULL,
                      export = FALSE,
                      project = NULL,
                      website = NULL) {
    # If `project` and `website` not given, tries to get them from environment
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
    showShareDF <- dataset %>%
        dplyr::filter(is.na(date)==FALSE) %>%
        dplyr::mutate(Date = as.Date(date)) %>%
        dplyr::select(id, Date, contents) %>%
        dplyr::arrange(Date) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(Mentions = stringr::str_detect(string = contents, pattern = stringr::fixed(terms, ignore_case = TRUE))) %>%
        dplyr::select(-contents) %>%
        dplyr::group_by(Date) %>%
        dplyr::mutate(nArt = dplyr::n(), nArtMentions = sum(Mentions)) %>%
        dplyr::select(-Mentions, -id) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        dplyr::mutate(Date = cut(Date, breaks = breaks)) %>%
        dplyr::group_by(Date) %>%
        dplyr::mutate(nPerTimeUnit = sum(nArt), nMentionsPerTimeUnit = sum(nArtMentions, na.rm = TRUE)) %>%
        dplyr::select(Date, nPerTimeUnit, nMentionsPerTimeUnit) %>%
        dplyr::mutate(Share = nMentionsPerTimeUnit/nPerTimeUnit) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        dplyr::arrange(Date)
    if (output == "graph") {
        graph <-
            showShareDF %>%
            dplyr::mutate(negative = 1-Share) %>%
            tidyr::gather(c(Share, negative), key = Mentions, value = Share) %>%
            dplyr::mutate(Mentions = gsub(pattern = "Share", replacement = paste("Mentions", sQuote(terms)), x = Mentions)) %>%
            dplyr::mutate(Mentions = gsub(pattern = "negative", replacement = paste("Does not mention", sQuote(terms)), x = Mentions)) %>%
            ggplot2::ggplot(mapping = ggplot2::aes(x=Date, y = Share, fill = Mentions)) + ggplot2::geom_col() + ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) + ggplot2::scale_y_continuous(name = "", labels = scales::percent) + ggplot2::labs(title = paste("Share of articles including reference to", sQuote(x = terms)), fill="") +
            if(breaks == "year") {
                ggplot2::scale_x_discrete(name = "", labels = lubridate::year(x = showShareDF$Date))
            } else if (breaks == "month") {
                ggplot2::scale_x_discrete(name = "", labels = paste(month.abb[lubridate::month(showShareDF$Date)], lubridate::year(x = showShareDF$Date)))
            } else {
                ggplot2::scale_x_discrete(name = "")
            }
        if (is.null(customTitle)==FALSE) {
            graph <- graph + ggplot2::ggtitle(label = customTitle)
        }
        if (export == TRUE) {
            graph
            if (is.null(project) == FALSE & is.null(website) == FALSE) {
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, paste(terms, collapse = "-"), sep = "-"), ".png")))
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, paste(terms, collapse = "-"), sep = "-"), ".png"))))
                ggplot2::ggsave(file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, paste(terms, collapse = "-"), sep = "-"), ".pdf")))
                message(paste("File saved in", file.path(baseFolder, project, website, "Outputs", paste0(paste("timeseries", project, website, paste(terms, collapse = "-"), sep = "-"), ".pdf"))))
            } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, paste(terms, collapse = "-"), sep = "-"), ".png")))
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, paste(terms, collapse = "-"), sep = "-"), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", project, paste(terms, collapse = "-"), sep = "-"), ".pdf")))
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", project, paste(terms, collapse = "-"), sep = "-"), ".pdf"))))
            } else {
                if (!file.exists(file.path("Outputs"))) {
                    dir.create(file.path("Outputs"))
                }
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = "-"), sep = "-"), ".png")))
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = "-"), sep = "-"), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = "-"), sep = "-"), ".pdf")))
                message(paste("File saved in", file.path("Outputs", paste0(paste("timeseries", paste(terms, collapse = "-"), sep = "-"), ".pdf"))))
            }
        }
        graph
    } else {
        showShareDF
    }
}

