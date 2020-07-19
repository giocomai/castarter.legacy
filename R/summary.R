#' Summarise key data about a dataset in a data.frame
#'
#' Summarise key data about a dataset in a data.frame
#'
#' @param dataset A dataset created with 'castarter'.
#' @return A data.frame with summary statistics.
#' @export
#' @examples
#' \dontrun{
#' SummariseDataset(dataset)
#' }

SummariseDataset <- function(dataset) {
    websites <- levels(as.factor(dataset$website))
    datasetSummary <- tibble::tibble(Website = websites,
                                     From = as.Date(rep(NA, length(websites))),
                                     Until = as.Date(rep(NA, length(websites))),
                                     Total = as.numeric(NA),
                                     `Average daily` = as.numeric(NA))
    for (i in seq_along(websites)) {
        datasetSummary$From[i] <- min(dataset$date[dataset$website==websites[i]])
        datasetSummary$Until[i] <- max(dataset$date[dataset$website==websites[i]])
        datasetSummary$Total[i] <- nrow(dataset[dataset$website==websites[i],])
        datasetSummary$`Average daily`[i] <- datasetSummary$Total[i]/as.numeric(diff.Date(x = c(datasetSummary$From[i], datasetSummary$Until[i])))
    }
    return(datasetSummary)
}

