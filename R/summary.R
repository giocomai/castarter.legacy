#' Summarise key data about a dataset in a data.frame
#'
#' Summarise key data about a dataset in a data.frame
#'
#' @param dataset A dataset created with 'castarter'.
#' @return A data.frame with summary statistics.
#' @export
#' @examples
#' SummariseDataset(dataset)

SummariseDataset <- function(dataset) {
    websites <- levels(as.factor(dataset$website))
    datasetSummary <- data.frame(website = websites, minDates = as.Date(rep(NA, length(websites))), maxDates = as.Date(rep(NA, length(websites))), totalPublications = as.numeric(NA), avgDailyPublications = as.numeric(NA))
    for (i in seq_along(websites)) {
        datasetSummary$minDates[i] <- min(dataset$dates[dataset$website==websites[i]])
        datasetSummary$maxDates[i] <- max(dataset$dates[dataset$website==websites[i]])
        datasetSummary$totalPublications[i] <- nrow(dataset[dataset$website==websites[i],])
        datasetSummary$avgDailyPublications[i] <- datasetSummary$totalPublications[i]/as.numeric(diff.Date(x = c(datasetSummary$minDates[i], datasetSummary$maxDates[i])))
    }
    return(datasetSummary)
}

