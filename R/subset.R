#' Subsets a 'castarter' dataset
#'
#' @param dataset A dataset created with 'castarter'.
#' @param terms A character vector. Only articles including one of these terms are included in the output.
#' @param startDate, endDate A date in the format YYYY-MM-DD.
#' @return A dataset in the 'castarter' format.
#' @export
#' @examples
#' \dontrun{
#' SubsetDataset(dataset, terms)
#' }

SubsetDataset <- function(dataset, terms = NULL, startDate = NULL, endDate = NULL, sample = NULL) {
    if (is.null(terms) == FALSE) {
        terms <- base::paste(terms,collapse="|")
        dataset <- dataset[base::grep(terms, dataset$contents, ignore.case = TRUE), ]
    }
    if (is.null(startDate) == FALSE) {
        dataset <- dataset[dataset$date > as.POSIXct(startDate), ]
    }
    if (is.null(endDate) == FALSE) {
        dataset <- dataset[dataset$date < as.POSIXct(endDate), ]
    }
    if (is.null(sample) == FALSE) {
        dataset <- dataset[base::sample.int(n = dim(dataset)[1], size = sample),]
    }
    dataset
}

