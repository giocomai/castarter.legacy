#' Subsets a 'castarter' dataset
#' 
#' @param dataset A dataset created with 'castarter'.
#' @param terms A character vector. Only articles including one of these terms are included in the output.
#' @param startDate, endDate A date in the format YYYY-MM-DD.
#' @return A dataset in the 'castarter' format. 
#' @export
#' @examples
#' SubsetDataset(dataset, terms)

SubsetDataset <- function(dataset, terms = NULL, startDate = NULL, endDate = NULL, sample = NULL) {
    if (is.null(terms) == FALSE) {
        terms <- base::paste(terms,collapse="|")
        dataset <- dataset[base::grep(terms, dataset$articlesTxt, ignore.case = TRUE), ]
    }
    if (is.null(startDate) == FALSE) {
        dataset <- dataset[dataset$dates > as.POSIXct(startDate), ]
    }
    if (is.null(endDate) == FALSE) {
        dataset <- dataset[dataset$dates < as.POSIXct(endDate), ]
    }
    if (is.null(sample) == FALSE) {
        dataset <- dataset[base::sample.int(n = dim(dataset)[1], size = sample),]
    }
    dataset
}

#' Divide corpus by website
#' 
#' @param corpus A corpus as created by the 'tm' package including metadata.
#' @return A data.frame
#' @export
#' @examples
#' DivideByWebsite(corpus, nameOfProject)
DivideByWebsite <- function(corpus, nameOfProject) {
    listOfWebsites <- levels(as.factor(unlist(NLP::meta(corpus, "author"))))
    byWebsite <- data.frame(matrix(NA, nrow = length(corpus), ncol = length(listOfWebsites)))
    names(byWebsite) <- listOfWebsites
    for (i in 1:length(listOfWebsites)) {
        byWebsite[, i] <- NLP::meta(corpus, "author") == listOfWebsites[i]
    }
    byWebsite
}