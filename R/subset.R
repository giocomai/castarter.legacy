#' Subsets a 'castarter' dataset
#' 
#' @param dataset A dataset created with 'castarter'.
#' @param terms A character vector. Only articles including these one of these terms are included in the output.
#' @param startDate, endDate A date in the format YYYY-MM-DD.
#' @return A dataset in the 'castarter' format. 
#' @export
#' @examples
#' SubsetDataset(dataset, terms)

SubsetDataset <- function(dataset, terms = NULL, startDate = NULL, endDate = NULL) {
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
    listOfWebsites <- as.character(unlist(NLP::meta(corpus, "author")))
    # listOfWebsites <- gsub(paste0(nameOfProject, "/"), "", list.dirs(file.path(nameOfProject), recursive = FALSE), fixed = TRUE)
    byWebsite <- data.frame(matrix(NA, nrow = length(corpus), ncol = length(listOfWebsites)))
    names(byWebsite) <- listOfWebsites
    for (i in 1:length(listOfWebsites)) {
        byWebsite[, i] <- NLP::meta(corpus, "author") == listOfWebsites[i]
    }
    byWebsite
}

# SubsetByDate <- function(corpus) { listOfDates <- seq(start(docSeries), end(docSeries), 'DSTday') byDate <- data.frame(matrix(NA, nrow =
# length(corpus), ncol = length(listOfDates))) names(byDate) <- listOfDates for (i in 1:length(listOfDates)) { byDate[, i] <- meta(corpus,
# 'datetimestamp') == listOfDates[i] } byDate }

IncludeOnly <- function(corpus, keepOnly, removeCharacterOrWord = "") {
    Include <- content_transformer(function(x, pattern) regmatches(x, gregexpr(pattern, x, perl = TRUE)))
    keepOnly <- gsub("[[:punct:]]+", "", keepOnly)
    keepOnly <- gsub("[[:space:]]+", "", keepOnly)
    keepOnly <- paste(keepOnly, collapse = "|")
    corpus <- tm_map(corpus, Include, keepOnly)
    corpus <- CleanCorpus(corpus, removeCharacter = removeCharacterOrWord)
    corpus
} 
