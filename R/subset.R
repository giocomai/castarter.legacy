
DivideByWebsite <- function(corpus, nameOfProject) {
    listOfWebsites <- gsub(paste0(nameOfProject, "/"), "", list.dirs(file.path(nameOfProject), recursive = FALSE), fixed = TRUE)
    byWebsite <- data.frame(matrix(NA, nrow = length(corpus), ncol = length(listOfWebsites)))
    names(byWebsite) <- listOfWebsites
    for (i in 1:length(listOfWebsites)) {
        byWebsite[, i] <- meta(corpus, "author") == listOfWebsites[i]
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
