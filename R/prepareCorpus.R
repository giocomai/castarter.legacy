LoadAllDatasets <- function(nameOfProject) {
    listOfWebsites <- gsub(paste0(nameOfProject, "/"), "", list.dirs(file.path(nameOfProject), recursive = FALSE), fixed = TRUE)
    lastSavedDatasets <- vector()
    for (i in 1:length(listOfWebsites)) {
        nameOfWebsite <- listOfWebsites[i]
        datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[str_extract(list.files(file.path(nameOfProject, 
            nameOfWebsite, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(nameOfProject, nameOfWebsite, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    allDatasets <- data.frame()
    for (i in 1:length(lastSavedDatasets)) {
        load(lastSavedDatasets[i])
        allDatasets <- rbind(allDatasets, dataset)
        rm(dataset)
    }
    # allDatasets$nameOfProject <- as.factor(allDatasets$nameOfProject) allDatasets$nameOfWebsite <- as.factor(allDatasets$nameOfWebsite)
    allDatasets
}

#' Converts a 'castarter' dataset into a 'tm' corpus.
#' 
#' Takes a dataset created with 'castarter' and converts it into a 'tm' corpus. Metadata are automatically imported.
#'  
#' @param dataset A data.frame created by 'castarter' including metadata and full text articles to be converted into a corpus. 
#' @return A corpus as created by the 'tm' package including metadata. 
#' @keywords tm
#' @export
#' @examples
#' corpus <- ConvertToCorpus(dataset)
#' 

ConvertToCorpus <- function(dataset) {
    corpus <- VCorpus(VectorSource(dataset$articlesTxt))
    for (i in 1:length(dataset$articlesTxt)) {
        meta(corpus[[i]], tag = "author") <- dataset$nameOfWebsite[i]
        meta(corpus[[i]], tag = "datetimestamp") <- dataset$dates[i]
        meta(corpus[[i]], tag = "heading") <- dataset$titles[i]
        meta(corpus[[i]], tag = "id") <- dataset$articlesId[i]
        meta(corpus[[i]], tag = "language") <- dataset$language[i]
        meta(corpus[[i]], tag = "origin") <- dataset$articlesLinks[i]
    }
    corpus
} 
