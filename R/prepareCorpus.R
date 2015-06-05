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

#' A function that converts a dataset created by 'castarter' into a corpus using the 'tm' package.Metadata are automatically imported.
#'
#' @keywords tm
#' @export
#' @examples
#' ConvertToCorpus(dataset)
#' 

ConvertToCorpus <- function(allDatasets) {
    corpus <- VCorpus(VectorSource(allDatasets$articlesTxt))
    for (i in 1:length(allDatasets$articlesTxt)) {
        meta(corpus[[i]], tag = "author") <- allDatasets$nameOfWebsite[i]
        meta(corpus[[i]], tag = "datetimestamp") <- allDatasets$dates[i]
        meta(corpus[[i]], tag = "heading") <- allDatasets$titles[i]
        meta(corpus[[i]], tag = "id") <- allDatasets$articlesId[i]
        meta(corpus[[i]], tag = "language") <- allDatasets$language[i]
        meta(corpus[[i]], tag = "origin") <- allDatasets$articlesLinks[i]
    }
    corpus
} 
