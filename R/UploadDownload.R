#' Uploads a set of datasets from existings 'castarter' projects to an online repository.
#' 
#' Uploads a set of datasets from existings 'castarter' projects to an online repository.
#'  
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "nameOfProject/nameOfWebsite".
#' @return A data frame including all loaded datasets.
#' @export
#' @examples
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' UploadDataset(projectsAndWebsites)
UploadDatasets <- function(projectsAndWebsites, removeNAdates = TRUE) {
    projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    lastSavedDatasets <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        nameOfProject <- projectsAndWebsites[[i]][1]
        nameOfWebsite <- projectsAndWebsites[[i]][2]
        datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(nameOfProject, nameOfWebsite, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    for (i in 1:length(lastSavedDatasets)) {
        RCurl::ftpUpload(what = lastSavedDatasets[1], to = "", userpwd = "")
        
    }
    allDatasets <- data.frame()
    for (i in 1:length(lastSavedDatasets)) {
        load(lastSavedDatasets[i])
        allDatasets <- rbind(allDatasets, dataset)
        rm(dataset)
    }
    if (removeNAdates == TRUE) {
        allDatasets <- allDatasets[is.na(allDatasets$dates) == FALSE, ]
    }
    allDatasets
}