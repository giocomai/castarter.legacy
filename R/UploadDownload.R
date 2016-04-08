#' Uploads a set of datasets from existings 'castarter' projects to an online repository.
#' 
#' Uploads a set of datasets from existings 'castarter' projects to an online repository.
#'  
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "nameOfProject/nameOfWebsite".
#' @return Nothing. Used for its side effects (uploading datasets to an ftp server).
#' @export
#' @examples
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' UploadDataset(projectsAndWebsites, server = "myserver.com", user = "me@myserver.com", pwd = "secretPassword!")
UploadDatasets <- function(projectsAndWebsites, removeNAdates = TRUE, server, user, pwd) {
    projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    lastSavedDatasets <- vector()
    datasetFilenames <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        nameOfProject <- projectsAndWebsites[[i]][1]
        nameOfWebsite <- projectsAndWebsites[[i]][2]
        datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(nameOfProject, nameOfWebsite, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
            datasetFilenames[i] <- datasetFilename
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    opts <- list(ftp.create.missing.dirs=TRUE)
    for (i in 1:length(lastSavedDatasets)) {
        RCurl::ftpUpload(what = lastSavedDatasets[i],
                         to = paste0("ftp://", server, ":21/",
                                     paste(projectsAndWebsites[[i]][1], projectsAndWebsites[[i]][2], "Dataset", sep = "/"),
                                     "/", datasetFilenames[i]),
                         userpwd = paste(user, pwd, sep = ":"),
                         .opts=opts)
    }
    # if (removeNAdates == TRUE) {
    #     allDatasets <- allDatasets[is.na(allDatasets$dates) == FALSE, ]
    # }
}