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
UploadDatasets <- function(projectsAndWebsites, server, user, pwd, dataset = TRUE, corpusQ = TRUE, corpusDtmQ = TRUE) {
    projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    lastSavedDatasets <- vector()
    datasetFilenames <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        nameOfProject <- projectsAndWebsites[[i]][1]
        nameOfWebsite <- projectsAndWebsites[[i]][2]
        if (dataset == TRUE) {
            datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        }
        if (corpusQ == TRUE) {
            corpusQFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "corpusQ.RData") == "corpusQ.RData"], decreasing = TRUE)[1]
        }
        if (corpusDtmQ == TRUE) {
            corpusDtmQFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "corpusDtmQ.RData") == "corpusDtmQ.RData"], decreasing = TRUE)[1]
        }
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
}


#' Downloads a set of datasets from existings 'castarter' projects to an online repository.
#' 
#' Downloads a set of datasets from existings 'castarter' projects to an online repository.
#'  
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "nameOfProject/nameOfWebsite".
#' @return A 'castarter' dataset.
#' @export
#' @examples
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' DownloadDataset(projectsAndWebsites, server = "myserver.com", user = "me@myserver.com", pwd = "secretPassword!")
DownloadDatasets <- function(projectsAndWebsites, server, user, pwd) {
    datasetExport <- data.frame()
    for (i in 1:length(projectsAndWebsites)) {
        filenames <- RCurl::getURL(url = paste0("ftp://", server, ":21/", projectsAndWebsites[i], "/", "Dataset", "/"), userpwd = paste(user, pwd, sep = ":"),
                                   ftp.use.epsv = TRUE, dirlistonly = TRUE, verbose = FALSE)
        filenames <- unlist(strsplit(x = filenames, split = "\n", fixed = TRUE))
        filenames <- gsub(pattern = "\r", replacement = "", x = filenames)
        filenames <- filenames[filenames!="."&filenames!=".."]
        filenames <- gsub(pattern = " ", replacement = "%20", x = filenames)
        datasetBin <- RCurl::getBinaryURL(paste0("ftp://", server, ":21/", projectsAndWebsites[i], "/", "Dataset", "/", filenames[1]), userpwd = paste(user, pwd, sep = ":"), verbose = FALSE, ftp.use.epsv = TRUE)
        load(rawConnection(datasetBin))
        datasetExport <- rbind(datasetExport, dataset)
        print(x = paste("Dataset", projectsAndWebsites[i], "downloaded."))
    }
    datasetExport
}