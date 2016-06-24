#' Loads a specific set of datasets from existings 'castarter' projects.
#' 
#' Takes a specific set of datasets from existings 'castarter' projects.
#'  
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "nameOfProject/nameOfWebsite".
#' @param type Defines the format in which the dataset will be loaded. Available options are:
##' \itemize{
##'  \item{"dataset"}{: Outputs a 'castarter' dataset as data.frame. This is the default option.}
##'  \item{"corpus"}{: Outputs a corpus.}
##' }
#' @return A data frame including all loaded datasets.
#' @export
#' @examples
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' allDatasets <- LoadDatasets(projectsAndWebsites)
LoadDatasets <- function(projectsAndWebsites, type = "dataset", removeNAdates = FALSE) {
    projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    lastSavedDatasets <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        nameOfProject <- projectsAndWebsites[[i]][1]
        nameOfWebsite <- projectsAndWebsites[[i]][2]
        if (type == "corpus") {
            datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "corpus.RData") == "corpus.RData"], decreasing = TRUE)[1]
        } else if (type == "dataset") {
            datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        } else {
            stop("Type can be either 'dataset' or 'corpus'")
        }
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(nameOfProject, nameOfWebsite, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    if (type == "corpus") {
        for (i in 1:length(lastSavedDatasets)) {
            load(lastSavedDatasets[i])
            if (exists("corpusTemp") == TRUE) {
                corpusAll <- corpusTemp+corpus
                rm(corpusTemp)
            } else if (exists("corpusAll")) {
                corpusAll <- corpusAll+corpus
            } else {
                corpusTemp <- corpus
            }
            rm(corpus)
        }
    } else if (type == "dataset") {
        allDatasets <- data.frame()
        for (i in 1:length(lastSavedDatasets)) {
            load(lastSavedDatasets[i])
            allDatasets <- rbind(allDatasets, dataset)
            rm(dataset)
        }
    }
    if (removeNAdates == TRUE) {
        allDatasets <- allDatasets[is.na(allDatasets$dates) == FALSE, ]
    }
    if (type == "corpus") {
        return(corpusAll)
    } else if (type == "dataset") {
        return(allDatasets)
    }
}


#' Loads all datasets of a 'castarter' project.
#' 
#' Takes all datasets from all websites in a project and outputs them in a data frame.
#'  
#' @param nameOfProject The name of the project whose datasets are to be imported.
#' @return A data frame including all datasets of a project.
#' @export
#' @examples
#' allDatasets <- LoadAllDatasets(nameOfProject)

LoadAllDatasets <- function(nameOfProject, removeNAdates = TRUE) {
    listOfWebsites <- gsub(paste0(nameOfProject, "/"), "", list.dirs(file.path(nameOfProject), recursive = FALSE), fixed = TRUE)
    lastSavedDatasets <- vector()
    for (i in 1:length(listOfWebsites)) {
        nameOfWebsite <- listOfWebsites[i]
        datasetFilename <- sort(list.files(file.path(nameOfProject, nameOfWebsite, "Dataset"))[stringr::str_extract(list.files(file.path(nameOfProject, 
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
    if (removeNAdates == TRUE) {
        allDatasets <- allDatasets[is.na(allDatasets$dates) == FALSE, ]
    }
    allDatasets
}

#' Converts a 'castarter' dataset into a 'tm' corpus.
#' 
#' Takes a dataset created with 'castarter' and converts it into a 'tm' corpus. Metadata are automatically imported.
#'  
#' @param dataset A data.frame created by 'castarter' including metadata and full text articles to be converted into a corpus. 
#' @param Logical, defaults to FAlSE. If TRUE generates corpus of the 'quanteda' package, otherwise of the 'tm' package. 
#' @return A corpus as created by the 'tm' or 'quanteda' package including metadata. 
#' @export
#' @examples
#' corpus <- ConvertToCorpus(dataset)
ConvertToCorpus <- function(dataset, quanteda = FALSE) {
    if (quanteda==TRUE) {
        corpus <- quanteda::corpus(dataset$articlesTxt,
                                   docvars=data.frame(nameOfWebsite=dataset$nameOfWebsite, date=as.Date(dataset$dates), title=dataset$titles, links = dataset$articlesLinks, ID=dataset$articlesId))
    } else {
        corpus <- tm::VCorpus(tm::VectorSource(dataset$articlesTxt))
        for (i in 1:length(dataset$articlesTxt)) {
            NLP::meta(corpus[[i]], tag = "author") <- dataset$nameOfWebsite[i]
            NLP::meta(corpus[[i]], tag = "datetimestamp") <- dataset$dates[i]
            NLP::meta(corpus[[i]], tag = "heading") <- dataset$titles[i]
            NLP::meta(corpus[[i]], tag = "id") <- dataset$articlesId[i]
            NLP::meta(corpus[[i]], tag = "language") <- dataset$language[i]
            NLP::meta(corpus[[i]], tag = "origin") <- dataset$articlesLinks[i]
        }
    }
    corpus
} 
