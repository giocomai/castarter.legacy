#' Loads a specific set of datasets from existings 'castarter' projects.
#' 
#' Takes a specific set of datasets from existings 'castarter' projects.
#'  
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "project/website". If none is given, it will check if project and website have been set with `SetCastarter()`
#' @param type Defines the format in which the dataset will be loaded. Available options are:
##' \itemize{
##'  \item{"dataset"}{: Outputs a 'castarter' dataset as data.frame. This is the default option.}
##'  \item{"corpus"}{: Outputs a corpus.}
##' }
#' @param removeNAdates Logical, defaults to TRUE. If TRUE, dataset items that do not have a date in the records are not imported.
#' @return A data frame including all loaded datasets.
#' @export
#' @examples
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' allDatasets <- LoadDatasets(projectsAndWebsites)
LoadDatasets <- function(projectsAndWebsites = NULL, type = "dataset", removeNAdates = TRUE) {
    if (gtools::invalid(projectsAndWebsites) == TRUE) {
        project <- CastarterOptions("project")
        website <- CastarterOptions("website")
        projectsAndWebsites <- list(projectsAndWebsites = c(project, website))
    } else {
        projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    }
    lastSavedDatasets <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        project <- projectsAndWebsites[[i]][1]
        website <- projectsAndWebsites[[i]][2]
        if (type == "corpusQ") {
            datasetFilename <- sort(list.files(file.path(project, website, "Dataset"))[stringr::str_extract(list.files(file.path(project, website, "Dataset")), "corpusQ.RData") == "corpusQ.RData"], decreasing = TRUE)[1]
        } else if (type == "dataset") {
            datasetFilename <- sort(list.files(file.path(project, website, "Dataset"))[stringr::str_extract(list.files(file.path(project, website, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        } else if (type == "corpusDtmQ") {
            datasetFilename <- sort(list.files(file.path(project, website, "Dataset"))[stringr::str_extract(list.files(file.path(project, website, "Dataset")), "corpusDtmQ.RData") == "corpusDtmQ.RData"], decreasing = TRUE)[1]
        } else {
            stop("Type can be 'dataset', 'corpusQ', 'corpusTM, or 'corpusDtmQ'")
        }
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(project, website, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    if (type == "corpusQ") {
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
            # introduced for backward compatibility with datasets created with castarter ver<0.2
            colnames(x = dataset)[colnames(dataset)=="nameOfProject"]<-"project"
            colnames(x = dataset)[colnames(dataset)=="nameOfWebsite"]<-"website"
            allDatasets <- rbind(allDatasets, dataset)
            rm(dataset)
        }
        if (removeNAdates == TRUE) {
            allDatasets <- allDatasets[is.na(allDatasets$dates) == FALSE, ]
        }
    } else if (type == "corpusDtmQ") {
        load(lastSavedDatasets[i])
    }
    if (type == "corpusQ") {
        if (length(lastSavedDatasets)==1) {
            return(corpusTemp)
        } else {
            return(corpusAll)
        }
    } else if (type == "dataset") {
        return(allDatasets)
    } else if (type == "corpusDtmQ") {
        load(lastSavedDatasets)
        return(corpusDtm)
    }
}


#' Loads all datasets of a 'castarter' project.
#' 
#' Takes all datasets from all websites in a project and outputs them in a data frame.
#'  
#' @param project The name of the project whose datasets are to be imported.
#' @return A data frame including all datasets of a project.
#' @export
#' @examples
#' allDatasets <- LoadAllDatasets(project)

LoadAllDatasets <- function(project, removeNAdates = TRUE) {
    listOfWebsites <- gsub(paste0(project, "/"), "", list.dirs(file.path(project), recursive = FALSE), fixed = TRUE)
    lastSavedDatasets <- vector()
    for (i in 1:length(listOfWebsites)) {
        website <- listOfWebsites[i]
        datasetFilename <- sort(list.files(file.path(project, website, "Dataset"))[stringr::str_extract(list.files(file.path(project, 
            website, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(project, website, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    allDatasets <- data.frame()
    for (i in 1:length(lastSavedDatasets)) {
        load(lastSavedDatasets[i])
        # introduced for backward compatibility with datasets created with castarter ver<0.2
        colnames(x = dataset)[colnames(dataset)=="nameOfProject"]<-"project"
        colnames(x = dataset)[colnames(dataset)=="nameOfWebsite"]<-"website"
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
#' @param quanteda Logical, defaults to TRUE. If TRUE generates corpus of the 'quanteda' package, otherwise of the 'tm' package. 
#' @return A corpus as created by the 'tm' or 'quanteda' package including metadata. 
#' @export
#' @examples
#' corpus <- CreateCorpus(dataset)
CreateCorpus <- function(dataset, quanteda = TRUE) {
    if (quanteda==TRUE) {
        corpus <- quanteda::corpus(dataset$articlesTxt, docnames = paste(as.Date(dataset$dates), dataset$articlesId, dataset$titles, sep = " - "), 
                                   docvars=data.frame(website=dataset$website, date=as.Date(dataset$dates), title=dataset$titles, links = dataset$articlesLinks, ID=dataset$articlesId))
    } else {
        corpus <- tm::VCorpus(tm::VectorSource(dataset$articlesTxt))
        for (i in 1:length(dataset$articlesTxt)) {
            NLP::meta(corpus[[i]], tag = "author") <- dataset$website[i]
            NLP::meta(corpus[[i]], tag = "datetimestamp") <- dataset$dates[i]
            NLP::meta(corpus[[i]], tag = "heading") <- dataset$titles[i]
            NLP::meta(corpus[[i]], tag = "id") <- dataset$articlesId[i]
            NLP::meta(corpus[[i]], tag = "language") <- dataset$language[i]
            NLP::meta(corpus[[i]], tag = "origin") <- dataset$articlesLinks[i]
        }
    }
    corpus
} 
