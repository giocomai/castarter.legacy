#' Loads a specific set of datasets from existings 'castarter' projects.
#'
#' Takes a specific set of datasets from existings 'castarter' projects.
#'
#' @param projectsAndWebsites A character vector listing websites to be loaded in the format "project/website". If none is given, it will check if project and website have been set with `SetCastarter()`
#' @param type Defines the format in which the dataset will be loaded. Available options are:
##' \itemize{
##'  \item{"dataset"}{: Outputs a standard `castarter` dataset as data frame (a tibble). This is the default option.}
##'  \item{"datasetTidy"}{: Outputs a 'castarter' dataset, with one word per row, in line with tidy principles. See also the `tidytext` package.}
##'  \item{"datasetBySentence"}{: Outputs a 'castarter' dataset, with one sentence per row. This can be used to speed up the loading time of datasets analysed through the `AnalyseDataset()` function.}
##' }
#' @param arrangeByDate Logical, defaults to TRUE. If TRUE, the dataset is put in order by date (oldest first).
#' @param removeNAdates Logical, defaults to TRUE. If TRUE, dataset items that do not have a date in the records are not imported.
#' @param convertToUTF8 Logical, defaults to FALSE. If TRUE, converts the `title` and `text` column (or correspondent, in `datasetTidy` and `datasetBySentence`) to UTF8.
#' @param addProjectColumn Logical, defatults to FALSE. If TRUE, a column with the project name is appended.
#' @return A data frame including all loaded datasets.
#' @export
#' @examples
#' \dontrun{
#' projectsAndWebsites <- c("ProjectX/Website1", "ProjectY/Website3", "ProjectZ/Website2")
#' allDatasets <- LoadDatasets(projectsAndWebsites)
#' }
LoadDatasets <- function(projectsAndWebsites = NULL,
                         type = "dataset",
                         arrangeByDate = TRUE,
                         removeNAdates = TRUE,
                         convertToUTF8 = FALSE,
                         addProjectColumn = FALSE) {
    if (is.null(projectsAndWebsites) == TRUE) {
        project <- CastarterOptions("project")
        website <- CastarterOptions("website")
        projectsAndWebsites <- list(projectsAndWebsites = c(project, website))
    } else {
        projectsAndWebsites <- base::strsplit(projectsAndWebsites, "/")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    lastSavedDatasets <- vector()
    for (i in 1:length(projectsAndWebsites)) {
        project <- projectsAndWebsites[[i]][1]
        website <- projectsAndWebsites[[i]][2]
        if (type == "dataset") {
            datasetFilename <- sort(list.files(file.path(baseFolder, project, website, "Dataset"))[stringr::str_extract(list.files(file.path(baseFolder, project, website, "Dataset")), "dataset.rds") == "dataset.rds"], decreasing = TRUE)[1]
        } else if (type == "datasetTidy") {
            datasetFilename <- sort(list.files(file.path(baseFolder, project, website, "Dataset"))[stringr::str_extract(list.files(file.path(baseFolder, project, website, "Dataset")), "datasetTidy.rds") == "datasetTidy.rds"], decreasing = TRUE)[1]
        } else if (type == "datasetBySentence") {
            datasetFilename <- sort(list.files(file.path(baseFolder, project, website, "Dataset"))[stringr::str_extract(list.files(file.path(baseFolder, project, website, "Dataset")), "datasetBySentence.rds") == "datasetBySentence.rds"], decreasing = TRUE)[1]
        } else if (type == "datasetRdata") {
            datasetFilename <- sort(list.files(file.path(baseFolder, project, website, "Dataset"))[stringr::str_extract(list.files(file.path(baseFolder, project, website, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        } else {
            stop("Type can be 'dataset', 'datasetTidy', or 'datasetBySentence'")
        }
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(baseFolder, project, website, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    if (type == "dataset"|type == "datasetTidy"|type == "datasetBySentence") {
        allDatasets <- tibble::data_frame()
        for (i in 1:length(lastSavedDatasets)) {
            dataset <- readRDS(lastSavedDatasets[i])
            # introduced for backward compatibility with datasets created with castarter ver<0.2 #legacy
            colnames(x = dataset)[colnames(dataset)=="nameOfProject"]<-"project"
            colnames(x = dataset)[colnames(dataset)=="nameOfWebsite"]<-"website"
            colnames(x = dataset)[colnames(dataset)=="articlesTxt"]<-"text"
            colnames(x = dataset)[colnames(dataset)=="contents"]<-"text"
            colnames(x = dataset)[colnames(dataset)=="articlesId"]<-"id"
            colnames(x = dataset)[colnames(dataset)=="articlesLinks"]<-"link"
            colnames(x = dataset)[colnames(dataset)=="links"]<-"link"
            colnames(x = dataset)[colnames(dataset)=="titles"]<-"title"
            colnames(x = dataset)[colnames(dataset)=="dates"]<-"date"
            if (convertToUTF8 == TRUE) {
                dataset$title <- base::iconv(x = dataset$title, to = "UTF-8")
                if (type == "dataset") {
                    dataset$text <- base::iconv(x = dataset$text, to = "UTF-8")
                } else if (type == "datasetTidy") {
                    dataset$word <- base::iconv(x = dataset$word, to = "UTF-8")
                } else if (type == "datasetBySentence") {
                    dataset$sentence <- base::iconv(x = dataset$sentence, to = "UTF-8")
                }
            }
            if (addProjectColumn == TRUE) {
                dataset$project <- projectsAndWebsites[[i]][1]
            }
            allDatasets <- dplyr::bind_rows(allDatasets, dataset)
            rm(dataset)
        }
        if (removeNAdates == TRUE) {
            allDatasets <- allDatasets[is.na(allDatasets$date) == FALSE, ]
        }
    }
    if (arrangeByDate==TRUE) {
        return(dplyr::as_data_frame(allDatasets) %>%
                   dplyr::arrange(date))
    } else {
        return(dplyr::as_data_frame(allDatasets))
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
#' \dontrun{
#' allDatasets <- LoadAllDatasets(project)
#' }

LoadAllDatasets <- function(project, removeNAdates = TRUE) {
    listOfWebsites <- gsub(paste0(project, "/"), "", list.dirs(file.path(baseFolder, project), recursive = FALSE), fixed = TRUE)
    lastSavedDatasets <- vector()
    for (i in 1:length(listOfWebsites)) {
        website <- listOfWebsites[i]
        datasetFilename <- sort(list.files(file.path(baseFolder, project, website, "Dataset"))[stringr::str_extract(list.files(file.path(baseFolder, project,
            website, "Dataset")), "dataset.RData") == "dataset.RData"], decreasing = TRUE)[1]
        if (is.na(datasetFilename) == FALSE) {
            lastSavedDataset <- file.path(file.path(baseFolder, project, website, "Dataset"), datasetFilename)
            lastSavedDatasets[i] <- lastSavedDataset
        }
        lastSavedDatasets <- lastSavedDatasets[!is.na(lastSavedDatasets)]
    }
    allDatasets <- data.frame()
    for (i in 1:length(lastSavedDatasets)) {
        load(lastSavedDatasets[i])
        # introduced for backward compatibility with datasets created with castarter ver<0.2 #legacy
        colnames(x = dataset)[colnames(dataset)=="nameOfProject"]<-"project"
        colnames(x = dataset)[colnames(dataset)=="nameOfWebsite"]<-"website"
        colnames(x = dataset)[colnames(dataset)=="articlesTxt"]<-"contents"
        colnames(x = dataset)[colnames(dataset)=="articlesId"]<-"id"
        colnames(x = dataset)[colnames(dataset)=="articlesLinks"]<-"link"
        colnames(x = dataset)[colnames(dataset)=="links"]<-"link"
        colnames(x = dataset)[colnames(dataset)=="titles"]<-"title"
        colnames(x = dataset)[colnames(dataset)=="dates"]<-"date"
        allDatasets <- rbind(allDatasets, dataset)
        rm(dataset)
    }
    if (removeNAdates == TRUE) {
        allDatasets <- allDatasets[is.na(allDatasets$date) == FALSE, ]
    }
    return(dplyr::as_data_frame(allDatasets))
}
