#' Creates the folder structure needed by 'castarter'.
#'
#' Creates all the standard folders required by a 'castarter' project.
#' @export
#' @examples
#' CreateFolderStructure(nameOfProject, nameOfWebsite)
CreateFolderStructure <- function(nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (!file.exists(file.path(nameOfProject))) {
        dir.create(file.path(nameOfProject))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite))) {
        dir.create(file.path(nameOfProject, nameOfWebsite))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Logs"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Logs"))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Html"))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Txt"))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Dataset"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Dataset"))
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
    }
}

#' Finds the full path of the latest saved workspace for selected website. 
#'
#' Used in combination with load, it loads the most recent workspace saved for a given website based on the date included in the filename of the .Rdata file. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @examples
#' load(LoadLatest(nameOfProject, nameOfWebsite))
LoadLatest <- function(nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    lastSavedFile <- file.path(file.path(nameOfProject, nameOfWebsite), sort(list.files(file.path(nameOfProject, nameOfWebsite))[stringr::str_extract(list.files(file.path(nameOfProject, nameOfWebsite)), "RData") == "RData"], decreasing = TRUE)[1])
}

#' Saves working environment and dataset.
#'
#' It saves the working environment in the website folder, the dataset as an .Rdata file in the dataset subfolder, and, if exportXlsx = TRUE (by default), exports it as an .xlsx file in the Dataset sub-folder.
#' It assumes that previous operations have been completed, and both a 'metadata' and an 'articlesTxt' object exist. If they don't, it just prints this to the console. 
#' @param saveEnvironment Logical, defaults to TRUE. If TRUE, saves environment as .Rdata in the correspodning website folder. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param dataset A 'castarter' dataset. Required to export dataset in its dedicated folder if no metadata and articlesTxt are found in the environment, e.g. because dataset has been created through CreateDatasetFromHtml() function.
#' @param exportCsv Logical, defaults to FALSE. If TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx Logical, defaults to FALSE. If TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return Nothing. Used for its side effects (save current workspace and dataset in relevant folders).
#' @export
#' @examples
#' SaveWebsite(nameOfProject, nameOfWebsite)
SaveWebsite <- function(saveEnvironment = TRUE, dataset = NULL, corpus = NULL, corpusDtm = NULL, nameOfProject = NULL, nameOfWebsite = NULL, exportCsv = FALSE, exportXlsx = FALSE) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (saveEnvironment == TRUE) {
        save.image(file = file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, sep = " - "), ".RData")))
        print(paste("Environment saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, sep = " - "), ".RData"))))
    }
    if (is.null(dataset) == FALSE) {
        if (dataset == TRUE) {
            if (exists("metadata")|exists("articlesTxt") == FALSE) {
                warning("Dataset not provided, and either metadata or articlesTxt not available: the dataset has not been saved separately.")
            } else {
                dataset <- cbind(metadata, articlesTxt, stringsAsFactors = FALSE)
                save(dataset, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData")))
                print(paste("Dataset saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData"))))
            }
        } else if (is.data.frame(dataset)==TRUE) {
            save(dataset, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData")))
            print(paste("Dataset saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData"))))
        }
    }
    if (is.null(corpus)==FALSE) {
        if (quanteda::is.corpus(corpus)==TRUE) {
            save(corpus, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusQ", sep = " - "), ".RData")))
            message(paste("Corpus saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusQ", sep = " - "), ".RData"))))
        } else {
            save(corpus, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusTM", sep = " - "), ".RData")))
            message(paste("Corpus saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusTM", sep = " - "), ".RData"))))
        }
    }
    if (is.null(corpusDtm)==FALSE) {
        if (quanteda::is.dfm(corpusDtm)) {
            save(corpusDtm, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusDtmQ", sep = " - "), ".RData")))
            message(paste("corpusDtm of the 'tm' type saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusDtmQ", sep = " - "), ".RData"))))
        } else {
            save(corpusDtm, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusDtmTM", sep = " - "), ".RData")))
            message(paste("corpusDtm of the 'tm' type saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "corpusDtmTM", sep = " - "), ".RData"))))
        }
 }
    if (exportCsv == TRUE) {
        write.csv(dataset, file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".csv")))
    }
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(dataset, file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".xlsx")))
    }
} 