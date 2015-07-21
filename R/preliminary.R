#' Loads all packages needed for 'castarter'
#'
#' It needs to be deleted.
#' @keywords toBeRemoved
#' @export
#' @examples
#' LoadRequiredPackages()

LoadRequiredPackages <- function() {
    if (!require("pacman")) 
        install.packages("pacman")
    pacman::p_load("RCurl", "stringr", "gtools", "boilerpipeR", "XML", "lubridate", "tm", "SnowballC", "ggplot2", "wordcloud", "dplyr", "xlsx", 
        "slam", "zoo", "RGtk2Extras", "scales", "reshape2", "gridExtra", "RColorBrewer", "mgcv")
}

#' Creates the folder structure needed by 'castarter'.
#'
#' It needs to be deleted.
#' @keywords toBeRemoved
#' @export
#' @examples
#' CreateFolderStructure(nameOfProject, nameOfWebsite)
CreateFolderStructure <- function(nameOfProject, nameOfWebsite) {
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

#' Loads last saved workspace for selected website. 
#'
#' It loads the most recent workspace saved for a given website based on the date included in the filename of the .Rdata file. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @examples
#' LoadLatestWebsite(nameOfProject, nameOfWebsite)
LoadLatestWebsite <- function(nameOfProject, nameOfWebsite) {
    lastSavedFile <- file.path(file.path(nameOfProject, nameOfWebsite), sort(list.files(file.path(nameOfProject, nameOfWebsite))[str_extract(list.files(file.path(nameOfProject, 
        nameOfWebsite)), "RData") == "RData"], decreasing = TRUE)[1])
    if (file.exists(lastSavedFile)) {
        load(file = lastSavedFile)
    }
}

#' Saves working environment and dataset.
#'
#' It saves the working environment in the website folder, the dataset as an .Rdata file in the dataset subfolder, and, if exportXlsx = TRUE (by default), exports it as an .xlsx file in the Dataset sub-folder.
#' It assumes that previous operations have been completed, and both a 'metadata' and an 'articlesTxt' object exist. If they don't, it just prints this to the console. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @export
#' @examples
#' LoadLatestWebsite(nameOfProject, nameOfWebsite)
SaveAndExportWebsite <- function(nameOfProject, nameOfWebsite, exportXlsx = FALSE) {
    ## Save environment
    save.image(file = file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, sep = " - "), ".RData")))
    print(paste("Environment saved in", file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, sep = " - "), ".RData"))))
    ## Save dataset
    if (exists("metadata")==FALSE) {
        print("Metadata not available, the dataset has not been saved separately.")
    } else {
    dataset <- cbind(metadata, articlesTxt, stringsAsFactors = FALSE)
    save(dataset, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", 
        sep = " - "), ".RData")))
    }
    # export all as xlsx
    if (exportXlsx == TRUE) {
        write.xlsx(cbind(metadata, articlesTxt), file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, 
            "metadata and txt", sep = " - "), ".xlsx")))
    }
} 
