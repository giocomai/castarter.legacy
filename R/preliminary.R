#' Creates the folder structure needed by 'castarter'.
#'
#' Creates all the standard folders required by a 'castarter' project.
#' @export
#' @examples
#' CreateFolders(project, website)
CreateFolders <- function(project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (!file.exists(file.path(project))) {
        dir.create(file.path(project))
    }
    if (!file.exists(file.path(project, website))) {
        dir.create(file.path(project, website))
    }
    if (!file.exists(file.path(project, website, "Logs"))) {
        dir.create(file.path(project, website, "Logs"))
    }
    if (!file.exists(file.path(project, website, "IndexHtml"))) {
        dir.create(file.path(project, website, "IndexHtml"))
    }
    if (!file.exists(file.path(project, website, "Html"))) {
        dir.create(file.path(project, website, "Html"))
    }
    if (!file.exists(file.path(project, website, "Txt"))) {
        dir.create(file.path(project, website, "Txt"))
    }
    if (!file.exists(file.path(project, website, "Dataset"))) {
        dir.create(file.path(project, website, "Dataset"))
    }
    if (!file.exists(file.path(project, website, "Outputs"))) {
        dir.create(file.path(project, website, "Outputs"))
    }
}

#' Finds the full path of the latest saved workspace for selected website.
#'
#' Used in combination with load, it loads the most recent workspace saved for a given website based on the date included in the filename of the .Rdata file.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @examples
#' load(LoadLatest(project, website))
LoadLatest <- function(project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    lastSavedFile <- file.path(file.path(project, website), sort(list.files(file.path(project, website))[stringr::str_extract(list.files(file.path(project, website)), "RData") == "RData"], decreasing = TRUE)[1])
}

#' Saves working environment and dataset.
#'
#' It saves the working environment in the website folder, the dataset as an .Rdata file in the dataset subfolder, and, if exportXlsx = TRUE (by default), exports it as an .xlsx file in the Dataset sub-folder.
#' It assumes that previous operations have been completed, and both a 'metadata' and a 'contents' object exist. If they don't, it just prints this to the console.
#' @param saveEnvironment Logical, defaults to TRUE. If TRUE, saves environment as .Rdata in the correspodning website folder.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param dataset A 'castarter' dataset. Required to export dataset in its dedicated folder if no metadata and contents are found in the environment, e.g. because dataset has been created through CreateDatasetFromHtml() function.
#' @param exportCsv Logical, defaults to FALSE. If TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx Logical, defaults to FALSE. If TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @return Nothing. Used for its side effects (save current workspace and dataset in relevant folders).
#' @export
#' @examples
#' SaveWebsite(project, website)
SaveWebsite <- function(saveEnvironment = TRUE, dataset = NULL, corpus = NULL, tidyCorpus = NULL, corpusDtm = NULL, project = NULL, website = NULL, exportCsv = FALSE, exportTxt = FALSE, exportXlsx = FALSE) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (saveEnvironment == TRUE) {
        save.image(file = file.path(project, website, paste0(paste(Sys.Date(), project, website, sep = " - "), ".RData")))
        print(paste("Environment saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, sep = " - "), ".RData"))))
    }
    if (is.null(dataset) == FALSE) {
        if (is.data.frame(dataset)==TRUE) {
            save(dataset, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData")))
            print(paste("Dataset saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData"))))
        } else if (dataset == TRUE) {
            dataset <- cbind(metadata, contents, stringsAsFactors = FALSE)
            save(dataset, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData")))
            print(paste("Dataset saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".RData"))))
        }
    }
    if (is.null(corpus)==FALSE) {
        if (quanteda::is.corpus(corpus)==TRUE) {
            save(corpus, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "corpusQ", sep = " - "), ".RData")))
            message(paste("Corpus of the 'quanteda' type saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "corpusQ", sep = " - "), ".RData"))))
        } else {
            save(corpus, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "corpusTM", sep = " - "), ".RData")))
            message(paste("Corpus of the 'tm' type saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "corpusTM", sep = " - "), ".RData"))))
        }
    }
    if (is.null(tidyCorpus)==FALSE) {
            saveRDS(object = tidyCorpus, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "tidyCorpus", sep = " - "), ".rds")))
            message(paste("Tidy corpus saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "tidyCorpus", sep = " - "), ".rds"))))
    }
    if (is.null(corpusDtm)==FALSE) {
        if (quanteda::is.dfm(corpusDtm)) {
            save(corpusDtm, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "corpusDtmQ", sep = " - "), ".RData")))
            message(paste("corpusDtm of the 'quanteda' type saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "corpusDtmQ", sep = " - "), ".RData"))))
        } else {
            save(corpusDtm, file = file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "corpusDtmTM", sep = " - "), ".RData")))
            message(paste("corpusDtm of the 'tm' type saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "corpusDtmTM", sep = " - "), ".RData"))))
        }
 }
    if (exportCsv == TRUE) {
        write.csv(dataset, file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".csv")))
    }
    if (exportXlsx == TRUE) {
        xlsx::write.xlsx(dataset, file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".xlsx")))
    }
    if (exportTxt == TRUE) {
        writeLines(paste(paste("Date:", dataset$date), paste("Title:", dataset$title), paste("Link:", dataset$link), paste("ID:", dataset$id), dataset$contents, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".txt")))
        message(paste("Full dataset in txt format saved in", file.path(project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".txt"))))
    }
}
