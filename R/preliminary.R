#' Creates the folder structure needed by 'castarter'.
#'
#' Creates all the standard folders required by a 'castarter' project.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @examples
#' \dontrun{
#' CreateFolders(project, website)
#' }
CreateFolders <- function(project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (!file.exists(file.path(baseFolder))) {
        dir.create(file.path(baseFolder))
    }
    if (!file.exists(file.path(baseFolder, project))) {
        dir.create(file.path(baseFolder, project))
    }
    if (!file.exists(file.path(baseFolder, project, website))) {
        dir.create(file.path(baseFolder, project, website))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Logs"))) {
        dir.create(file.path(baseFolder, project, website, "Logs"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "IndexHtml"))) {
        dir.create(file.path(baseFolder, project, website, "IndexHtml"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Html"))) {
        dir.create(file.path(baseFolder, project, website, "Html"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Txt"))) {
        dir.create(file.path(baseFolder, project, website, "Txt"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Dataset"))) {
        dir.create(file.path(baseFolder, project, website, "Dataset"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Outputs"))) {
        dir.create(file.path(baseFolder, project, website, "Outputs"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "SessionRdata"))) {
        dir.create(file.path(baseFolder, project, website, "SessionRdata"))
    }
}

#' Finds the full path of the latest saved workspace for selected website.
#'
#' Used in combination with load, it loads the most recent workspace saved for a given website based on the date included in the filename of the .Rdata file.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @examples
#' \dontrun{
#' load(LoadLatest(project, website))
#' }
LoadLatest <- function(project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    lastSavedFile <- file.path(file.path(baseFolder, project, website, "SessionRdata"), sort(list.files(file.path(baseFolder, project, website, "SessionRdata"))[stringr::str_extract(list.files(file.path(baseFolder, project, website, "SessionRdata")), "RData") == "RData"], decreasing = TRUE)[1])
}

#' Saves working environment and dataset.
#'
#' It saves the working environment in the website folder, the dataset as an .Rdata file in the dataset subfolder, and, if exportXlsx = TRUE (by default), exports it as an .xlsx file in the Dataset sub-folder.
#' It assumes that previous operations have been completed, and both a 'metadata' and a 'contents' object exist. If they don't, it just prints this to the console.
#' @param saveEnvironment Logical, defaults to TRUE. If TRUE, saves environment as .Rdata in the correspodning website folder.
#' @param dataset Defaults to NULL. If TRUE, it seeks for a `metadata` and a `text` object in the current environment. If a `castarter` dataset is provided, this is then stored as the dataset.
#' @param datasetTidy Exports the dataset in the .rds format, one word per row, in line with tidy principles. See also the package `tidytext`.
#' @param datasetBySentence Exports the dataset in the .rds format, one sentence per row.
#' @param exportCsv Logical, defaults to FALSE. If TRUE, exports the complete dataset in the .csv file format in the Dataset sub-folder.
#' @param exportXlsx If equal to TRUE, exports the complete dataset in the .xlsx file format in the Dataset sub-folder.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A castarter dataset. It can however been used only for its side effects (save current workspace and dataset in relevant folders).
#' @export
#' @examples
#' \dontrun{
#' SaveWebsite(dataset = dataset)
#' }
SaveWebsite <- function(dataset = NULL,
                        datasetTidy = FALSE,
                        datasetBySentence = FALSE,
                        saveEnvironment = FALSE,
                        exportCsv = FALSE,
                        exportTxt = FALSE,
                        exportXlsx = FALSE,
                        project = NULL,
                        website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (requireNamespace("tidytext", quietly = TRUE)==FALSE) {
        stop("You need to install the `tidytext` package with `install.packages('tidytext')` to export datasets by word or by sentence.")
    }

    if (saveEnvironment == TRUE) {
        save.image(file = file.path(baseFolder, project, website, "SessionRdata", paste0(paste(Sys.Date(), project, website, sep = "-"), ".RData")))
        message(paste("Environment saved in", file.path(baseFolder, project, website, "SessionRdata", paste0(paste(Sys.Date(), project, website, sep = "-"), ".RData"))))
    }
    if (is.null(dataset) == FALSE) {
        if (is.data.frame(dataset)==TRUE) {
            saveRDS(object = dataset, file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds")))
            message(paste("Dataset saved in", file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds"))))
        } else if (dataset == TRUE) {
            dataset <- dplyr::bind_cols(tibble::data_frame(doc_id = metadata$doc_id, text = text),
                                        metadata %>%
                                            dplyr::select(-doc_id))
            saveRDS(object = dataset, file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds")))
            message(paste("Dataset saved in", file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".rds"))))
            }
    }
    if (is.null(datasetTidy)==FALSE) {
        if (datasetTidy==TRUE) {
            saveRDS(object = dataset %>%
                        tidytext::unnest_tokens(input = text,
                                                output = word,
                                                token = "words",
                                                to_lower = FALSE),
                    file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "datasetTidy", sep = "-"), ".rds")))
            message(paste("Tidy dataset saved in", file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "datasetTidy", sep = " - "), ".rds"))))
        }
    }
    if (is.null(datasetBySentence)==FALSE) {
        if (datasetBySentence==TRUE) {
            saveRDS(object = dataset %>%
                        tidytext::unnest_tokens(input = text,
                                                output = sentence,
                                                token = "sentences",
                                                to_lower = FALSE),
                    file = file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "datasetBySentence", sep = "-"), ".rds")))
            message(paste("Dataset (by sentence) saved in", file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "datasetBySentence", sep = " - "), ".rds"))))
        }
    }
    if (exportCsv == TRUE) {
        readr::write_csv(x = dataset,
                         path = file.path(baseFolder,
                                          project,
                                          website,
                                          "Dataset",
                                          paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".csv")))
    }
    if (exportTxt == TRUE) {
        writeLines(paste(paste("Date:", dataset$date), paste("Title:", dataset$title), paste("Link:", dataset$link), paste("ID:", dataset$id), dataset$text, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(baseFolder, project, website, "Dataset", paste0(paste(Sys.Date(), project, website, "dataset", sep = " - "), ".txt")))
        message(paste("Full dataset in txt format saved in", file.path(baseFolder, project, website, paste0(paste(Sys.Date(), project, website, "dataset", sep = "-"), ".txt"))))
    }
    invisible(dataset)
}
