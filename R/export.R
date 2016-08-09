#' Exports all articles that contain a given term.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param term The term that determines which articles are exported.Must be a character vector of length = 1.
#' @param onlyNaDates Logical, defaults to FALSE. Used to for troubleshooting dataset. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no nameOfWebsite is provided, exported files are saved in the nameOfProject/Outputs folder.
#' @param txt Logical, defaults to TRUE. If TRUE, exports all articles including the term provided in a .txt file.
#' @param csv Logical, defaults to FALSE. If TRUE, exports all articles including the term provided in a .csv file.
#' @param xlsx Logical, defaults to FALSE. If TRUE, exports all articles including the term provided in a .xlsx file.
#' @param data.fram Logical, defaults to FALSE. If TRUE, the function outputs a data frame. If FALSE, function used only for its side effects (i.e. exporting to files)
#' @export
#' @examples
#' ExportArticlesWith(dataset, "example")

ExportArticlesWith <- function(dataset, term, onlyNaDates = FALSE, txt = TRUE, csv = FALSE, xlsx = FALSE, data.frame = FALSE, includeOnly = NULL, sortBy = "date", nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (onlyNaDates == TRUE) {
        dataset <- dataset[is.na(dataset$dates),]
    }
    # Export only items that include...
    tempDataset <- dataset[grep(term, dataset$articlesTxt, ignore.case = TRUE), ]
    if (is.null(includeOnly) == FALSE) {
        tempDataset <- tempDataset[tempDataset$nameOfWebsite == includeOnly, ]
    }
    if (sortBy == "date") {
        tempDataset <- tempDataset[order(tempDataset$dates), ]
    }
    if (is.null(nameOfWebsite) == TRUE) {
        if (!file.exists(file.path(nameOfProject, "Outputs"))) {
            dir.create(file.path(nameOfProject, "Outputs"))
        } 
    } else {
        if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs"))) {
            dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
        }
    }
    if (csv == TRUE) {
        if (is.null(nameOfWebsite) == TRUE) {
            utils::write.csv(tempDataset, file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".csv", sep = ""))))     
        } else {
            utils::write.csv(tempDataset, file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".csv", sep = ""))))     
        }
    }
    if (txt == TRUE) {
        if (is.null(nameOfWebsite) == TRUE) {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$titles), paste("Link:", tempDataset$articlesLinks), paste("ID:", tempDataset$articlesId), tempDataset$articlesTxt, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(nameOfProject, "Outputs", paste(term, " in ", ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfProject, ".txt", sep = ""))))
        } else {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$titles), paste("Link:", tempDataset$articlesLinks), paste("ID:", tempDataset$articlesId), tempDataset$articlesTxt, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".txt", sep = ""))))
        }
    }
    if (xlsx == TRUE) {
        if (is.null(nameOfWebsite) == TRUE) {
            xlsx::write.xlsx(tempDataset, file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = ""))))     
        } else {
            xlsx::write.xlsx(tempDataset, file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = ""))))     
        }
    }
    if (data.frame == TRUE) {
        tempDataset
    }
} 
