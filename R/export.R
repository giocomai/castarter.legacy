#' Exports all articles that contain a given term.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param term The term that determines which articles are exported.Must be a character vector of length = 1.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no nameOfWebsite is provided, exported files are saved in the nameOfProject/Outputs folder.
#' @export
#' @examples
#' ExportArticlesWith(dataset, "example", nameOfProject, nameOfWebsite)

ExportArticlesWith <- function(dataset, term, nameOfProject, nameOfWebsite = NULL, txt = TRUE, csv = FALSE, xlsx = FALSE, data.frame = FALSE, includeOnly = NULL, sortBy = "date") {
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
    if (xlsx == TRUE) {
        if (is.null(nameOfWebsite) == TRUE) {
            xlsx::write.xlsx(tempDataset, file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(nameOfProject, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = ""))))     
        } else {
            xlsx::write.xlsx(tempDataset, file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(term, " in ", nameOfWebsite, ".xlsx", sep = ""))))     
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
    if (data.frame == TRUE) {
        tempDataset
    }
} 
