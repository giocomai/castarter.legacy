#' Exports all articles that contain a given term.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param term The term that determines which articles are exported.Must be a character vector of length = 1.
#' @param onlyNaDates Logical, defaults to FALSE. Used to for troubleshooting dataset. 
#' @param txt Logical, defaults to TRUE. If TRUE, exports all articles including the term provided in a .txt file.
#' @param csv Logical, defaults to FALSE. If TRUE, exports all articles including the term provided in a .csv file.
#' @param xlsx Logical, defaults to FALSE. If TRUE, exports all articles including the term provided in a .xlsx file.
#' @param data.frame Logical, defaults to FALSE. If TRUE, the function outputs a data frame. If FALSE, function used only for its side effects (i.e. exporting to files)
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no website is provided, exported files are saved in the project/Outputs folder.
#' @export
#' @examples
#' ExportArticlesWith(dataset, "example")

ExportArticlesWith <- function(dataset, term, includeNaDates = TRUE, onlyNaDates = FALSE, txt = TRUE, csv = FALSE, xlsx = FALSE, data.frame = FALSE, includeOnly = NULL, sortBy = "date", project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (onlyNaDates == TRUE) {
        dataset <- dataset[is.na(dataset$dates),]
    }
    if (includeNaDates == FALSE) {
        dataset <- dataset[is.na(dataset$dates)==FALSE,]
    }
    # Export only items that include...
    tempDataset <- dataset[grep(term, dataset$articlesTxt, ignore.case = TRUE), ]
    if (is.null(includeOnly) == FALSE) {
        tempDataset <- tempDataset[tempDataset$website == includeOnly, ]
    }
    if (sortBy == "date") {
        tempDataset <- tempDataset[order(tempDataset$dates), ]
    }
    if (is.null(website) == TRUE) {
        if (!file.exists(file.path(project, "Outputs"))) {
            dir.create(file.path(project, "Outputs"))
        } 
    } else {
        if (!file.exists(file.path(project, website, "Outputs"))) {
            dir.create(file.path(project, website, "Outputs"))
        }
    }
    if (csv == TRUE) {
        if (is.null(website) == TRUE) {
            utils::write.csv(tempDataset, file.path(project, "Outputs", paste(term, " in ", website, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(project, "Outputs", paste(term, " in ", website, ".csv", sep = ""))))     
        } else {
            utils::write.csv(tempDataset, file.path(project, website, "Outputs", paste(term, " in ", website, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(project, website, "Outputs", paste(term, " in ", website, ".csv", sep = ""))))     
        }
    }
    if (txt == TRUE) {
        if (is.null(website) == TRUE) {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$titles), paste("Link:", tempDataset$articlesLinks), paste("ID:", tempDataset$articlesId), tempDataset$articlesTxt, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(project, "Outputs", paste(term, " in ", ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(project, "Outputs", paste(term, " in ", project, ".txt", sep = ""))))
        } else {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$titles), paste("Link:", tempDataset$articlesLinks), paste("ID:", tempDataset$articlesId), tempDataset$articlesTxt, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(project, website, "Outputs", paste(term, " in ", website, ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(project, website, "Outputs", paste(term, " in ", website, ".txt", sep = ""))))
        }
    }
    if (xlsx == TRUE) {
        if (is.null(website) == TRUE) {
            xlsx::write.xlsx(tempDataset, file.path(project, "Outputs", paste(term, " in ", website, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(project, "Outputs", paste(term, " in ", website, ".xlsx", sep = ""))))     
        } else {
            xlsx::write.xlsx(tempDataset, file.path(project, website, "Outputs", paste(term, " in ", website, ".xlsx", sep = "")))
            print(paste("File .xlsx exported to:", file.path(project, website, "Outputs", paste(term, " in ", website, ".xlsx", sep = ""))))     
        }
    }
    if (data.frame == TRUE) {
        tempDataset
    }
} 
