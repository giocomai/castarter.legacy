#' Imports html files into a character vector. 
#' 
#' Imports html files already downloaded into a character vector.
#'  
#' @param from Allows to choose which type of contents to import. Can be either "articles" or "index".
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A character vector of html files.
#' @export
#' @examples
#' articlesHtml <- ImportHtml(from = articles, nameOfProject, nameOfWebsite)
ImportHtml <- function(from = "", nameOfProject = "", nameOfWebsite = "", pathToHtmlFolder = "") {
    if (from == "articles") {
        htmlFilesList <- list.files(file.path(nameOfProject, nameOfWebsite, "Html"), pattern = "\\.html$", full.names = TRUE)
    } else if (from == "index") {
        htmlFilesList <- list.files(file.path(nameOfProject, nameOfWebsite, "IndexHtml"), pattern = "\\.html$", full.names = TRUE)
    } else {
        htmlFilesList <- list.files(pathToHtmlFolder, pattern = "\\.html$", full.names = TRUE)
    }
    htmlFilesList <- mixedsort(htmlFilesList)
    numberOfArticles <- length(htmlFilesList)
    htmlFiles <- rep(NA, numberOfArticles)
    for (i in 1:numberOfArticles) {
        htmlFile <- readLines(htmlFilesList[i])
        htmlFile <- paste(htmlFile, collapse = "\n")
        htmlFiles[i] <- htmlFile
    }
    htmlFiles
}

RenumberArticles <- function(nameOfProject = "", nameOfWebsite = "") {
    htmlFilesList <- list.files(file.path(nameOfProject, nameOfWebsite, "Html"), pattern = "\\.html$", full.names = TRUE)
    htmlFilesList <- mixedsort(htmlFilesList)
    numberOfArticles <- length(htmlFilesList)
    htmlFilesListNew <- file.path(nameOfProject, nameOfWebsite, "Html", paste0(1:numberOfArticles, ".html"))
    file.rename(htmlFilesList, htmlFilesListNew)
} 
