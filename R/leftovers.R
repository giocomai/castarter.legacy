RenumberArticles <- function(nameOfProject = "", nameOfWebsite = "") {
    htmlFilesList <- list.files(file.path(nameOfProject, nameOfWebsite, "Html"), pattern = "\\.html$", full.names = TRUE)
    htmlFilesList <- gtools::mixedsort(htmlFilesList)
    numberOfArticles <- length(htmlFilesList)
    htmlFilesListNew <- file.path(nameOfProject, nameOfWebsite, "Html", paste0(1:numberOfArticles, ".html"))
    file.rename(htmlFilesList, htmlFilesListNew)
} 