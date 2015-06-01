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
