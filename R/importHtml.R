#' Imports html files into a character vector.
#'
#' Imports html files already downloaded into a character vector.
#'
#' @param from Allows to choose which type of contents to import. Can be either "articles" or "index".
#' @param sample Defaults to NULL. If a numeric value n is provided, then instead of importing all html files it imports n random files.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param sort Logical, defaults to TRUE. For folders downloaded through 'castarter' (e.g. with DownloadContents) this should be left to TRUE, otherwise mismatch between articlesLinks and imported documents is due to occur.
#' @param recursive Value is passed to list.files function when using the pathToHtmlFolder option.
#' @return A character vector of html file, or a data frame if includePath is set to TRUE.
#' @export
#' @examples
#' \dontrun{
#' articlesHtml <- ImportHtml(from = articles, project, website)
#' }
ImportHtml <- function(from = "articles", sample = NULL, project = NULL, website = NULL, pathToHtmlFolder = "", sort = TRUE, recursive = FALSE, includePath = FALSE) {
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
    if (from == "articles") {
        htmlFilesList <- list.files(file.path(project, website, "Html"), pattern = "\\.html$", full.names = TRUE)
    } else if (from == "index") {
        htmlFilesList <- list.files(file.path(project, website, "IndexHtml"), pattern = "\\.html$", full.names = TRUE)
    } else {
        if (recursive == TRUE) {
            htmlFilesList <- list.files(pathToHtmlFolder, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
        } else {
        htmlFilesList <- list.files(pathToHtmlFolder, pattern = "\\.html$", full.names = TRUE)
        }
    }
    if (sort == TRUE) {
        htmlFilesList <- htmlFilesList[stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
    }
    if (is.numeric(sample) == TRUE) {
        htmlFilesList <- sample(x = htmlFilesList, size = sample)
    }
    numberOfArticles <- length(htmlFilesList)
    htmlFiles <- rep(NA, numberOfArticles)
    for (i in 1:numberOfArticles) {
        htmlFile <- readLines(htmlFilesList[i])
        htmlFile <- paste(htmlFile, collapse = "\n")
        htmlFiles[i] <- htmlFile
    }
    if (includePath == TRUE) {
        htmlFiles <- data.frame(htmlFiles, htmlFilesList, stringsAsFactors = FALSE)
    }
    htmlFiles
}
