#' Archives Html, Txt, and IndexHtml folders in .tar.gz files.
#'
#' It archives  Html, Txt, and IndexHtml folders in .tar.gz files, and stores them in a dated sub-folder within the Archives sub-folder.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param removeArchivedFolders If equal to TRUE, it removes the folders and the original html and txt files from the hard disk, after having archived them.
#' @export
#' @section Warning:
#' If the option removeArchivedFolders is enabled, the function actually deletes files from the hard disk. Files are removed only after they have been successfully stored in compressed .tar.gz file, but it is recommended to backup valuable data before enabling this option.
#' @examples
#' \dontrun{
#' ArchiveFolders(project, website, removeArchivedFolders = FALSE)
#' }
ArchiveFolders <- function(project = NULL, website = NULL, removeArchivedFolders = FALSE) {
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
    if (!file.exists(file.path(baseFolder, project, website, "Archives"))) {
        dir.create(file.path(baseFolder, project, website, "Archives"))
    }
    today <- Sys.Date()
    if (!file.exists(file.path(baseFolder, project, website, "Archives", today))) {
      dir.create(file.path(baseFolder, project, website, "Archives", today))
    }
    if (file.exists(file.path(baseFolder, project, website, "Html"))) {
        tar(file.path(baseFolder, project, website, "Archives", today, "Html.tar.gz"), file.path(baseFolder, project, website, "Html"), compression = "gzip")
    }
    if (file.exists(file.path(baseFolder, project, website, "IndexHtml"))) {
        tar(file.path(baseFolder, project, website, "Archives", today, "IndexHtml.tar.gz"), file.path(baseFolder, project, website, "IndexHtml"), compression = "gzip")
    }
    if (file.exists(file.path(baseFolder, project, website, "Txt"))) {
        tar(file.path(baseFolder, project, website, "Archives", today, "Txt.tar.gz"), file.path(baseFolder, project, website, "Txt"), compression = "gzip")
    }
    if (removeArchivedFolders == TRUE) {
        if (file.exists(file.path(baseFolder, project, website, "Html"))) {
            unlink(file.path(baseFolder, project, website, "Html"), recursive = TRUE)
        }
        if (file.exists(file.path(baseFolder, project, website, "IndexHtml"))) {
            unlink(file.path(baseFolder, project, website, "IndexHtml"), recursive = TRUE)
        }
        if (file.exists(file.path(baseFolder, project, website, "Txt"))) {
            unlink(file.path(baseFolder, project, website, "Txt"), recursive = TRUE)
        }
    }
}

#' Restore Html, Txt, and IndexHtml folders previously archived with in .tar.gz files with ArchiveFolders().
#'
#' Restore Html, Txt, and IndexHtml folders previously archived with in .tar.gz files with ArchiveFolders().
#' @param html Logical, defaults to FALSE. If TRUE, restores previously archived html files in their standard folder.
#' @param indexHtml Logical, defaults to FALSE. If TRUE, restores previously archived indexHtml files in their standard folder.
#' @param txt Logical, defaults to FALSE. If TRUE, restores previously archived txt files in their standard folder.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @section Warning:
#' If the option overwrite is enabled, the function will overwrite files in Html, IndexHtml and Txt folders.
#' @examples
#' \dontrun{
#' RestoreArchives(project, website, removeArchivedFolders = FALSE)
#' }
RestoreArchives <- function(html = FALSE, indexHtml = FALSE, txt = FALSE, overwrite = FALSE, project = NULL, website = NULL) {
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
    if (html==TRUE) {
        if (file.exists(file.path(baseFolder, project, website, "Html"))==TRUE&overwrite==FALSE) {
            stop("Html folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(baseFolder, project, website, "Archives"), sort(list.files(file.path(baseFolder, project, website, "Archives")), decreasing = TRUE)[1])
            lastSavedHtmlFile <- file.path(lastArchiveFolderDate, "Html.tar.gz")
            untar(tarfile = lastSavedHtmlFile)
        }
    }
    if (indexHtml==TRUE) {
        if (file.exists(file.path(baseFolder, project, website, "IndexHtml"))==TRUE&overwrite==FALSE) {
            stop("IndexHtml folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(baseFolder, project, website, "Archives"), sort(list.files(file.path(baseFolder, project, website, "Archives")), decreasing = TRUE)[1])
            lastSavedIndexHtmlFile <- file.path(lastArchiveFolderDate, "IndexHtml.tar.gz")
            untar(tarfile = lastSavedIndexHtmlFile)
        }
    }
    if (txt==TRUE) {
        if (file.exists(file.path(baseFolder, project, website, "Txt"))==TRUE) {
            stop("Txt folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(baseFolder, project, website, "Archives"), sort(list.files(file.path(baseFolder, project, website, "Archives")), decreasing = TRUE)[1])
            lastSavedTxtFile <- file.path(lastArchiveFolderDate, "Txt.tar.gz")
            untar(tarfile = lastSavedTxtFile)
        }
    }
}
