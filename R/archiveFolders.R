#' Archives Html, Txt, and IndexHtml folders in .tar.gz files. 
#'
#' It archives  Html, Txt, and IndexHtml folders in .tar.gz files, and stores them in a dated sub-folder within the Archives sub-folder. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param removeArchivedFolders If equal to TRUE, it removes the folders and the original html and txt files from the hard disk, after having archived them. 
#' @export
#' @section Warning:
#' If the option removeArchivedFolders is enabled, the function actually deletes files from the hard disk. Files are removed only after they have been successfully stored in compressed .tar.gz file, but it is recommended to backup valuable data before enabling this option.
#' @examples
#' ArchiveFolders(nameOfProject, nameOfWebsite, removeArchivedFolders = FALSE))
ArchiveFolders <- function(nameOfProject = NULL, nameOfWebsite = NULL, removeArchivedFolders = FALSE) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Archives"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Archives"))
    }
    today <- Sys.Date()
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Archives", today))) {
      dir.create(file.path(nameOfProject, nameOfWebsite, "Archives", today))
    }    
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", today, "Html.tar.gz"), file.path(nameOfProject, nameOfWebsite, "Html"), compression = "gzip")
    }
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", today, "IndexHtml.tar.gz"), file.path(nameOfProject, nameOfWebsite, "IndexHtml"), compression = "gzip")
    }
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", today, "Txt.tar.gz"), file.path(nameOfProject, nameOfWebsite, "Txt"), compression = "gzip")
    }
    if (removeArchivedFolders == TRUE) {
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))) {
            unlink(file.path(nameOfProject, nameOfWebsite, "Html"), recursive = TRUE)
        }
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))) {
            unlink(file.path(nameOfProject, nameOfWebsite, "IndexHtml"), recursive = TRUE)
        }
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))) {
            unlink(file.path(nameOfProject, nameOfWebsite, "Txt"), recursive = TRUE)
        }
    }
}

#' Restore Html, Txt, and IndexHtml folders previously archived with in .tar.gz files with ArchiveFolders().
#'
#' Restore Html, Txt, and IndexHtml folders previously archived with in .tar.gz files with ArchiveFolders().
#' @param html Logical, defaults to FALSE. If TRUE, restores previously archived html files in their standard folder.
#' @param indeHtml Logical, defaults to FALSE. If TRUE, restores previously archived indexHtml files in their standard folder.
#' @param txt Logical, defaults to FALSE. If TRUE, restores previously archived txt files in their standard folder.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @export
#' @section Warning:
#' If the option overwrite is enabled, the function will overwrite files in Html, IndexHtml and Txt folders. 
#' @examples
#' RestoreArchives(nameOfProject, nameOfWebsite, removeArchivedFolders = FALSE))
RestoreArchives <- function(html = FALSE, indexHtml = FALSE, txt = FALSE, overwrite = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (html==TRUE) {
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))==TRUE) {
            stop("Html folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(nameOfProject, nameOfWebsite, "Archives"), sort(list.files(file.path(nameOfProject, nameOfWebsite, "Archives")), decreasing = TRUE)[1])
            lastSavedHtmlFile <- file.path(lastArchiveFolderDate, "Html.tar.gz")
            untar(tarfile = lastSavedHtmlFile)
        }
    }
    if (indexHtml==TRUE) {
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "indexHtml"))==TRUE) {
            stop("indexHtml folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(nameOfProject, nameOfWebsite, "Archives"), sort(list.files(file.path(nameOfProject, nameOfWebsite, "Archives")), decreasing = TRUE)[1])
            lastSavedIndexHtmlFile <- file.path(lastArchiveFolderDate, "indexHtml.tar.gz")
            untar(tarfile = lastSavedIndexHtmlFile)
        }
    }
    if (Txt==TRUE) {
        if (file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))==TRUE) {
            stop("Txt folder already exists, and overwrite option is not enabled. Either remove folder or set overwrite=TRUE")
        } else {
            lastArchiveFolderDate <- file.path(file.path(nameOfProject, nameOfWebsite, "Archives"), sort(list.files(file.path(nameOfProject, nameOfWebsite, "Archives")), decreasing = TRUE)[1])
            lastSavedTxtFile <- file.path(lastArchiveFolderDate, "Txt.tar.gz")
            untar(tarfile = lastSavedTxtFile)
        }
    }
} 
