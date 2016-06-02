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
        nameOfProject <- CastarterOptions("nameOfWebsite")
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
