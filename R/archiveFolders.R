
ArchiveFolders <- function(nameOfProject, nameOfWebsite) {
    if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Archives"))) {
        dir.create(file.path(nameOfProject, nameOfWebsite, "Archives"))
    }
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", "html.tgz"), file.path(nameOfProject, nameOfWebsite, "Html"), compression = "gzip")
    }
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", "IndexHtml.tgz"), file.path(nameOfProject, nameOfWebsite, "IndexHtml"), compression = "gzip")
    }
    if (file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))) {
        tar(file.path(nameOfProject, nameOfWebsite, "Archives", "Txt.tgz"), file.path(nameOfProject, nameOfWebsite, "Txt"), compression = "gzip")
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
