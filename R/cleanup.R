#' Counts the number of HTML found in all folders of `castarter` projects
#'
#' This function facilitates the identification of folders with high numbers of Html files. Particularly useful before backups.
#'
#' @param exportCsv Defaults to FALSE. If TRUE, exports the outputs as csv file in the base folder (usually `castarter`, inside the working folder).
#' @return A data frame with the number of html files inlcuded in project subfolders
#' @export
#' @examples
#' CountHtml()
CountHtml <- function(exportCsv = FALSE) {
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }

    htmlFileNumber <- tibble::tibble(
        dir = dirname(list.files(path = baseFolder,
                                 pattern=".html",
                                 full.names = FALSE,
                                 recursive = TRUE))) %>%
        dplyr::count(dir, sort = TRUE)

    if (exportCsv == TRUE) {
        readr::write_csv(x = htmlFileNumber, path = "htmlCount.csv")
    }

    return(htmlFileNumber)
}


