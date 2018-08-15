#' Starts a Shiny app that facilitates the creation of a 'castarter' dataset.
#'
#'
#' @export
#' @examples
#' CreateDataset()

CreateDataset <- function() {
    appDir <- system.file("extdata", "shiny", "CreateDataset", package = "castarter")
    if (appDir == "") {
        stop("Could not find directory for the 'CreateDataset' app. Try re-installing `castarter`.", call. = FALSE)
    }
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
    }
    shiny::runApp(appDir, display.mode = "normal")
}

#' Starts a Shiny app that facilitates sub-setting, reading, and tagging a `castarter` dataset.
#'
#'
#' @export
#' @examples
#' SubsetAndRead()

SubsetAndRead <- function() {
    appDir <- system.file("extdata", "shiny", "SubsetAndRead", package = "castarter")
    if (appDir == "") {
        stop("Could not find directory for the 'SubsetAndRead' app. Try re-installing `castarter`.", call. = FALSE)
    }
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
    }
    shiny::runApp(appDir, display.mode = "normal")
}

#' Starts a Shiny app that facilitates sub-setting, reading, and tagging a `castarter` dataset.
#'
#'
#' @export
#' @examples
#' AnalyseDataset()

AnalyseDataset <- function() {
    appDir <- system.file("extdata", "shiny", "AnalyseDataset", package = "castarter")
    if (appDir == "") {
        stop("Could not find directory for the 'AnalyseDataset' app. Try re-installing `castarter`.", call. = FALSE)
    }
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
    }
    shiny::runApp(appDir, display.mode = "normal")
}
