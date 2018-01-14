#' Starts app that facilitates creation of a 'castarter' dataset
#'
#'
#' @export
#' @examples
#' CreateDataset()

CreateDataset <- function() {
    appDir <- system.file("shiny", "CreateDataset", package = "castarter")
    if (appDir == "") {
        stop("Could not find directory for the 'CreateDataset' app. Try re-installing `castarter`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
