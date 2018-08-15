#' Creates and starts a Shiny App exposing basic 'castarter' functions based on the given dataset
#'
#' Creates and starts a Shiny App exposing basic 'castarter' functions based on the given dataset
#'
#' @param dataset A 'castarter' dataset.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return Nothing, used for side effects: a shiny app is created in the Outputs folder.
#' @export
#' @examples
#' CreateShinyApp(dataset)
CreateShinyApp <- function(dataset,
                           terms = "europe",
                           customTitle = NULL,
                           project = NULL,
                           website = NULL) {
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
    if (!file.exists(file.path(baseFolder, project, website, "Outputs", "shinyApp"))) {
        dir.create(file.path(baseFolder, project, website, "Outputs", "shinyApp"))
    }
    if (!file.exists(file.path(baseFolder, project, website, "Outputs", "shinyApp", "data"))) {
        dir.create(file.path(baseFolder, project, website, "Outputs", "shinyApp", "data"))
    }
    # create dataset divided by sentence and store in shiny folder
    saveRDS(object = tidytext::unnest_tokens(tbl = dataset,
                                             input = text,
                                             output = sentence,
                                             token = "sentences",
                                             to_lower = FALSE),
            file = file.path(baseFolder, project, website, "Outputs", "shinyApp", "data", "dataset_bySentence.rds"))

    file.copy(from = system.file("extdata", "shiny", "DatasetAnalysis", "server.R", package = "castarter"),
              to = file.path(baseFolder, project, website, "Outputs", "shinyApp","server.R"))
    file.copy(from = system.file("extdata", "shiny", "DatasetAnalysis", "ui.R", package = "castarter"),
              to = file.path(baseFolder, project, website, "Outputs", "shinyApp","ui.R"))
    if (is.null(x = customTitle)==TRUE) {
        customTitle <- website
    }
    write(x = paste(c("library('shiny')",
                      "library('dplyr')",
                      "library('castarter')",
                      "# load dataset",
                      "dataset <- readRDS(file = file.path('data', 'dataset_bySentence.rds'))",

                      "minDate <- min(dataset$date)",
                      "maxDate <- max(dataset$date)",

                      paste0("applicationTitle <- '" , customTitle, "'"),
                      paste0("exampleTerms <- '" , paste(terms, collapse = ', '), "'"),
                      paste0("SetCastarter(project = '", project, "', website = '", website, "')")
    ), sep = "\n")
    , file = file.path(baseFolder, project, website, "Outputs", "shinyApp", "global.R"))
    shiny::runApp(file.path(baseFolder, project, website, "Outputs", "shinyApp"), display.mode = "normal")
}
