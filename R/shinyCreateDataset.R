#' Starts a Shiny app that facilitates the creation of a 'castarter' dataset.
#'
#' Starts a Shiny app that facilitates the creation of a 'castarter' dataset.
#'
#' @export
#' @examples
#' \dontrun{
#' CreateDataset()
#' }

CreateDataset <- function() {
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
    }
    shiny::shinyApp(ui = fluidPage(

                    tags$head(
                    tags$style(HTML("
                    .row {
                        margin-left: 0;
                        margin-right: 0;
                    }

                    .tab-content {
                        padding-top: 20px;
                    }
                    "))
                    ),

                    titlePanel("Create a textual dataset from a website"),

                    navlistPanel(
                    #### Beginning of SetCastarter page ####
                    tabPanel("Set up project",
                    fluidPage(theme="style.css",
                    selectInput(inputId = "availableProject", label = "Available projects",
                                choices = as.list(c("", list.dirs(path = "castarter", full.names = FALSE, recursive = FALSE)), selected = "")),
                    uiOutput(outputId = "selectWebsite_UI"),
                    uiOutput(outputId = "selectProjectManually_UI"),
                    uiOutput(outputId = "selectWebsiteManually_UI"),
                    actionButton(inputId = "SetCastarter", label = "Set up project", icon = icon("check"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        uiOutput(outputId = "SetCastarter")
                    )
                    ),
#### End of SetCastarter page ####

#### Beginning of CreateLinks page ####
tabPanel("Create index links",
         fluidPage(theme="style.css",

                   column(6,
                          h3("Introduce parameters"),
                          textInput(inputId = "CreateLinks_linkFirstChunk", label = "First part of the link", value = "https://www.example.com/news/", width = NULL, placeholder = "e.g. https://www.example.com/news/"),
                          textInput(inputId = "CreateLinks_linkSecondChunk", label = "Part of the link to append after varying part", value = NULL, width = NULL, placeholder = "e.g. .html"),
                          radioButtons(inputId = "CreateLinks_RadioUI", label = "Links based on numbers or dates?", choices = c("Numbers", "Dates"), inline = TRUE),
                          ## Reactive CreateLinks UI
                          uiOutput("CreateLinks_UI"),
                          actionButton(inputId = "CreateLinks", label = "Confirm settings", icon = icon("check"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),

                   column(6,
                          fluidRow(
                              h3("Preview of generated links"),
                              uiOutput("PreviewIndexLinks_UI")
                          )
                   )
         )
),
#### End of CreateLinks page ####

#### Download index page ####
tabPanel("Download index pages",
         fluidPage(theme="style.css",
                   column(12,
                          fluidRow(
                              h3("Download index pages"),
                              actionButton(inputId = "DownloadIndexPages", label = "Download index pages now", icon = icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")

                          )
                   )
         )),
#### Extract links ####
tabPanel("Extract links",
         fluidPage(theme="style.css",
                   column(12,
                          fluidRow(
                              h3("Extract links")

                          )
                   )
         )),
tabPanel("Download articles",
         fluidPage(theme="style.css",
                   column(12,
                          fluidRow(
                              h3("Download articles"),
                              actionButton(inputId = "DownloadArticles", label = "Download articles now", icon = icon("download"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")

                          )
                   )
         )),
widths = c(3, 8)
                    )
                    ),
                    server = function(input, output, session) {

                        ##### SetCastarter options ######
                        output$SetCastarter <- renderUI({
                            input$SetCastarter
                            shiny::isolate({
                                if (is.null(input$project)==FALSE&is.null(input$website)==FALSE) {
                                    if (input$project!="" & input$website!="") {
                                        SetCastarter(project = input$project, website = input$website)
                                        tryCatch(expr = CreateFolders(), warning = function(w) {
                                            if (grepl(pattern = "Permission denied", x = w) == TRUE) {
                                                print("Folders could not be created: permission denied.")
                                            }
                                        })
                                        if (file.exists(file.path(input$project, input$website))) {
                                            if (file.exists(file.path(input$project, input$website, "Logs", paste0(input$project, "-", input$website, "-parameters.rds")))) {

                                                importedParameters <- readRDS(file = file.path(input$project, input$website, "Logs", paste0(input$project, "-", input$website, "-parameters.rds")))
                                                updateTextInput(session = session, inputId = "CreateLinks_linkFirstChunk", value = importedParameters$CreateLinks$linkFirstChunk)
                                                updateTextInput(session = session, inputId = "CreateLinks_linkSecondChunk", value = importedParameters$CreateLinks$linkSecondChunk)
                                                updateTextInput(session = session, inputId = "CreateLinks_startPage", value = importedParameters$CreateLinks$startPage)
                                                updateTextInput(session = session, inputId = "CreateLinks_endPage", value = importedParameters$CreateLinks$endPage)
                                                updateTextInput(session = session, inputId = "CreateLinks_increaseBy", value = importedParameters$CreateLinks$increaseBy)
                                            }

                                            HTML(paste0("Name of project (", sQuote(input$project), ") and website (", sQuote(input$project), ") set for this session."))
                                        }
                                        # if (file.exists(file.path(input$project, input$website, "Logs", paste0(input$project, "-", input$website, "-parameters.rds")))) {
                                        #     HTML(paste0("Name of project (", sQuote(input$project), ") and website (", sQuote(input$project), ") set for this session.", "<br />Previously stored parameters for this project and website have been loaded for this session."))
                                        # }
                                    }
                                }
                            })
                        })


                        #### Select project/website ui ####

                        output$selectWebsite_UI <- renderUI({
                            if (nchar(input$availableProject)>0) {
                                selectInput(inputId = "availableWebsite", label = "Available websites", choices = as.list(c("", list.dirs(path = file.path("castarter", input$availableProject), full.names = FALSE, recursive = FALSE)), selected = ""))
                            }
                        })

                        output$selectProjectManually_UI <- renderUI({
                            textInput(inputId = "project", label = "Name of project", value = input$availableProject, width = NULL, placeholder = "e.g. News")
                        })

                        output$selectWebsiteManually_UI <- renderUI({
                            textInput(inputId = "website", label = "Name of website", value = input$availableWebsite, width = NULL, placeholder = "e.g. News")
                        })

                        ##### CreateLinks UI ####
                        output$CreateLinks_UI <- renderUI({

                            if (input$CreateLinks_RadioUI == "Numbers"){
                                fluidRow(class = "ReactiveUI",
                                         textInput(inputId = "CreateLinks_startPage", label = "First page", value = 1, placeholder = NULL),
                                         textInput(inputId = "CreateLinks_endPage", label = "Last page", value = 10, placeholder = NULL),
                                         textInput(inputId = "CreateLinks_increaseBy", label = "Increase by", value = 1, placeholder = NULL)
                                )
                            } else if (input$CreateLinks_RadioUI == "Dates") {
                                fluidRow(
                                    radioButtons(inputId = "CreateLinks_dateFormat", label = "Date format, if index link includes a date", choices =
                                                     c("year-month-day" = "ymd",
                                                       "year-month" = "ym",
                                                       "year" = "y"), inline = TRUE),
                                    dateRangeInput(inputId = "CreateLinks_DateRange",
                                                   label = "Select date range"),
                                    textInput(inputId = "CrateLinks_dateSeparator", label = "Date separator", value = "-", width = "40px")
                                )
                            }
                        })


                        # Preview index links

                        output$PreviewIndexLinks_UI <- renderUI({
                            indexLinks <- castarter::CreateLinks(linkFirstChunk = input$CreateLinks_linkFirstChunk,
                                                                 linkSecondChunk = input$CreateLinks_linkSecondChunk,
                                                                 startPage = as.numeric(input$CreateLinks_startPage),
                                                                 endPage = as.numeric(input$CreateLinks_endPage),
                                                                 increaseBy = as.numeric(input$CreateLinks_increaseBy),
                                                                 dateFormat = input$CreateLinks_dateFormat,
                                                                 startDate = format(input$CreateLinks_DateRange[1]),
                                                                 endDate =  format(input$CreateLinks_DateRange[2]),
                                                                 dateSeparator = input$CrateLinks_dateSeparator,
                                                                 export = FALSE,
                                                                 exportParameters = FALSE)
                            HTML(paste("<b>First Links:</b><br />",
                                       paste("<a href='", head(x = indexLinks), "'>", head(x = indexLinks), "</a>",  collapse = "<br />")),
                                 "<b><br />Last Links:</b><br />",
                                 paste("<a href='", tail(x = indexLinks), "'>", tail(x = indexLinks), "</a>",  collapse = "<br />")
                            )
                        })


                        # Confirm settings index links

                        observeEvent(input$CreateLinks, {
                            indexLinks <- castarter::CreateLinks(linkFirstChunk = input$CreateLinks_linkFirstChunk,
                                                                 linkSecondChunk = input$CreateLinks_linkSecondChunk,
                                                                 startPage = as.numeric(input$CreateLinks_startPage),
                                                                 endPage = as.numeric(input$CreateLinks_endPage),
                                                                 increaseBy = as.numeric(input$CreateLinks_increaseBy),
                                                                 dateFormat = input$CreateLinks_dateFormat,
                                                                 startDate = format(input$CreateLinks_DateRange[1]),
                                                                 endDate =  format(input$CreateLinks_DateRange[2]),
                                                                 dateSeparator = input$CrateLinks_dateSeparator,
                                                                 export = FALSE,
                                                                 exportParameters = TRUE)
                        })



                        # observeEvent(input$SetCastarter, {
                        #     if (file.exists(file.path(input$project, input$website, "Logs", paste0(input$project, "-", input$website, "-parameters.rds")))) {
                        #
                        #         importedParameters <- readRDS(file = file.path(input$project, input$website, "Logs", paste0(input$project, "-", input$website, "-parameters.rds")))
                        #         updateTextInput(session = session, inputId = "CreateLinks_linkFirstChunk", value = importedParameters$CreateLinks$linkFirstChunk)
                        #         updateTextInput(session = session, inputId = "CreateLinks_startPage", value = importedParameters$CreateLinks$startPage)
                        #         updateTextInput(session = session, inputId = "CreateLinks_endPage", value = importedParameters$CreateLinks$endPage)
                        #         updateTextInput(session = session, inputId = "CreateLinks_increaseBy", value = importedParameters$CreateLinks$increaseBy)
                        #     }
                        #
                        # })

                        ### Download index pages ####

                        observeEvent(eventExpr = input$DownloadIndexPages, {

                            links <- indexLinks

                            # params that should be possible to set
                            wait <- 1

                            htmlFilePath <- file.path(input$project, input$website, "IndexHtml")
                            htmlFilesList <- list.files(htmlFilePath, full.names = TRUE)
                            # put them in order [equivalent to gtools::mixedorder()]
                            htmlFilesList <- htmlFilesList[stringr::str_extract(string = htmlFilesList, pattern = "[[:digit:]]+[[:punct:]]html") %>% stringr::str_sub(start = 1L, end = -6L) %>% as.integer() %>% order()]
                            htmlFileSize <- file.info(htmlFilesList)["size"]
                            articlesId <- 1:length(links)
                            # if (missingArticles == TRUE) {
                            articlesHtmlFilenamesInTheory <- file.path(htmlFilePath, paste0(articlesId, ".html"))
                            linksToDownload <- !is.element(articlesHtmlFilenamesInTheory, htmlFilesList)
                            #}
                            temp <- 1
                            n <- length(links[linksToDownload])
                            withProgress(message = 'Downloading index pages', value = 0, {
                                for (i in links[linksToDownload]) {
                                    articleId <- articlesId[linksToDownload][temp]
                                    try(utils::download.file(url = i, destfile = file.path(htmlFilePath, paste0(articleId, ".html")), method = "auto"))
                                    incProgress(1/n, detail = paste("Downloading index page", temp, "of", n, paste0("- id: ", articleId)))
                                    Sys.sleep(wait)
                                    temp <- temp + 1
                                }
                            })
                        })

                    })
}
