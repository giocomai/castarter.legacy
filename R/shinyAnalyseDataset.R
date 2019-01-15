#' Starts a Shiny app that facilitates analysing a `castarter` dataset through time series.
#'
#' Starts a Shiny app that facilitates analysing a `castarter` dataset through time series.
#'
#' @export
#' @examples
#' \dontrun{
#' AnalyseDataset()
#' }

AnalyseDataset <- function() {
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
    }
    if (requireNamespace("tidytext", quietly = TRUE)==FALSE) {
        stop("You need to install the `tidytext` package with `install.packages('tidytext')` to use this function.")
    }
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `DT` package with `install.packages('DT')` to use this function.")
    }
    shiny::shinyApp(ui = shiny::fluidPage(

        shiny::tags$head(
            shiny::tags$style(shiny::HTML("
                            .row {
                            margin-left: 0;
                            margin-right: 0;
                            }

                            .tab-content {
                            padding-top: 20px;
                            }
                            ")
            )
        ),

        # Application title
        shiny::titlePanel("Analyse dataset"),
        shiny::tabsetPanel(
            #### Beginning of select dataset page ####
            shiny::tabPanel("Select dataset(s)",
                            shiny::fluidPage(shiny::fluidRow(
                                shiny::column(8,
                                              shiny::uiOutput(outputId = "selectWebsite_UI")

                                ),
                                shiny::column(4,
                                              shiny::actionButton(inputId = "SetCastarter",
                                                                  label = "Select dataset",
                                                                  icon = shiny::icon("check"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 1.5em;")))
                                ,
                                shiny::fluidRow(
                                    shiny::uiOutput(outputId = "datasetInfo"),
                                    shiny::tableOutput("SummariseDataset_table")
                                )
                            )
            ),
            #### End of select dataset page ####

            #### Beginning of Analyse dataset page ####
            shiny::tabPanel("Analyse dataset",
                            shiny::fluidPage(shiny::column(3,
                                                           shiny::textInput(inputId = 'term',
                                                                            label = 'Terms to be analysed',
                                                                            value = ""),
                                                           shiny::radioButtons(inputId = "freq",
                                                                               label = NULL,
                                                                               choices = c("Absolute frequency",
                                                                                           "Relative frequency")),
                                                           shiny::sliderInput(inputId = "rollingAverage",
                                                                              label = "Apply rolling average for ... days",
                                                                              min = 1,
                                                                              max = 91,
                                                                              value = 91,
                                                                              round = TRUE),
                                                           shiny::uiOutput(outputId = "dateRangeInput_UI"),
                                                           shiny::actionButton("go", "Go!")
                            ),
                            shiny::column(9,
                                          shiny::fluidRow(
                                              shiny::plotOutput("freqPlot")
                                          )
                            ),
                            shiny::mainPanel(shiny::fluidRow(DT::dataTableOutput(outputId = "kwic")), width = 12)
                            )
            )
        )
    ),
    server = function(input, output) {

        dataset <- tibble::data_frame(NA) %>% na.omit()

        #### Select project/website ui ####

        output$selectWebsite_UI <- shiny::renderUI({

            projects <- list.dirs(path = "castarter", full.names = FALSE, recursive = FALSE)

            projectsAndWebsites_list <- purrr::map(.x = projects,
                                                   .f = ~stringr::str_remove(string = list.dirs(path = file.path("castarter", .x),
                                                                                                full.names = TRUE,
                                                                                                recursive = FALSE),
                                                                             pattern = "castarter/"))
            names(projectsAndWebsites_list) <- projects

            shiny::selectizeInput(inputId = "selected_websites",
                                  label = "Available projects and websites",
                                  choices = as.list(c("", projectsAndWebsites_list)),
                                  selected = "",
                                  multiple = TRUE,
                                  width = "95%")

        })

        output$datasetInfo <- shiny::renderUI({
            if (nrow(dataset)==0) {
                shiny::HTML(text = "Select a dataset from the list above and click on 'Select dataset' to load it in the current session.")
            } else {
                shiny::HTML(text = paste0("The following datasets are now loaded in the current section:<br>", paste(unique(dataset$website), collapse = ", ")))
            }
        })

        ##### Load rds after selection ######

        shiny::observeEvent(input$SetCastarter, {

            dataset <<- shiny::withProgress(expr = {LoadDatasets(projectsAndWebsites = input$selected_websites,
                                                                 addProjectColumn = TRUE) %>%
                    tidytext::unnest_tokens(input = text,
                                            output = sentence,
                                            token = "sentences",
                                            to_lower = FALSE)},
                    message = "Loading dataset(s)... please wait")

            output$datasetInfo <- shiny::renderUI({
                if (nrow(dataset)==0) {
                    shiny::HTML(text = "Select a dataset from the list above and click on 'Select dataset' to load it in the current session.")
                } else {
                    shiny::HTML(text = "The following datasets are now loaded in the current session:<br>")
                }


            })

            output$SummariseDataset_table <- shiny::renderTable(
                castarter::SummariseDataset(dataset = dataset) %>%
                    dplyr::mutate(From = as.character(From),
                                  Until = as.character(Until),
                                  Total = scales::number(as.integer(Total)))
            )

            output$dateRangeInput_UI <- renderUI({shiny::dateRangeInput(inputId = "dateRange",
                                                                        label = "Date range",
                                                                        start = min(dataset$date),
                                                                        end = max(dataset$date),
                                                                        weekstart = 1)})
        })


        #### End of select project/website ui ####


        tsGG <- shiny::eventReactive(input$go, {
            if (input$freq=="Absolute frequency") {
                castarter::ShowAbsoluteTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                          dataset = dataset,
                                          type = "graph",
                                          rollingAverage = input$rollingAverage,
                                          startDate = input$dateRange[1],
                                          endDate = input$dateRange[2])

            } else if (input$freq=="Relative frequency") {
                castarter::ShowRelativeTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                          dataset = dataset,
                                          type = "graph",
                                          rollingAverage = input$rollingAverage,
                                          startDate = input$dateRange[1],
                                          endDate = input$dateRange[2])
            }
        })

        output$freqPlot <- shiny::renderPlot({
            if (input$go==0) {

            } else {
                tsGG()
            }
        })

        kwic_react <- shiny::eventReactive(input$go, {
            temp <- dataset %>%
                dplyr::filter(date>input$dateRange[1], date<input$dateRange[2]) %>%
                dplyr::filter(stringr::str_detect(string = sentence,
                                                  pattern = stringr::regex(ignore_case = TRUE,
                                                                           pattern = paste(as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))), collapse = "|")))) %>%
                dplyr::mutate(Source = paste0("<a target='_blank' href='", link, "'>", title, "</a><br />")) %>%
                dplyr::rename(Sentence = sentence, Date = date) %>%
                dplyr::select(Date, Source, Sentence) %>%
                dplyr::arrange(desc(Date))

            if (length(as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))))==1) {
                temp$Sentence <- purrr::map_chr(.x = temp$Sentence,
                                                .f = function (x)
                                                    paste(c(rbind(as.character(stringr::str_split(string = x,
                                                                                                  pattern = stringr::regex(pattern = as.character(input$term), ignore_case = TRUE), simplify = TRUE)),
                                                                  c(paste0("<span style='background-color: #FFFF00'>",
                                                                           as.character(stringr::str_extract_all(string = x,
                                                                                                                 pattern = stringr::regex(as.character(input$term),
                                                                                                                                          ignore_case = TRUE),
                                                                                                                 simplify = TRUE)),
                                                                           "</span>"), ""))),
                                                          collapse = ""))
            }
            temp
        })

        # Renders table at the bottom of the main tab
        output$kwic <- DT::renderDataTable({
            DT::datatable(data = kwic_react(),
                          options = list(pageLength = 3,
                                         lengthMenu = c(3, 5, 10, 15, 20)),
                          escape = FALSE, rownames = FALSE)
        })
    })
}
