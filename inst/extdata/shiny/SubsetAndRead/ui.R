library("shiny")
library("castarter")

shinyUI(fluidPage(
    titlePanel("Subset and read documents"),

    tabsetPanel(

        #### Beginning of SetCastarter page ####
        tabPanel("Select dataset(s)",
                 fluidPage(theme="style.css",
                           fluidRow(
                           column(8,
                               uiOutput(outputId = "selectWebsite_UI")

                           ),
                           column(4,
                               actionButton(inputId = "SetCastarter",
                                            label = "Select dataset",
                                            icon = icon("check"),
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 1.5em;")))
                          ,
                          fluidRow(
                           uiOutput(outputId = "datasetInfo"),
                           tableOutput("SummariseDataset_table")
                          )
                 )
        ),
        #### End of SetCastarter page ####

        #### Beginning of CreateLinks page ####
        tabPanel("Read and tag",
                 fluidPage(theme="style.css",

                           column(3,
                                  htmlOutput("totalItems"),
                                  splitLayout(
                                      uiOutput(outputId = "filterRangeSelector_ui"),
                                      uiOutput("filterResetButton")
                                  ),
                                  radioButtons(inputId = "showRadio", label = NULL, choices = c("All", "Only tagged", "Only untagged"), selected = "All", inline = TRUE),
                                  # actionButton("previousButton", "Previous"),
                                  # actionButton("nextButton", "Next"),
                                  # check: http://stackoverflow.com/questions/38302682/next-button-in-a-r-shiny-app
                                  textInput("pattern", "Pattern to be highlighted", value = ""),
                                  checkboxInput(inputId = "highlightDigits", label = "Highlight digits"),
                                  uiOutput("tagSelector"),
                                  uiOutput("typeSelector"),
                                  uiOutput("categorySelector"),
                                  actionButton("submit", "Submit"),
                                  htmlOutput("previousTags"),
                                  checkboxInput(inputId = "filterCheckbox", label = "Filter", value = FALSE, width = NULL),
                                  # Only show this panel if Filter is selected
                                  uiOutput("filterPattern"),
                                  uiOutput("filterTagSelector"),
                                  uiOutput("filterTypeSelector"),
                                  uiOutput("filtercategorySelector"),
                                  uiOutput("filterAndOrRadio"),
                                  uiOutput("invertFilterCheckbox"),
                                  uiOutput("filterActionButton")),
                           column(9,
                                  fluidRow(
                                      textOutput("UpdateDataset"),
                                      htmlOutput("id"),
                                      h2(textOutput("title")),
                                      textOutput("date"),
                                      htmlOutput("link"),
                                      htmlOutput("contents")
                                  )
                           )
                 )
        ),
        #### End of CreateLinks page ####

        #### Download index page ####
        tabPanel("Analyse tags",
                 fluidPage(theme="style.css",
                           column(12,
                                  fluidRow(
                                      h3("Analyse tags")
                                  )
                           )
                 ))
    )
))
