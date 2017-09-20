library("shiny")
library("castarter")

shinyUI(fluidPage(
    titlePanel("Create a textual dataset from a website"),

    navlistPanel(
        #### Beginning of SetCastarter page ####
        tabPanel("Set up project",
                 fluidPage(theme="style.css",
                           selectInput(inputId = "availableProject", label = "Available projects", choices = as.list(c("", list.dirs(full.names = FALSE, recursive = FALSE)), selected = "")),
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
        tabPanel("Component 3"),
        tabPanel("Component 4"),
        tabPanel("Component 5"),
        widths = c(3, 8)
    )
))
