library("shiny")
library("castarter")

shinyUI(fluidPage(
    titlePanel("Create a textual dataset from a website"),

    navlistPanel(
        #### End of CreateLinks page ####
        tabPanel("Create index links",
                 fluidPage(theme="style.css",

                     column(6,
                            h3("Introduce parameters"),
                         textInput(inputId = "CreateLinks_linkFirstChunk", label = "First part of the link", value = "", width = NULL, placeholder = "e.g. http://www.example.com/news/"),
                         textInput(inputId = "CreateLinks_linkSecondChunk", label = "Part of the link to append after varying part", value = NULL, width = NULL, placeholder = "e.g. .html"),
                         radioButtons(inputId = "CreateLinks_RadioUI", label = "Links based on numbers or dates?", choices = c("Numbers", "Dates"), inline = TRUE),
                         ## Reactive CreateLinks UI
                         uiOutput("CreateLinks_UI")),

                     column(6,

                     fluidRow(
                         h3("Preview of generated links")
                     )
                     )
                 )
        ),
        #### End of CreateLinks page ####
        tabPanel("Component 2"),
        tabPanel("Component 3"),
        tabPanel("Component 4"),
        tabPanel("Component 5"),
        widths = c(3, 8)
    )
))
