library("shiny")
library("castarter")

shinyUI(fluidPage(
    titlePanel("Create a textual dataset from a website"),

    navlistPanel(
        tabPanel("Create index links"),
        fluidRow(h2("Create index links")),
        fluidRow(
            textInput(inputId = "CreateLinks_linkFirstChunk", label = "First part of the link", value = "", width = NULL, placeholder = "e.g. http://www.example.com/news/"),
            textInput(inputId = "CreateLinks_linkSecondChunk", label = "Part of the link to append after varying part", value = NULL, width = NULL, placeholder = "e.g. .html"),
            splitLayout(
            textInput(inputId = "CreateLinks_startPage", label = "First page", value = 1, width = 80, placeholder = NULL),
            textInput(inputId = "CreateLinks_endPage", label = "Last page", value = 10, width = 80, placeholder = NULL),
            textInput(inputId = "CreateLinks_increaseBy", label = "Increase by", value = 1, width = 80, placeholder = NULL)
            ),
            radioButtons(inputId = "CreateLinks_dateFormat", label = "Date format, if index link includes a date", choices =
                             c("year-month-day" = "ymd",
                               "year-month" = "ym",
                               "year" = "y"), selected = character(0), inline = TRUE),
            dateRangeInput(inputId = "CreateLinks_DateRange", label = "Select date range")
            ),
        tabPanel("Component 2"),
        tabPanel("Component 3"),
        tabPanel("Component 4"),
        tabPanel("Component 5")
    )
))
