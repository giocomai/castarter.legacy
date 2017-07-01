library("shiny")
library("castarter")

shinyServer(function(input, output) {

    # CreateLinks UI
    output$CreateLinks_UI <- renderUI({
        if(input$CreateLinks_RadioUI == "Numbers"){
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
                dateRangeInput(inputId = "CreateLinks_DateRange", label = "Select date range")
            )
        }
    })



})
