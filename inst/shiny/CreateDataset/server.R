library("shiny")
library("castarter")

shinyServer(function(input, output) {

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
                        HTML(paste0("Name of project (", sQuote(input$project), ") and website (", sQuote(input$project), ") set for this session."))
                    }
                }
            }
        })
    })

    ### Select website ui

    output$selectWebsite_UI <- renderUI({
        if (nchar(input$availableProject)>0) {
            selectInput(inputId = "availableWebsite", label = "Available websites", choices = as.list(c("", list.dirs(path = file.path(input$availableProject), full.names = FALSE, recursive = FALSE)), selected = ""))
        }
    })

    output$selectProjectManually_UI <- renderUI({
        textInput(inputId = "project", label = "Name of project", value = input$availableProject, width = NULL, placeholder = "e.g. News")
    })

    output$selectWebsiteManually_UI <- renderUI({
        textInput(inputId = "website", label = "Name of website", value = input$availableWebsite, width = NULL, placeholder = "e.g. News")
    })

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
        HTML(paste("<b>First Links:</b>",
            paste("<a href='", head(x = indexLinks), "'>", head(x = indexLinks), "</a>",  collapse = "<br />")),
            "<b><br />Last Links:</b>",
            paste("<a href='", tail(x = indexLinks), "'>", tail(x = indexLinks), "</a>",  collapse = "<br />")
        )
    })


})


