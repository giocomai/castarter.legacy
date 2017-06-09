#' Creates and starts a Shiny App exposing basic 'castarter' functions based on the given dataset
#'
#' Creates and starts a Shiny App exposing basic 'castarter' functions based on the given dataset
#'
#' @param dataset A 'castarter' dataset or a corpus created with 'castarter' of the quanteda type.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return Nothing, used for side effects: a shiny app is created in the Outputs folder.
#' @export
#' @examples
#' CreateShinyApp(dataset)
CreateShinyApp <- function(dataset, terms = "europ*", customTitle = NULL, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (!file.exists(file.path(project, website, "Outputs", "shinyApp"))) {
        dir.create(file.path(project, website, "Outputs", "shinyApp"))
    }
    if (!file.exists(file.path(project, website, "Outputs", "shinyApp", "data"))) {
        dir.create(file.path(project, website, "Outputs", "shinyApp", "data"))
    }
    if (quanteda::is.corpus(dataset)==TRUE) {
        corpus <- dataset
    } else {
        corpus <- castarter::CreateCorpus(dataset = dataset, quanteda = FALSE)
    }
    corpusDtm <- castarter::CreateDtm(corpus = corpus)
    rm(dataset)
    save(list = c("corpus", "corpusDtm"), file = file.path(project, website, "Outputs", "shinyApp", "data", "dataset.RData"))
if (is.null(x = customTitle)==TRUE) {
    customTitle <- website
}
    write(x = paste0(
    "library('shiny')
library('ggplot2')
library('castarter')
library('stringi')
# required for ggplot stat_smooth
library('mgcv')
SetCastarter(project = '", project, "', website = '", website,"')",
"
# load dataset
load(file = file.path('data', 'dataset.RData'))
# find earliest and latest date
dailyFreq <- data.frame(docs = quanteda::docnames(corpusDtm), base::as.data.frame(corpusDtm))
dailyFreq <- tidyr::separate(data = dailyFreq, col = docs, into = c('Date','website'), sep = '\\\\.')
dailyFreq$Date <- as.Date(dailyFreq$Date)
minDate <- min(dailyFreq$Date)
maxDate <- max(dailyFreq$Date)

ui <- fluidPage(
    # Application title
    titlePanel('", customTitle, "'),
    sidebarLayout(
        #inputs
        sidebarPanel(
            radioButtons(inputId = 'graphType', label = 'Select type of graph', choices = c('Time series', 'Barchart')),
            textInput(inputId = 'term', label = 'Terms to be analysed*', value = '", terms, "'),
            conditionalPanel(
                condition = \"input.graphType == 'Time series'\",
                dateInput(inputId = 'startDate', label = 'startDate', value = minDate, min = minDate, max = maxDate, format = 'yyyy-mm-dd', startview = 'year', weekstart = 0, language = 'en', width = NULL),
                dateInput(inputId = 'endDate', label = 'endDate', value = maxDate, min = minDate, max = maxDate, format = 'yyyy-mm-dd', startview = 'year', weekstart = 0, language = 'en', width = NULL),
                sliderInput(inputId = 'rollingAvg',
                            label = 'Rolling average (days)',
                            value = 90, min = 1, max = 90),
                checkboxInput(inputId = 'smoothLine', label = 'Smooth', value = FALSE),
                checkboxInput(inputId = 'trendLine', label = 'Linear smooth', value = FALSE),
                checkboxInput(inputId = 'dygraphs', label = 'Show interactive time series', value = FALSE),
checkboxInput(inputId = 'kwic', label = 'Show keywords in context', value = FALSE),
p('* If more than one, terms must be comma-separated; to merge mutliple terms in calculations, use the format: Russia=Russia*+Moscow+Putin, Europe=Europ*+EU+Brussels')
            ),
conditionalPanel(
condition = \"input.graphType == 'Barchart'\",
radioButtons(inputId = 'barchartType', label = 'Select type of barchart', choices = c('Barchart by website', 'Barchart by year and website', 'Barchart by year (merge multiple sources)', 'Barchart (merge multiple sources)')),
radioButtons(inputId = 'relFreq', label = 'Frequency', choices = c('Relative frequency', 'Absolute number of occurrences'))
)
        ),
        mainPanel(
            plotOutput('graph'),
dygraphs::dygraphOutput('dygraphs')
        )
    ),
    fluidRow(dataTableOutput('kwic'))
)

server <- function(input, output) {
    output$graph <- reactivePlot(function() {
        specificTerms <- stringi::stri_trim_both(unlist(strsplit(x = input$term, split = ',')))
if (grepl(pattern = '+', x = specificTerms, fixed = TRUE)) {
specificTerms <- castarter::CreateDictionary(terms = specificTerms)
}
        if (input$graphType == 'Time series') {
            graph <- ShowTS(terms = specificTerms, corpus = corpus, corpusDtm = corpusDtm, startDate = input$startDate, endDate = input$endDate, rollingAverage = input$rollingAvg)
            if (input$smoothLine == TRUE) {
                graph <- graph + ggplot2::stat_smooth(size = 1.2)
            }
            if (input$trendLine == TRUE) {
                graph <- graph + ggplot2::stat_smooth(size = 1.2, method = 'lm')
            }
        } else if (input$graphType == 'Barchart') {
if (input$relFreq=='Relative frequency') {
relFreq <- TRUE
} else {
relFreq <- FALSE
}
if (input$barchartType == 'Barchart (merge multiple sources)') {
graph <- ShowFreq(corpusDtm = corpusDtm, mode = 'barchart', terms = specificTerms, byDate = FALSE, byWebsite = FALSE, relFreq = relFreq)
} else if (input$barchartType == 'Barchart by year (merge multiple sources)') {
            graph <- ShowFreq(corpusDtm = corpusDtm, mode = 'barchart', terms = specificTerms, byDate = TRUE, invert = TRUE, relFreq = relFreq)
} else if (input$barchartType == 'Barchart by website') {
graph <- ShowFreq(corpusDtm = corpusDtm, mode = 'barchart', terms = specificTerms, byDate = FALSE, byWebsite = TRUE, invert = TRUE, relFreq = relFreq)
} else if (input$barchartType == 'Barchart by year and website') {
graph <- ShowFreq(corpusDtm = corpusDtm, mode = 'barchart', terms = specificTerms, byDate = TRUE, byWebsite = TRUE, invert = TRUE, relFreq = relFreq)
}
}
        print(graph)
    }
    )
    output$kwic <- renderDataTable({
        if (input$kwic == TRUE) {
            stringKwic <- stringi::stri_trim_both(unlist(strsplit(x = input$term, split = ',')))
            stringKwic <- paste(stringKwic, collapse = '|')
      context <- quanteda::kwic(x = corpus, keywords = stringKwic, window = 7, case_insensitive = TRUE)
      id <- stringr::str_extract(string = as.character(context$docname), pattern = ' - [[:digit:]]+')
      id <- as.numeric(gsub(pattern = ' - ', replacement = '', x = id))
      idFilter <- match(x = id, table = quanteda::docvars(corpus)$ID)
      links <- as.character(quanteda::docvars(corpus)$links[idFilter])
      titles <- as.character(quanteda::docvars(corpus)$title[idFilter])
      data.frame(Date = stringi::stri_sub(str = context$docname, from = 1, to = 10), context[3:5], Source = paste0('<a href=\"', links, '\", target=\"_blank\">', titles, '</a>'))
      }
      }, escape = FALSE
      )
output$dygraphs <- dygraphs::renderDygraph({
        if (input$dygraphs==TRUE) {
        specificTerms <- stringi::stri_trim_both(unlist(strsplit(x = input$term, split = ',')))
        ShowTS(terms = specificTerms, corpus = corpus, corpusDtm = corpusDtm, startDate = input$startDate, endDate = input$endDate, rollingAverage = input$rollingAvg, dygraphs = TRUE)
        }
    })
      }

      shinyApp(ui = ui, server = server)"), file = file.path(project, website, "Outputs", "shinyApp", "app.R"))
shiny::runApp(file.path(project, website, "Outputs", "shinyApp"), display.mode = "normal")
}
