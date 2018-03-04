library("shiny")
library("DT")

shinyUI(fluidPage(

  # Application title
  titlePanel("Frequency of terms in press releases posted on Kremlin.ru (English version)"),
  sidebarLayout(
    sidebarPanel(
        textInput(inputId = 'term', label = 'Terms to be analysed', value = 'crimea, syria'),
        radioButtons(inputId = "freq", label = NULL, choices = c("Absolute frequency", "Relative frequency")),
        sliderInput(inputId = "rollingAverage", label = "Apply rolling average for ... days", min = 1, max = 91, value = 91, round = TRUE),
        dateRangeInput(inputId = "dateRange", label = "Date range", start = min(dataset$date), end = max(dataset$date), weekstart = 1),
        actionButton("go", "Go!")
    ),

    mainPanel(
       plotOutput("freqPlot")

       )

    ),
  mainPanel(fluidRow(DT::dataTableOutput(outputId = "kwic")), width = 12)
  )
)
