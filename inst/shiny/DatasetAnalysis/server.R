library("shiny")
library("stringr")
library("scales")
library("DT")

shinyServer(function(input, output) {


  tsGG <- eventReactive(input$go, {
      if (input$freq=="Absolute frequency") {
          castarter::ShowAbsoluteTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                    dataset = dataset,
                                    type = "graph",
                                    rollingAverage = input$rollingAverage, startDate = input$dateRange[1], endDate = input$dateRange[2])

      } else if (input$freq=="Relative frequency") {
          castarter::ShowRelativeTS(terms = as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))),
                                    dataset = dataset,
                                    type = "graph",
                                    rollingAverage = input$rollingAverage, startDate = input$dateRange[1], endDate = input$dateRange[2])
      }
  })

  output$freqPlot <- renderPlot({
    if (input$go==0) {
        castarter::ShowAbsoluteTS(terms = as.character(tolower(trimws(stringr::str_split(string = exampleTerms, pattern = ",", simplify = TRUE)))),
                                  dataset = dataset,
                                  type = "graph",
                                  rollingAverage = 91)
    } else {
      tsGG()
    }
  })

  kwic_react <- eventReactive(input$go, {
      dataset_bySentence %>%
          filter(date>input$dateRange[1], date<input$dateRange[2]) %>%
          filter(stringr::str_detect(string = sentence,
                                     pattern = stringr::regex(ignore_case = TRUE,
                                                              pattern = paste(as.character(tolower(trimws(stringr::str_split(string = input$term, pattern = ",", simplify = TRUE)))), collapse = "|")))) %>%
          mutate(Source = paste0("<a target='_blank' href='", link, "'>", title, "</a><br />")) %>%
          rename(Sentence = sentence, Date = date) %>%
          select(Date, Source, Sentence) %>%
          arrange(desc(Date))
  })

  output$kwic <- DT::renderDataTable({
      if (input$go==0) {
          DT::datatable(data = dataset_bySentence %>%
                            filter(stringr::str_detect(string = sentence,
                                                       pattern = stringr::regex(ignore_case = TRUE,
                                                                                pattern = paste(as.character(tolower(trimws(stringr::str_split(string = exampleTerms, pattern = ",", simplify = TRUE)))), collapse = "|")))) %>%
                            mutate(Source = paste0("<a target='_blank' href='", link, "'>", title, "</a><br />")) %>%
                            rename(Sentence = sentence, Date = date) %>%
                            select(Date, Source, Sentence) %>%
                            arrange(desc(Date)),
                        options = list(pageLength = 3, lengthMenu = c(3, 5, 10, 15, 20)),
                        escape = FALSE, rownames = FALSE)
      } else {
          DT::datatable(data = kwic_react(), options = list(pageLength = 3, lengthMenu = c(3, 5, 10, 15, 20)),
                        escape = FALSE, rownames = FALSE)
      }

  })
})

