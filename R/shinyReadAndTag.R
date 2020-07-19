#' Starts a Shiny app that facilitates sub-setting, reading, and tagging a `castarter` dataset.
#'
#' Starts a Shiny app that facilitates sub-setting, reading, and tagging a `castarter` dataset.
#'
#'
#' @param additional_links Logical, defaults to FALSE. If TRUE, adds direct links to the Archive.org version of the current web page, as well as to save a current snapshot of the page on archive.org.
#' @export
#' @examples
#' \dontrun{
#' ReadAndTag()
#' }

ReadAndTag <- function(additional_links = FALSE) {
    if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
        stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
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
                            "))
        ),


        shiny::titlePanel("Subset and read documents"),
        shiny::tabsetPanel(
            #### Beginning of SetCastarter page ####
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
            #### End of SetCastarter page ####

            #### Beginning of Read and tag page ####
            shiny::tabPanel("Read and tag",
                            shiny::fluidPage(shiny::column(3,
                                                           shiny::htmlOutput("totalItems"),
                                                           shiny::splitLayout(
                                                               shiny::uiOutput(outputId = "filterRangeSelector_ui"),
                                                               shiny::uiOutput("filterResetButton")
                                                           ),
                                                           shiny::radioButtons(inputId = "showRadio", label = NULL, choices = c("All", "Only tagged", "Only untagged"), selected = "All", inline = TRUE),
                                                           # actionButton("previousButton", "Previous"),
                                                           # actionButton("nextButton", "Next"),
                                                           # check: http://stackoverflow.com/questions/38302682/next-button-in-a-r-shiny-app
                                                           shiny::textInput("pattern", "Pattern to be highlighted", value = ""),
                                                           shiny::checkboxInput(inputId = "highlightDigits", label = "Highlight digits"),
                                                           shiny::uiOutput("tagSelector"),
                                                           shiny::uiOutput("typeSelector"),
                                                           shiny::uiOutput("categorySelector"),
                                                           shiny::actionButton("submit", "Submit"),
                                                           shiny::htmlOutput("previousTags"),
                                                           shiny::checkboxInput(inputId = "filterCheckbox", label = "Filter", value = FALSE, width = NULL),
                                                           # Only show this panel if Filter is selected
                                                           shiny::uiOutput("filterPatternTitle"),
                                                           shiny::uiOutput("filterPatternText"),
                                                           shiny::uiOutput("filterTagSelector"),
                                                           shiny::uiOutput("filterTypeSelector"),
                                                           shiny::uiOutput("filtercategorySelector"),
                                                           shiny::uiOutput("filterAndOrRadio"),
                                                           shiny::uiOutput("invertFilterCheckbox"),
                                                           shiny::uiOutput("filterActionButton")),
                                             shiny::column(9,
                                                           shiny::fluidRow(
                                                               shiny::textOutput("UpdateDataset"),
                                                               shiny::htmlOutput("id"),
                                                               shiny::h2(shiny::textOutput("title")),
                                                               shiny::textOutput("date"),
                                                               shiny::htmlOutput("link"),
                                                               shiny::htmlOutput("contents")
                                                           )
                                             )
                            )
            ),

            shiny::tabPanel("Analyse tags",
                            shiny::fluidPage(theme="style.css",
                                             shiny::column(12,
                                                           shiny::fluidRow(
                                                               shiny::h3("Analyse tags")
                                                           )
                                             )
                            )),
            shiny::tabPanel("Export tags",
                            shiny::fluidPage(theme="style.css",
                                             shiny::column(12,
                                                           shiny::fluidRow(
                                                               shiny::h3("Export tags")
                                                           )
                                             )
                            ))
        )
    ), server = function(input, output, session) {
        allTags <- tags <- tibble::tibble(doc_id = NA,  tag = NA, category = NA, type = NA)
        dataset <- tibble::tibble(NA) %>% na.omit()

        #### Select project/website ui ####

        output$selectWebsite_UI <- shiny::renderUI({

            projects <- list.dirs(path = "castarter",
                                  full.names = FALSE,
                                  recursive = FALSE)

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

        #### End of select project/website ui ####

        output$tagSelector <- shiny::renderUI({
            shiny::selectizeInput("tag", label = "Tag", choices =
                                      if (is.null(unlist(allTags$tag))) {
                                          NULL
                                      } else {
                                          tibble::tibble(tag = unlist(allTags$tag)) %>%
                                              dplyr::group_by(tag) %>%
                                              dplyr::tally() %>%
                                              dplyr::arrange(desc(n)) %>%
                                              na.omit() %>%
                                              dplyr::pull(tag)},
                                  multiple = TRUE,
                                  options = list(create = TRUE, placeholder = "Insert tag"))
        })

        output$typeSelector <- shiny::renderUI({
            shiny::selectizeInput("type", label = "Type", choices =
                                      if (is.null(unlist(allTags$type))) {
                                          NULL
                                      } else {
                                          tibble::tibble(type = unlist(allTags$type)) %>%
                                              dplyr::group_by(type) %>%
                                              dplyr::tally() %>%
                                              dplyr::arrange(desc(n)) %>%
                                              na.omit() %>%
                                              dplyr::pull(type)},
                                  multiple = TRUE,
                                  options = list(create = TRUE, placeholder = "Insert type"))


        })

        output$categorySelector <- shiny::renderUI({
            shiny::selectizeInput("category", label = "Category", choices =
                                      if (is.null(unlist(allTags$category))) {
                                          NULL
                                      } else {
                                          tibble::tibble(category = unlist(allTags$category)) %>%
                                              dplyr::group_by(category) %>%
                                              dplyr::tally() %>%
                                              dplyr::arrange(desc(n)) %>%
                                              na.omit() %>%
                                              dplyr::pull(category)},
                                  multiple = TRUE,
                                  options = list(create = TRUE,
                                                 placeholder = "Insert category"))
        })



        ##### Load rds after selection ######

        shiny::observeEvent(input$SetCastarter, {

            dataset <<- shiny::withProgress(expr = {LoadDatasets(projectsAndWebsites = input$selected_websites, addProjectColumn = TRUE)},
                                            message = "Loading dataset(s)... please wait")

            for (i in file.path("castarter", input$selected_websites, "Logs", "tags.rds")[file.exists(file.path("castarter", input$selected_websites, "Logs", "tags.rds"))==FALSE]) {
                fs::dir_create(path = fs::path("castarter", input$selected_websites, "Logs"))
                tibble::tibble(doc_id = dataset$doc_id[input$id],  tag = list(input$tag), category = list(input$category), type = list(input$type))

                readr::write_rds(x = tibble::tibble(doc_id = NA,
                                                    tag = list(NA),
                                                    category = list(NA),
                                                    type=list(NA)) %>%
                                     dplyr::filter(is.na(doc_id)==FALSE),
                                 path = i)
            }

            allTags <<- purrr::map_df(.x = file.path("castarter", input$selected_websites, "Logs", "tags.rds"), .f = readr::read_rds)

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
        })


        ##### UI read and tag ######

        output$id <- shiny::renderText({
            paste("<b>id</b>:", dataset$doc_id[input$id])
        })

        output$title <- shiny::renderText({
            dataset$title[input$id]
        })

        output$date <- shiny::renderText({
            as.character(as.Date(dataset$date[input$id]))
        })

        output$link <- shiny::renderText({
            if (additional_links == TRUE) {
                paste0("Original URL: ", '<a target="_blank" href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>', "<br />",
                       '<a target="_blank" href="', "https://web.archive.org/", dataset$link[input$id], '">', "View on Archive.org", '</a>', " - ",
                       '<a target="_blank" href="', "https://web.archive.org/save/", dataset$link[input$id], '">', "Save on Archive.org", '</a>', " - ",
                       '<a target="_blank" href="', "https://translate.google.com/translate?sl=auto&tl=en&u=", dataset$link[input$id], '">', "Open in Google Translate", '</a>', "<br /><br />")
            } else {
                paste0('<a target="_blank" href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>')
            }
        })

        output$contents <- shiny::renderText({

            if (input$pattern==""&input$highlightDigits==FALSE) {
                dataset$text[input$id]
            } else {

                if (input$highlightDigits==TRUE) {
                    if (input$pattern=="") {
                        patternToHighlight <- "[[:digit:]]+"
                    } else {
                        patternToHighlight <- paste0(as.character(input$pattern),
                                                     "|[[:digit:]]")
                    }

                } else {
                    patternToHighlight <- as.character(input$pattern)
                }
                paste(c(rbind(as.character(stringr::str_split(string = dataset$text[input$id],
                                                              pattern = stringr::regex(pattern = patternToHighlight, ignore_case = TRUE),
                                                              simplify = TRUE)),
                              c(paste0("<span style='background-color: #FFFF00'>",
                                       as.character(stringr::str_extract_all(string = dataset$text[input$id],
                                                                             pattern = stringr::regex(patternToHighlight, ignore_case = TRUE),
                                                                             simplify = TRUE)),
                                       "</span>"), ""))),
                      collapse = "")
            }

        })

        #### Submit tags ####

        shiny::observeEvent(input$submit, {

            if (file.exists(file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds")) == FALSE) {
                saveRDS(object = tibble::tibble(doc_id = NA, tag = list(NA), category = list(NA), type=list(NA)) %>%
                            dplyr::filter(is.na(doc_id)==FALSE),
                        file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))
            }
            tags <- readRDS(file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))
            if (is.null(nrow(tags))==FALSE) { #check that tags is not empty.
                if (is.element(el = dataset$doc_id[input$id], set = tags$doc_id)) { #checks if given item has already been added to tags: must have same id, same project name, and same website name
                    tags$tag[tags$doc_id==dataset$doc_id[input$id]] <- list(input$tag)
                    tags$category[tags$doc_id==dataset$doc_id[input$id]] <- list(input$category)
                    tags$type[tags$doc_id==dataset$doc_id[input$id]] <- list(input$type)
                } else {
                    tags <- dplyr::bind_rows(tags,
                                             tibble::tibble(doc_id = dataset$doc_id[input$id],
                                                                tag = list(input$tag),
                                                                category = list(input$category),
                                                                type = list(input$type))) %>%
                        dplyr::arrange(doc_id)
                }
            } else { #runs when *tags* is empty, adds first item
                tags <- tibble::tibble(doc_id = dataset$doc_id[input$id],  tag = list(input$tag), category = list(input$category), type = list(input$type))
            }
            saveRDS(object = tags, file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))

            output$previousTags <- shiny::renderText({
                tags <- readRDS(file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))
                paste("<b>tag</b>: ", paste(as.character(unlist(tags$tag[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                      "<b>category</b>: ", paste(as.character(unlist(tags$category[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                      "<b>type</b>: ", paste(as.character(unlist(tags$type[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "))
            })
        })

        output$totalItems <- shiny::renderText({
            paste("<b>Total items:</b>", nrow(dataset))
        })

        output$previousTags <- shiny::renderText({
            paste("<b>tag</b>: ", paste(as.character(unlist(allTags$tag[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                  "<b>category</b>: ", paste(as.character(unlist(allTags$category[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                  "<b>type</b>: ", paste(as.character(unlist(allTags$type[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "))
        })

        output$filterPatternText <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                shiny::tagList(
                    shiny::textInput(inputId = "patternFilterText",
                                     label = "Pattern to filter in text",
                                     value = ""),
                    shiny::checkboxInput(inputId = "patternFilterTextInvert",
                                         label = "Invert (i.e. text does not include pattern)",
                                         value = FALSE)
                )
            }
        })

        output$filterPatternTitle <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                shiny::tagList(
                    shiny::textInput(inputId = "patternFilterTitle",
                                     label = "Pattern to filter in title",
                                      value = ""),
                    shiny::checkboxInput(inputId = "patternFilterTitleInvert",
                                         label = "Invert (i.e. title does not include pattern)",
                                         value = FALSE)
                )
            }
        })

        output$filterTagSelector <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                selectInput('tagFilter', 'Filter by tag',
                            choices =  if (is.null(unlist(allTags$tag))) {
                                NULL
                            } else {
                                tibble::tibble(tag = unlist(allTags$tag)) %>%
                                    dplyr::arrange(tag) %>%
                                    dplyr::distinct() %>%
                                    dplyr::pull(tag)},
                            multiple=TRUE, selectize=FALSE)
            }
        })

        output$filtercategorySelector <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                selectInput('categoryFilter', 'Filter by category',
                            choices =  if (is.null(unlist(allTags$category))) {
                                NULL
                            } else {tibble::tibble(category = unlist(allTags$category)) %>%
                                    dplyr::arrange(category) %>%
                                    dplyr::distinct() %>%
                                    dplyr::pull(category)},
                            multiple=TRUE, selectize=FALSE)
            }
        })

        output$filterTypeSelector <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                selectInput('typeFilter', 'Filter by type',
                            choices =  if (is.null(unlist(allTags$type))) {
                                NULL
                            } else {tibble::tibble(type = unlist(allTags$type)) %>%
                                    dplyr::arrange(type) %>%
                                    dplyr::distinct() %>%
                                    dplyr::pull(type)},
                            multiple=TRUE, selectize=FALSE)
            }
        })

        output$filterAndOrRadio <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                shiny::radioButtons(inputId = "andOrFilter", label = "Keep items that belong to any of the above categories, or only those that belong to all of those selected?",
                                    choices = c("Any", "All"),
                                    selected = "Any",
                                    inline = TRUE)
            }
        })

        output$invertFilterCheckbox <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                shiny::checkboxInput(inputId = "invertFilter",
                                     label = "Invert filter?",
                                     value = FALSE)
            }
        })


        output$filterActionButton <- shiny::renderUI({
            if(input$filterCheckbox == TRUE){
                shiny::actionButton("filterAction", "Filter")
            }
        })

        output$filterRangeSelector_ui <- shiny::renderUI({
            numericInput(inputId = "id",
                         label = NULL,
                         value = 1,
                         min = 1,
                         max = nrow(dataset))
        })

        output$filterResetButton <- shiny::renderUI({
            actionButton("filterReset", "Reset filter")
        })

        shiny::observeEvent(input$showRadio, {
            if (input$showRadio=="Only untagged") {
                dataset <<- dataset[which(!is.element(el = dataset$doc_id, set = tags$doc_id)),]
            } else if (input$showRadio=="Only tagged") {
                dataset <<- dataset[which(is.element(el = dataset$doc_id, set = tags$doc_id)),]
            }
            # update input UI
            shiny::updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = if(exists("dataset")) nrow(dataset) else 100)
            output$totalItems <- shiny::renderText({
                paste("<b>Total items:</b>", nrow(dataset))
            })
        })

        ##### Filter #####

        shiny::observeEvent(input$filterAction, {
            # "Neutralising" NULL in tags
            tagsF <- allTags
            tagsF$tag[purrr::map_lgl(.x = allTags$tag, .f = is.null)] <- ""
            tagsF$category[purrr::map_lgl(.x = allTags$category, .f = is.null)] <- ""
            tagsF$type[purrr::map_lgl(.x = allTags$type, .f = is.null)] <- ""
            # if only one of the filters given, turn others into NA
            tagFilter <- if (is.null(input$tagFilter)) {NA} else {unlist(input$tagFilter)}
            categoryFilter <- if (is.null(input$categoryFilter)) {NA} else {unlist(input$categoryFilter)}
            typeFilter <- if (is.null(input$typeFilter)) {NA} else {unlist(input$typeFilter)}
            # Creating logical vector for filter
            if (is.null(input$patternFilterText)==FALSE&input$patternFilterText!="") {
                if (input$patternFilterTextInvert==FALSE) {
                    filterPatternL <- stringr::str_detect(string = dataset$text,
                                                          pattern = stringr::regex(pattern = input$patternFilterText,
                                                                                   ignore_case = TRUE))
                } else if (input$patternFilterTextInvert==TRUE) {
                    filterPatternL <- stringr::str_detect(string = dataset$text,
                                                          pattern = stringr::regex(pattern = input$patternFilterText,
                                                                                   ignore_case = TRUE),
                                                          negate = TRUE)

                }

                dataset <<- dataset <- dataset[filterPatternL,]
                #filterL <- Reduce("&", list(filterL, filterPattern))
            }

            if (is.null(input$patternFilterTitle)==FALSE&input$patternFilterTitle!="") {
                if (input$patternFilterTitleInvert==FALSE) {
                    filterPatternL <- stringr::str_detect(string = dataset$title,
                                                          pattern = stringr::regex(pattern = input$patternFilterTitle,
                                                                                   ignore_case = TRUE))
                } else if (input$patternFilterTitleInvert==TRUE) {
                    filterPatternL <- stringr::str_detect(string = dataset$title,
                                                          pattern = stringr::regex(pattern = input$patternFilterTitle,
                                                                                   ignore_case = TRUE),
                                                          negate = TRUE)

                }

                dataset <<- dataset <- dataset[filterPatternL,]
                #filterL <- Reduce("&", list(filterL, filterPattern))
            }


            if (length(input$tagFilter)<2&length(input$categoryFilter)<2&length(input$typeFilter)<2) {
                filterL <- list(tag = purrr::map_lgl(.x = tagsF$tag, .f = is.element, el = tagFilter),
                                category = purrr::map_lgl(.x = tagsF$category, .f = is.element, el = categoryFilter),
                                type = purrr::map_lgl(.x = tagsF$type, .f = is.element, el = typeFilter))[is.na(c(tagFilter, categoryFilter, typeFilter))==FALSE]
            } else {
                # enable filter when more than one selected
                tagL <- rep(x = FALSE, times = length(tagsF$tag))
                for (i in seq_along(tagFilter)) {
                    temp <- purrr::map_lgl(.x = tagsF$tag, .f = is.element, el = tagFilter[i])
                    tagL <- Reduce("|", list(temp, tagL))
                }
                categoryL <- rep(x = FALSE, times = length(tagsF$category))
                for (i in seq_along(categoryFilter)) {
                    temp <- purrr::map_lgl(.x = tagsF$category, .f = is.element, el = categoryFilter[i])
                    categoryL <- Reduce("|", list(temp, categoryL))
                }
                typeL <- rep(x = FALSE, times = length(tagsF$type))
                for (i in seq_along(typeFilter)) {
                    temp <- purrr::map_lgl(.x = tagsF$type, .f = is.element, el = typeFilter[i])
                    typeL <- Reduce("|", list(temp, typeL))
                }
                filterL <- list(tag = tagL, category = categoryL, type = typeL)[is.na(c(tagFilter, categoryFilter, typeFilter))==FALSE]
            }
            if (input$andOrFilter == "Any") {
                filterL <- Reduce("|", filterL)
            } else if (input$andOrFilter == "All") {
                filterL <- Reduce("&", filterL)
            }
            # Filter (checking if invert filter is enabled)

            if (length(filterL)>0) {
                if (input$invertFilter== TRUE) {
                    dataset <<- dataset <- dataset[which(!is.element(el = dataset$doc_id,
                                                                     set = allTags$doc_id[filterL])),]
                } else {
                    dataset <<- dataset <- dataset[which(is.element(el = dataset$doc_id,
                                                                    set = allTags$doc_id[filterL])),]
                }
            }

            # update input UI
            shiny::updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = nrow(dataset))
            output$totalItems <- shiny::renderText({
                paste("<b>Total items:</b>", nrow(dataset))
            })
            ## update news item shown
            output$id <- shiny::renderText({
                paste("<b>Project-website-id</b>:", dataset$doc_id[input$id])
            })

            output$title <- shiny::renderText({
                dataset$title[input$id]
            })

            output$date <- shiny::renderText({
                as.character(as.Date(dataset$date[input$id]))
            })

            output$link <- shiny::renderText({
                if (additional_links == TRUE) {
                    paste0("Original URL: ", '<a target="_blank" href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>', "<br />",
                           '<a target="_blank" href="', "https://web.archive.org/", dataset$link[input$id], '">', "View on Archive.org", '</a>', " - ",
                           '<a target="_blank" href="', "https://web.archive.org/save/", dataset$link[input$id], '">', "Save on Archive.org", '</a>', " - ",
                           '<a target="_blank" href="', "https://translate.google.com/translate?sl=auto&tl=en&u=", dataset$link[input$id], '">', "Open in Google Translate", '</a>', "<br /><br />")
                } else {
                    paste('<a target="_blank" href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>')
                }
            })

            output$contents <- shiny::renderText({
                if (input$pattern==""&input$highlightDigits==FALSE) {
                    dataset$text[input$id]
                } else {

                    if (input$highlightDigits==TRUE) {
                        if (input$pattern=="") {
                            patternToHighlight <- "[[:digit:]]+"
                        } else {
                            patternToHighlight <- paste0(as.character(input$pattern), "|[[:digit:]]")
                        }

                    } else {
                        patternToHighlight <- as.character(input$pattern)
                    }
                    paste(c(rbind(as.character(stringr::str_split(string = dataset$text[input$id],
                                                                  pattern = stringr::regex(pattern = patternToHighlight, ignore_case = TRUE), simplify = TRUE)),
                                  c(paste0("<span style='background-color: #FFFF00'>",
                                           as.character(stringr::str_extract_all(string = dataset$text[input$id], pattern = stringr::regex(patternToHighlight, ignore_case = TRUE), simplify = TRUE)),
                                           "</span>"), ""))),
                          collapse = "")
                }
            })

        })

        shiny::observeEvent(input$filterReset, {
            dataset <<- dataset <- withProgress(expr = {LoadDatasets(projectsAndWebsites = input$selected_websites, addProjectColumn = TRUE)},
                                                message = "Loading dataset(s)... please wait")
            # update input UI
            shiny::updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = nrow(dataset))
            output$totalItems <- shiny::renderText({
                paste("<b>Total items:</b>", nrow(dataset))
            })
            ## update news item shown
            output$id <- shiny::renderText({
                paste("<b>Project-website-id</b>:", dataset$doc_id[input$id])
            })

            output$title <- shiny::renderText({
                dataset$title[input$id]
            })

            output$date <- shiny::renderText({
                as.character(as.Date(dataset$date[input$id]))
            })

            output$link <- shiny::renderText({
                paste('<a target="_blank" href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>')
            })

            output$contents <- shiny::renderText({
                if (input$pattern==""&input$highlightDigits==FALSE) {
                    dataset$text[input$id]
                } else {

                    if (input$highlightDigits==TRUE) {
                        if (input$pattern=="") {
                            patternToHighlight <- "[[:digit:]]+"
                        } else {
                            patternToHighlight <- paste0(as.character(input$pattern), "|[[:digit:]]")
                        }

                    } else {
                        patternToHighlight <- as.character(input$pattern)
                    }
                    paste(c(rbind(as.character(stringr::str_split(string = dataset$text[input$id],
                                                                  pattern = stringr::regex(pattern = patternToHighlight, ignore_case = TRUE), simplify = TRUE)),
                                  c(paste0("<span style='background-color: #FFFF00'>",
                                           as.character(stringr::str_extract_all(string = dataset$text[input$id], pattern = stringr::regex(patternToHighlight, ignore_case = TRUE), simplify = TRUE)),
                                           "</span>"), ""))),
                          collapse = "")
                }
            })
        })

        output$UpdateDataset <- shiny::renderText({
            if (is.null(input$categoryFilter)==FALSE) {
                filterL <- purrr::map_lgl(.x = tags$category, .f = is.element, el = input$categoryFilter)
                which(is.element(el = dataset$doc_id, set = tags$doc_id[filterL]))
            }
        })

    })
}
