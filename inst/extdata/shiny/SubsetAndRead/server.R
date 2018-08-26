library("shiny")
library("castarter")

shinyServer(function(input, output, session) {


    #### Select project/website ui ####

    output$selectWebsite_UI <- renderUI({

        projects <- list.dirs(path = "castarter", full.names = FALSE, recursive = FALSE)

        projectsAndWebsites_list <- purrr::map(.x = projects,
                                               .f = ~stringr::str_remove(string = list.dirs(path = file.path("castarter", .x),
                                                               full.names = TRUE,
                                                               recursive = FALSE), pattern = "castarter/"))
        names(projectsAndWebsites_list) <- projects

        selectizeInput(inputId = "selected_websites",
                       label = "Available projects and websites",
                       choices = as.list(c("", projectsAndWebsites_list)),
                       selected = "",
                       multiple = TRUE,
                       width = "95%")

    })

    output$tagSelector <- renderUI({
        selectizeInput("tag", label = "Tag", choices =
                           if (is.null(unlist(allTags$tag))) {
                               NULL
                           } else {
                           tibble::data_frame(tag = unlist(allTags$tag)) %>%
                           dplyr::group_by(tag) %>%
                           dplyr::tally() %>%
                           dplyr::arrange(desc(n)) %>%
                           na.omit() %>%
                           dplyr::pull(tag)},
                       multiple = TRUE,
                       options = list(create = TRUE, placeholder = "Insert tag"))
    })

    output$typeSelector <- renderUI({
        selectizeInput("type", label = "Type", choices =
                           if (is.null(unlist(allTags$type))) {
                               NULL
                           } else {
                           tibble::data_frame(type = unlist(allTags$type)) %>%
                           dplyr::group_by(type) %>%
                           dplyr::tally() %>%
                           dplyr::arrange(desc(n)) %>%
                           na.omit() %>%
                           dplyr::pull(type)},
                       multiple = TRUE,
                       options = list(create = TRUE, placeholder = "Insert type"))


    })

    output$categorySelector <- renderUI({
        selectizeInput("category", label = "Category", choices =
                           if (is.null(unlist(allTags$category))) {
                               NULL
                           } else {
                           tibble::data_frame(category = unlist(allTags$category)) %>%
                           dplyr::group_by(category) %>%
                           dplyr::tally() %>%
                           dplyr::arrange(desc(n)) %>%
                           na.omit() %>%
                           dplyr::pull(category)},
                       multiple = TRUE,
                       options = list(create = TRUE, placeholder = "Insert category"))
    })

    output$datasetInfo <- renderUI({
        if (nrow(dataset)==0) {
            HTML(text = "Select a dataset from the list above and click on 'Select dataset' to load it in the current session.")
        } else {
            HTML(text = paste0("The following datasets are now loaded in the current section:<br>", paste(unique(datasetOriginal$website), collapse = ", ")))
        }
    })



    ##### Load rds after selection ######

    observeEvent(input$SetCastarter, {

     #  message(paste(input$project, input$website, sep = "/"))
        datasetOriginal <<- withProgress(expr = {LoadDatasets(projectsAndWebsites = input$selected_websites, addProjectColumn = TRUE)},
                                         message = "Loading dataset(s)... please wait")
        dataset <<- datasetOriginal

        for (i in file.path("castarter", input$selected_websites, "Logs", "tags.rds")[file.exists(file.path("castarter", input$selected_websites, "Logs", "tags.rds"))==FALSE]) {
            tibble::data_frame(doc_id = dataset$doc_id[input$id],  tag = list(input$tag), category = list(input$category), type = list(input$type))

            saveRDS(object = tibble::data_frame(doc_id = NA,
                                                tag = list(NA),
                                                category = list(NA),
                                                type=list(NA)) %>% dplyr::filter(is.na(doc_id)==FALSE),
                    file = i)
        }

        allTags <<- purrr::map_df(.x = file.path("castarter", input$selected_websites, "Logs", "tags.rds"), .f = readRDS)

        output$datasetInfo <- renderUI({
            if (nrow(dataset)==0) {
                HTML(text = "Select a dataset from the list above and click on 'Select dataset' to load it in the current session.")
            } else {
                HTML(text = paste0("The following datasets are now loaded in the current session:<br>"
                                 #  , paste(unique(datasetOriginal$website), collapse = ", ")
                                 )
                     )
            }
        })

        output$SummariseDataset_table <-  renderTable(
            castarter::SummariseDataset(dataset = datasetOriginal) %>%
                dplyr::mutate(From = as.character(From), Until = as.character(Until), Total = scales::number(as.integer(Total)))
        )




    })


    ##### UI read and tag ######



    output$id <- renderText({
        paste("<b>id</b>:", dataset$doc_id[input$id])
    })

    output$title <- renderText({
        dataset$title[input$id]
    })

    output$date <- renderText({
        as.character(as.Date(dataset$date[input$id]))
    })

    output$link <- renderText({
        paste('<a href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>')
    })

    output$contents <- renderText({


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

    #### Submit tags ####

    observeEvent(input$submit, {


        if (file.exists(file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds")) == FALSE) {
            saveRDS(object = tibble::data_frame(doc_id = NA, tag = list(NA), category = list(NA), type=list(NA)) %>%
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
                                  tibble::data_frame(doc_id = dataset$doc_id[input$id],
                                                     tag = list(input$tag),
                                                     category = list(input$category),
                                                     type = list(input$type))) %>%
                    dplyr::arrange(doc_id)
            }
        } else { #runs when *tags* is empty, adds first item
            tags <- tibble::data_frame(doc_id = dataset$doc_id[input$id],  tag = list(input$tag), category = list(input$category), type = list(input$type))
        }
        saveRDS(object = tags, file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))

        output$previousTags <- renderText({
            tags <- readRDS(file = file.path("castarter", dataset$project[input$id], dataset$website[input$id], "Logs", "tags.rds"))
            paste("<b>tag</b>: ", paste(as.character(unlist(tags$tag[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                  "<b>category</b>: ", paste(as.character(unlist(tags$category[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
                  "<b>type</b>: ", paste(as.character(unlist(tags$type[tags$doc_id==dataset$doc_id[input$id]])), collapse = ", "))
        })
    })

    output$totalItems <- renderText({
        paste("<b>Total items:</b>", nrow(dataset))
    })

    output$previousTags <- renderText({
        paste("<b>tag</b>: ", paste(as.character(unlist(allTags$tag[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
              "<b>category</b>: ", paste(as.character(unlist(allTags$category[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "), "<br />",
              "<b>type</b>: ", paste(as.character(unlist(allTags$type[allTags$doc_id==dataset$doc_id[input$id]])), collapse = ", "))
    })

    output$filterPattern <- renderUI({
        if(input$filterCheckbox == TRUE){
            textInput("patternFilter", "Pattern to filter", value = "")
        }
    })

    output$filterTagSelector <- renderUI({
        if(input$filterCheckbox == TRUE){
            selectInput('tagFilter', 'Filter by tag',
                        choices =  if (is.null(unlist(allTags$tag))) {
                            NULL
                        } else {
                        tibble::data_frame(tag = unlist(allTags$tag)) %>%
                            dplyr::arrange(tag) %>%
                            dplyr::distinct() %>%
                            dplyr::pull(tag)},
                        multiple=TRUE, selectize=FALSE)
        }
    })

    output$filtercategorySelector <- renderUI({
        if(input$filterCheckbox == TRUE){
            selectInput('categoryFilter', 'Filter by category',
                        choices =  if (is.null(unlist(allTags$category))) {
                            NULL
                        } else {tibble::data_frame(category = unlist(allTags$category)) %>%
                            dplyr::arrange(category) %>%
                            dplyr::distinct() %>%
                            dplyr::pull(category)},
                        multiple=TRUE, selectize=FALSE)
        }
    })

    output$filterTypeSelector <- renderUI({
        if(input$filterCheckbox == TRUE){
            selectInput('typeFilter', 'Filter by type',
                        choices =  if (is.null(unlist(allTags$type))) {
                            NULL
                        } else {tibble::data_frame(type = unlist(allTags$type)) %>%
                            dplyr::arrange(type) %>%
                            dplyr::distinct() %>%
                            dplyr::pull(type)},
                        multiple=TRUE, selectize=FALSE)
        }
    })

    output$filterAndOrRadio <- renderUI({
        if(input$filterCheckbox == TRUE){
            radioButtons(inputId = "andOrFilter", label = "Keep items that belong to any of the above categories, or only those that belong to all of those selected?",
                         choices = c("Any", "All"),
                         selected = "Any",
                         inline = TRUE)
        }
    })

    output$invertFilterCheckbox <- renderUI({
        if(input$filterCheckbox == TRUE){
            checkboxInput(inputId = "invertFilter", label = "Invert filter?", value = FALSE)
        }
    })


    output$filterActionButton <- renderUI({
        if(input$filterCheckbox == TRUE){
            actionButton("filterAction", "Filter")
        }
    })

    output$filterRangeSelector_ui <- renderUI({
        numericInput(inputId = "id", label = NULL, value = 1, min = 1, max = nrow(dataset))
    })

    output$filterResetButton <- renderUI({
        actionButton("filterReset", "Reset filter")
    })

    observeEvent(input$showRadio, {
        if (input$showRadio=="Only untagged") {
            dataset <<- dataset[which(!is.element(el = dataset$doc_id, set = tags$doc_id)),]
        } else if (input$showRadio=="Only tagged") {
            dataset <<- dataset[which(is.element(el = dataset$doc_id, set = tags$doc_id)),]
        }
        # update input UI
        updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = if(exists("dataset")) nrow(dataset) else 100)
        output$totalItems <- renderText({
            paste("<b>Total items:</b>", nrow(dataset))
        })
    })

    observeEvent(input$filterAction, {
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
        if (is.null(input$patternFilter)==FALSE&input$patternFilter!="") {
            filterPatternL <- stringr::str_detect(string = dataset$text, pattern = stringr::regex(pattern = input$patternFilter, ignore_case = TRUE))
            dataset <<- dataset[filterPatternL,]
            #filterL <- Reduce("&", list(filterL, filterPattern))
        } else {
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
            if (input$invertFilter== TRUE) {
                dataset <<- dataset[which(!is.element(el = dataset$doc_id, set = allTags$doc_id[filterL])),]
            } else {
                dataset <<- dataset[which(is.element(el = dataset$doc_id, set = allTags$doc_id[filterL])),]
            }
            # update input UI
            updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = nrow(dataset))
            output$totalItems <- renderText({
                paste("<b>Total items:</b>", nrow(dataset))
            })
            ## update news item shown
            output$id <- renderText({
                paste("<b>Project-website-id</b>:", dataset$doc_id[input$id])
            })

            output$title <- renderText({
                dataset$title[input$id]
            })

            output$date <- renderText({
                as.character(as.Date(dataset$date[input$id]))
            })

            output$link <- renderText({
                paste('<a href="', dataset$link[input$id], '">', dataset$link[input$id], '</a>')
            })

            output$contents <- renderText({
                if (input$pattern=="") { #output text if no string is given
                    dataset$text[input$id]
                } else {
                    paste0(unlist(stringr::str_split(string = dataset$text[input$id], pattern = stringr::regex(pattern = input$pattern, ignore_case = TRUE))), collapse = paste0('<span style="background-color: #FFFF00">', input$pattern, '</span>'))
                }
            })
        }
    })

    observeEvent(input$filterReset, {
        dataset <<- datasetOriginal
        # update input UI
        updateNumericInput(session = session, inputId = "id", value = 1, min = 1, max = nrow(dataset))
        output$totalItems <- renderText({
            paste("<b>Total items:</b>", nrow(dataset))
        })
    })

    output$UpdateDataset <- renderText({
        if (is.null(input$categoryFilter)==FALSE) {
            filterL <- purrr::map_lgl(.x = tags$category, .f = is.element, el = input$categoryFilter)
            which(is.element(el = dataset$doc_id, set = tags$doc_id[filterL]))
        }
    })









})


