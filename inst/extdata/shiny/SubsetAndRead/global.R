library("magrittr")
library("shiny")
if (file.exists("castarter")==FALSE) {
    stop("`castarter` folder not found in the current working directory.")
}

allTags <- tags <- tibble::data_frame(doc_id = NA,  tag = NA, category = NA, type = NA)
dataset <- tibble::data_frame(NA) %>% na.omit()
