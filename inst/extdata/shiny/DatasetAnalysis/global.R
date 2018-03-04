library('shiny')
library('dplyr')
library('castarter')

# load dataset
dataset <- readRDS(file = file.path('data', 'dataset.rds'))
dataset_bySentence <- readRDS(file = file.path('data', 'dataset_bySentence.rds'))

minDate <- min(dataset$date)
maxDate <- max(dataset$date)

exampleTerms <- "Crimea, Syria"
SetCastarter(project = "Presidents", website = "Kremlin.ru (English version)")
