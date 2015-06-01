#' A function that loads all needed packages
#'
#' It needs to be deleted
#' @keywords toBeRemoved
#' @export
#' @examples
#' LoadRequiredPackages()

LoadRequiredPackages <- function() {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("RCurl", "stringr", "gtools", "boilerpipeR", "XML", "lubridate", "tm", "SnowballC", "ggplot2", "wordcloud", "dplyr", "xlsx", "slam", "zoo", "RGtk2Extras", "scales", "reshape2", "gridExtra", "RColorBrewer", "mgcv")
}

CreateFolderStructure <- function(nameOfProject, nameOfWebsite) {
  if (!file.exists(file.path(nameOfProject))) {dir.create(file.path(nameOfProject))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite))) {dir.create(file.path(nameOfProject, nameOfWebsite))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Logs"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "Logs"))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "IndexHtml"))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Html"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "Html"))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Txt"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "Txt"))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Dataset"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "Dataset"))}
  if (!file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs"))) {dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))}
}

LoadLatestWebsite <- function(nameOfProject, nameOfWebsite) {
  lastSavedFile <- file.path(file.path(nameOfProject, nameOfWebsite), sort(list.files(file.path(nameOfProject, nameOfWebsite))[str_extract(list.files(file.path(nameOfProject, nameOfWebsite)), "RData") == "RData"], decreasing = TRUE)[1])
  if (file.exists(lastSavedFile)) {load(file = lastSavedFile)}
}

SaveAndExportWebsite <- function(nameOfProject, nameOfWebsite, exportXlsx = TRUE) {
  ## Save environment 
  save.image(file = file.path(nameOfProject, nameOfWebsite, paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, sep = " - "), ".RData")))
  ## Save dataset
  dataset <- cbind(metadata, articlesTxt, stringsAsFactors = FALSE)
  save(dataset, file = file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "dataset", sep = " - "), ".RData")))
  #export all as xlsx
  if (exportXlsx == TRUE) {
    write.xlsx(cbind(metadata, articlesTxt), file.path(nameOfProject, nameOfWebsite, "Dataset", paste0(paste(Sys.Date(), nameOfProject, nameOfWebsite, "metadata and txt", sep = " - "), ".xlsx")))
  }
}
