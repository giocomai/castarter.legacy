ExportArticlesWith <- function(dataset, word, nameOfProject, nameOfWebsite, txt = TRUE, csv = FALSE, xlsx = FALSE, data.frame = FALSE, includeOnly = "", sortBy = "date") {
  #Export only items that include...
  tempDataset <- dataset[grep(word, dataset$articlesTxt, ignore.case = TRUE), ]
  if (includeOnly != "") {
    tempDataset <- tempDataset[tempDataset$nameOfWebsite == includeOnly, ]
  }
  if (sortBy == "date") {
    tempDataset <- tempDataset[order(tempDataset$dates), ]    
  }
  if (xlsx == TRUE) {
    write.xlsx(tempDataset, file.path(nameOfProject, nameOfWebsite, "Outputs", paste(word, " in ", nameOfWebsite, ".xlsx", sep = "")))
    print(paste("File .xlsx exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(word, " in ", nameOfWebsite, ".xlsx", sep = ""))))
  }
  if (txt == TRUE) {
    writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$titles), paste("Link:", tempDataset$articlesLinks), paste("ID:", tempDataset$articlesId), tempDataset$articlesTxt, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(nameOfProject, nameOfWebsite, "Outputs", paste(word, " in ", nameOfWebsite, ".txt", sep = "")))
    print(paste("File .txt exported to:", file.path(nameOfProject, nameOfWebsite, "Outputs", paste(word, " in ", nameOfWebsite, ".txt", sep = ""))))
  }
  if (data.frame == TRUE) {
    tempDataset
  }
}