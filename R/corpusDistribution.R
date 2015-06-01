
CountPublicationsPerWebsite <- function(allDatasets, order = "averagePublicationsPerDay", mode = "graph", typeOfWebsite = TRUE, keepOnly = "", 
    removeWebsite = "") {
    allDatasetsFactors <- allDatasets
    allDatasetsFactors$nameOfWebsite <- as.factor(allDatasetsFactors$nameOfWebsite)
    allNamesOfWebsites <- levels(allDatasetsFactors$nameOfWebsite)
    allNamesOfWebsitesOriginal <- as.character(allNamesOfWebsites)
    if (keepOnly[1] != "") {
        allNamesOfWebsites <- allNamesOfWebsites[is.element(allNamesOfWebsites, keepOnly)]
    }
    if (removeWebsite[1] != "") {
        allNamesOfWebsites <- allNamesOfWebsites[!is.element(allNamesOfWebsites, removeWebsite)]
    }
    if (typeOfWebsite == TRUE) {
        numberOfPublicationsPerWebsite <- as.data.frame(matrix(nrow = length(allNamesOfWebsites), ncol = 5))
        names(numberOfPublicationsPerWebsite) <- c("nameOfWebsite", "numberOfPublications", "daysOfActivity", "averagePublicationsPerDay", "typeOfWebsite")
    } else {
        numberOfPublicationsPerWebsite <- as.data.frame(matrix(nrow = length(allNamesOfWebsites), ncol = 4))
        names(numberOfPublicationsPerWebsite) <- c("nameOfWebsite", "numberOfPublications", "daysOfActivity", "averagePublicationsPerDay")
    }
    numberOfPublicationsPerWebsite$numberOfPublications <- as.integer(numberOfPublicationsPerWebsite$numberOfPublications)
    numberOfPublicationsPerWebsite$daysOfActivity <- as.integer(numberOfPublicationsPerWebsite$daysOfActivity)
    for (i in 1:length(allNamesOfWebsites)) {
        numberOfPublicationsPerWebsite$nameOfWebsite[i] <- allNamesOfWebsites[i]
        if (keepOnly[1] != "") {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "nameOfWebsite")$freq[is.element(allNamesOfWebsitesOriginal, 
                removeWebsite)][i])
        }
        if (removeWebsite[1] != "") {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "nameOfWebsite")$freq[!is.element(allNamesOfWebsitesOriginal, 
                removeWebsite)][i])
        } else {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "nameOfWebsite")$freq[i])
        }
        listOfDates <- allDatasets$dates[allDatasets$nameOfWebsite == allNamesOfWebsites[i]]
        listOfDates <- listOfDates[!is.na(listOfDates)]
        listOfDates <- listOfDates[order(listOfDates)]
        daysOfActivity <- as.integer(listOfDates[length(listOfDates)] - listOfDates[1])
        numberOfPublicationsPerWebsite$daysOfActivity[i] <- as.integer(daysOfActivity)
        numberOfPublicationsPerWebsite$averagePublicationsPerDay[i] <- numberOfPublicationsPerWebsite$numberOfPublications[i]/numberOfPublicationsPerWebsite$daysOfActivity[i]
        if (typeOfWebsite == TRUE) {
            numberOfPublicationsPerWebsite$typeOfWebsite[i] <- allDatasets$typeOfWebsite[allDatasets$nameOfWebsite == allNamesOfWebsites[i]][1]
        }
    }
    numberOfPublicationsPerWebsite$nameOfWebsite <- as.factor(numberOfPublicationsPerWebsite$nameOfWebsite)
    if (order == "averagePublicationsPerDay") {
        numberOfPublicationsPerWebsite$nameOfWebsite <- reorder(numberOfPublicationsPerWebsite$nameOfWebsite, numberOfPublicationsPerWebsite$averagePublicationsPerDay)
    } else if (order == "absoluteNumber") {
        numberOfPublicationsPerWebsite$numberOfPublications <- reorder(numberOfPublicationsPerWebsite$nameOfWebsite, numberOfPublicationsPerWebsite$numberOfPublications)
    }
    if (mode == "graph") {
        numberOfPublicationsPerWebsite <- ggplot(numberOfPublicationsPerWebsite, aes(x = nameOfWebsite, y = averagePublicationsPerDay), order = sample(seq_along(averagePublicationsPerDay))) + 
            geom_bar(stat = "identity") + coord_flip()
        numberOfPublicationsPerWebsite <- numberOfPublicationsPerWebsite + ggtitle("Average number of publications per website per day")
    }
    numberOfPublicationsPerWebsite
} 
