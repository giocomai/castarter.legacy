
CountPublicationsPerWebsite <- function(allDatasets, order = "averagePublicationsPerDay", mode = "graph", typeOfWebsite = TRUE, keepOnly = "", 
    removeWebsite = "") {
    allDatasetsFactors <- allDatasets
    allDatasetsFactors$website <- as.factor(allDatasetsFactors$website)
    allNamesOfWebsites <- levels(allDatasetsFactors$website)
    allNamesOfWebsitesOriginal <- as.character(allNamesOfWebsites)
    if (keepOnly[1] != "") {
        allNamesOfWebsites <- allNamesOfWebsites[is.element(allNamesOfWebsites, keepOnly)]
    }
    if (removeWebsite[1] != "") {
        allNamesOfWebsites <- allNamesOfWebsites[!is.element(allNamesOfWebsites, removeWebsite)]
    }
    if (typeOfWebsite == TRUE) {
        numberOfPublicationsPerWebsite <- as.data.frame(matrix(nrow = length(allNamesOfWebsites), ncol = 5))
        names(numberOfPublicationsPerWebsite) <- c("website", "numberOfPublications", "daysOfActivity", "averagePublicationsPerDay", "typeOfWebsite")
    } else {
        numberOfPublicationsPerWebsite <- as.data.frame(matrix(nrow = length(allNamesOfWebsites), ncol = 4))
        names(numberOfPublicationsPerWebsite) <- c("website", "numberOfPublications", "daysOfActivity", "averagePublicationsPerDay")
    }
    numberOfPublicationsPerWebsite$numberOfPublications <- as.integer(numberOfPublicationsPerWebsite$numberOfPublications)
    numberOfPublicationsPerWebsite$daysOfActivity <- as.integer(numberOfPublicationsPerWebsite$daysOfActivity)
    for (i in 1:length(allNamesOfWebsites)) {
        numberOfPublicationsPerWebsite$website[i] <- allNamesOfWebsites[i]
        if (keepOnly[1] != "") {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "website")$freq[is.element(allNamesOfWebsitesOriginal, 
                removeWebsite)][i])
        }
        if (removeWebsite[1] != "") {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "website")$freq[!is.element(allNamesOfWebsitesOriginal, 
                removeWebsite)][i])
        } else {
            numberOfPublicationsPerWebsite$numberOfPublications[i] <- as.integer(plyr::count(allDatasets, "website")$freq[i])
        }
        listOfDates <- allDatasets$dates[allDatasets$website == allNamesOfWebsites[i]]
        listOfDates <- listOfDates[!is.na(listOfDates)]
        listOfDates <- listOfDates[order(listOfDates)]
        daysOfActivity <- as.integer(listOfDates[length(listOfDates)] - listOfDates[1])
        numberOfPublicationsPerWebsite$daysOfActivity[i] <- as.integer(daysOfActivity)
        numberOfPublicationsPerWebsite$averagePublicationsPerDay[i] <- numberOfPublicationsPerWebsite$numberOfPublications[i]/numberOfPublicationsPerWebsite$daysOfActivity[i]
        if (typeOfWebsite == TRUE) {
            numberOfPublicationsPerWebsite$typeOfWebsite[i] <- allDatasets$typeOfWebsite[allDatasets$website == allNamesOfWebsites[i]][1]
        }
    }
    numberOfPublicationsPerWebsite$website <- as.factor(numberOfPublicationsPerWebsite$website)
    if (order == "averagePublicationsPerDay") {
        numberOfPublicationsPerWebsite$website <- reorder(numberOfPublicationsPerWebsite$website, numberOfPublicationsPerWebsite$averagePublicationsPerDay)
    } else if (order == "absoluteNumber") {
        numberOfPublicationsPerWebsite$numberOfPublications <- reorder(numberOfPublicationsPerWebsite$website, numberOfPublicationsPerWebsite$numberOfPublications)
    }
    if (mode == "graph") {
        numberOfPublicationsPerWebsite <- ggplot(numberOfPublicationsPerWebsite, aes(x = website, y = averagePublicationsPerDay), order = sample(seq_along(averagePublicationsPerDay))) + 
            geom_bar(stat = "identity") + coord_flip()
        numberOfPublicationsPerWebsite <- numberOfPublicationsPerWebsite + ggtitle("Average number of publications per website per day")
    }
    numberOfPublicationsPerWebsite
} 
