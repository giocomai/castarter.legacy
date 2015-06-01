AnalyseTerms <- function(corpus, nameOfProject, specificTerms, mode = "graph", includeOnly = "", order = "", tipology = "", frequency = "relative") {
    byWebsiteAll <- DivideByWebsite(corpus, nameOfProject)
    if (includeOnly[1] != "") {
        byWebsite <- byWebsiteAll[, includeOnly]
    } else {
        byWebsite <- byWebsiteAll
    }
    mostFrequentByWebsite <- as.data.frame(matrix(nrow = length(byWebsite), ncol = length(specificTerms) + 1))
    colnames(mostFrequentByWebsite)[1] <- "nameOfWebsite"
    for (i in 1:length(specificTerms)) {
        colnames(mostFrequentByWebsite)[i + 1] <- specificTerms[i]
    }
    for (i in 1:length(byWebsite)) {
        mostFrequent <- ShowMostFrequent(corpus[byWebsite[, i]], mode = "vector", number = "all")
        totalWords <- sum(mostFrequent$freq)
        mostFrequentByWebsite[i, 1] <- names(byWebsite)[i]
        for (j in 1:length(specificTerms)) {
            value <- mostFrequent$freq[mostFrequent$word == specificTerms[j]]
            if (length(value) == 0) {
                value <- 0
            } else {
                if (frequency == "absolute") {
                  mostFrequentByWebsite[i, j + 1] <- value
                } else {
                  mostFrequentByWebsite[i, j + 1] <- value/totalWords
                }
            }
        }
    }
    if (tipology != "") {
        mostFrequentByWebsite$typeOfWebsite <- NA
        tipology$typeOfWebsite <- as.character(tipology$typeOfWebsite)
        for (i in 1:length(mostFrequentByWebsite[, 1])) {
            if (is.element(mostFrequentByWebsite$nameOfWebsite[i], tipology$nameOfWebsite)) {
                mostFrequentByWebsite$typeOfWebsite[i] <- tipology$typeOfWebsite[tipology$nameOfWebsite == mostFrequentByWebsite$nameOfWebsite[i]]
            } else {
                mostFrequentByWebsite$typeOfWebsite[i] <- NA
            }
        }
        # mostFrequentByWebsite$nameOfWebsite <- reorder(mostFrequentByWebsite$nameOfWebsite, mostFrequentByWebsite$nameOfWebsite)
    }
    if (order == "frequency") {
        mostFrequentByWebsite <- mostFrequentByWebsite[order(mostFrequentByWebsite[, 2]), ]
        dataframeMostFrequentByWebsite <- mostFrequentByWebsite
    }
    if (mode == "graph") {
        if (tipology != "") {
            mostFrequentByWebsiteLong <- melt(mostFrequentByWebsite, id.vars = c("nameOfWebsite", "typeOfWebsite"), measure.vars = specificTerms, 
                value.name = "frequency")
        } else {
            mostFrequentByWebsiteLong <- melt(mostFrequentByWebsite, id.vars = "nameOfWebsite", measure.vars = specificTerms, value.name = "frequency")
        }
        names(mostFrequentByWebsiteLong)[names(mostFrequentByWebsiteLong) == "variable"] <- "Term"
        mostFrequentByWebsiteLong$nameOfWebsite <- as.factor(mostFrequentByWebsiteLong$nameOfWebsite)
        # mostFrequentByWebsiteLong$typeOfWebsite <- as.factor(mostFrequentByWebsiteLong$typeOfWebsite)
        # factor(mostFrequentByWebsiteLong$nameOfWebsite, levels = rev(levels(mostFrequentByWebsiteLong$typeOfWebsite)))
        mentionSpecificTerms <- paste(dQuote(rev(specificTerms)), collapse = ", ")
        mostFrequentByWebsite <- ggplot(mostFrequentByWebsiteLong, aes(x = nameOfWebsite, y = frequency, fill = Term)) + geom_bar(stat = "identity", 
            position = "dodge", colour = "black") + ggtitle(paste("Frequency of", mentionSpecificTerms)) + scale_x_discrete(name = "") + scale_y_continuous(name = "Frequency of term as % of all words", 
            labels = percent) + guides(fill = guide_legend(reverse = TRUE)) + coord_flip()
        if (order == "frequency") {
            mostFrequentByWebsite <- mostFrequentByWebsite + scale_x_discrete(limits = dataframeMostFrequentByWebsite$nameOfWebsite)
        }
    }
    mostFrequentByWebsite
} 
