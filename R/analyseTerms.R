#' Compares frequency of specific words across websites. 
#' 
#' @param corpus A TM corpus. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param terms Character vector of terms to be compared. 
#' @export
#' @examples
#' AnalyseTerms(corpus, nameOfProject, terms)

AnalyseTerms <- function(corpus, nameOfProject, terms, mode = "graph", includeOnly = NULL, order = "", tipology = "", frequency = "relative") {
    namesOfWebsites <- as.factor(unlist(NLP::meta(corpus, "author")))
    if (length(levels(namesOfWebsites)) > 1) {
        
    } else {
        
    }
    byWebsiteAll <- castarter::DivideByWebsite(corpus, nameOfProject)
    if (is.null(includeOnly) == FALSE) {
        byWebsite <- byWebsiteAll[, includeOnly]
    } else {
        byWebsite <- byWebsiteAll
    }
    mostFrequentByWebsite <- as.data.frame(matrix(nrow = length(byWebsite), ncol = length(terms) + 1))
    colnames(mostFrequentByWebsite)[1] <- "nameOfWebsite"
    for (i in 1:length(terms)) {
        colnames(mostFrequentByWebsite)[i + 1] <- terms[i]
    }
    for (i in 1:length(byWebsite)) {
        corpusTemp <- corpus[byWebsite[, i]]
        corpusTemp <- tm::tm_map(corpusTemp, PlainTextDocument)
        dtmTemp <- tm::DocumentTermMatrix(corpusTemp)
        mostFrequent <- castarter::ShowMostFrequent(dtmTemp, mode = "vector", number = "all")
        totalWords <- sum(mostFrequent$freq)
        mostFrequentByWebsite[i, 1] <- names(byWebsite)[i]
        for (j in 1:length(terms)) {
            value <- mostFrequent$freq[mostFrequent$word == terms[j]]
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
            mostFrequentByWebsiteLong <- reshape2::melt(mostFrequentByWebsite, id.vars = c("nameOfWebsite", "typeOfWebsite"), measure.vars = terms, 
                value.name = "frequency")
        } else {
            mostFrequentByWebsiteLong <- reshape2::melt(mostFrequentByWebsite, id.vars = "nameOfWebsite", measure.vars = terms, value.name = "frequency")
        }
        names(mostFrequentByWebsiteLong)[names(mostFrequentByWebsiteLong) == "variable"] <- "Term"
        mostFrequentByWebsiteLong$nameOfWebsite <- as.factor(mostFrequentByWebsiteLong$nameOfWebsite)
        # mostFrequentByWebsiteLong$typeOfWebsite <- as.factor(mostFrequentByWebsiteLong$typeOfWebsite)
        # factor(mostFrequentByWebsiteLong$nameOfWebsite, levels = rev(levels(mostFrequentByWebsiteLong$typeOfWebsite)))
        mentionterms <- paste(dQuote(rev(terms)), collapse = ", ")
        mostFrequentByWebsite <- ggplot2::ggplot(mostFrequentByWebsiteLong, aes(x = nameOfWebsite, y = frequency, fill = Term)) + geom_bar(stat = "identity", 
            position = "dodge", colour = "black") + ggtitle(paste("Frequency of", mentionterms)) + scale_x_discrete(name = "") + scale_y_continuous(name = "Frequency of term as % of all words", 
            labels = percent) + guides(fill = guide_legend(reverse = TRUE)) + coord_flip()
        if (order == "frequency") {
            mostFrequentByWebsite <- mostFrequentByWebsite + scale_x_discrete(limits = dataframeMostFrequentByWebsite$nameOfWebsite)
        }
    }
    mostFrequentByWebsite
} 

