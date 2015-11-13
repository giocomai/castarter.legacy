#' Compares frequency of specific words across websites. 
#' 
#' @param corpus A TM corpus. 
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param specificTerms Character vector of terms to be compared. 
#' @export
#' @examples
#' AnalyseTerms(corpus, nameOfProject, specificTerms)

AnalyseTerms <- function(corpus, nameOfProject, specificTerms, mode = "graph", includeOnly = NULL, order = TRUE, tipology = "", frequency = "relative") {
     namesOfWebsites <- levels(as.factor(unlist(NLP::meta(corpus, "author"))))
     corpusDtm <- CreateDtm(corpus)
     byWebsiteAll <- DivideByWebsite(corpus = corpus, nameOfProject = nameOfProject)
     if (is.null(includeOnly) == FALSE) {
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
        dtmTemp <- corpusDtm[byWebsite[, i], ]
        mostFrequent <- castarter::ShowMostFrequent(dtmTemp, mode = "data.frame", number = "all", specificTerms = specificTerms)
        totalWords <- sum(mostFrequent$freq)
        mostFrequentByWebsite[i, 1] <- names(byWebsite)[i]
        for (j in 1:length(specificTerms)) {
            value <- mostFrequent$freq[mostFrequent$term == specificTerms[j]]
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
    if (order == TRUE) {
        mostFrequentByWebsite <- mostFrequentByWebsite[order(mostFrequentByWebsite[, 2]), ]
        dataframeMostFrequentByWebsite <- mostFrequentByWebsite
    }
    if (mode == "graph") {
        if (tipology != "") {
            mostFrequentByWebsiteLong <- reshape2::melt(mostFrequentByWebsite, id.vars = c("nameOfWebsite", "typeOfWebsite"), measure.vars = specificTerms, 
                value.name = "frequency")
        } else {
            mostFrequentByWebsiteLong <- reshape2::melt(mostFrequentByWebsite, id.vars = "nameOfWebsite", measure.vars = specificTerms, value.name = "frequency")
        }
        names(mostFrequentByWebsiteLong)[names(mostFrequentByWebsiteLong) == "variable"] <- "Term"
        mostFrequentByWebsiteLong$nameOfWebsite <- as.factor(mostFrequentByWebsiteLong$nameOfWebsite)
        # mostFrequentByWebsiteLong$typeOfWebsite <- as.factor(mostFrequentByWebsiteLong$typeOfWebsite)
        # factor(mostFrequentByWebsiteLong$nameOfWebsite, levels = rev(levels(mostFrequentByWebsiteLong$typeOfWebsite)))
        mentionterms <- paste(dQuote(rev(specificTerms)), collapse = ", ")
        mostFrequentByWebsite <- ggplot2::ggplot(mostFrequentByWebsiteLong, ggplot2::aes(x = nameOfWebsite, y = frequency, fill = Term)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ggplot2::ggtitle(paste("Frequency of", mentionterms)) + 
            ggplot2::scale_x_discrete(name = "")
        if (frequency == "relative") {
            mostFrequentByWebsite + ggplot2::scale_y_continuous(name = "Frequency of term as % of all words", labels = percent) + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) + ggplot2::coord_flip()
        } else if (frequency == "absolute") {
            mostFrequentByWebsite + ggplot2::scale_y_continuous(name = "Frequency of term") + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) + ggplot2::coord_flip()
        }
        if (order == TRUE) {
            mostFrequentByWebsite <- mostFrequentByWebsite + ggplot2::scale_x_discrete(limits = dataframeMostFrequentByWebsite$nameOfWebsite)
        }
    }
    mostFrequentByWebsite
} 

