#' Shows most frequent terms in a corpus
#' 
#' Shows most frequent terms in a corpus starting from a document term matrix.
#'  
#' @param corpusDtm A document-term matrix created with the 'TM' package or a document-feature matrix of the 'quanteda' type.
#' @param mode Defines the type of output. Can be
##' \itemize{
##'  \item{"data.frame"}{: Outputs a data.frame This is the default option.}
##'  \item{"barchart"}{: Outputs a ggplot2 barchart.}
##'  \item{"wordcloud"}{: Outputs a wordcloud.}
##' }
##'
#' @param corpus Required only if corpusDtm is not of the 'quanteda' type. A corpus as created by the 'tm' package including relevant metadata and typically created with castarter's CreateCorpus() function. 
#' @param terms A character vector, defaults to NULL. If provided, only terms included in this vector will be included in the output.
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If nameOfProject and nameOfWebsite are provided, in saves the barchart in the "Outputs" subfolder. 
#' @param title A character vector, defaults to NULL.It allows to customize the title of the exported barchart file. 
#' @param tipology A data.frame of two columns, one named "term" and one named "type". If provided, and if type=="barchart", colors are used to differentiate terms belonging to different types. Example: tipology <- data.frame(term = c("apple", "orange", "bear", "lion"), type = c("fruit", "fruit", "animal", "animal"))
#' @param invert Logical, defaults to FALSE. If TRUE, inverts websites and terms in the barcharts mapping, i.e. terms determine the color while the website are on the y axis.
#' @return A data.frame, barchart, or wordcloud as defined with the 'mode' parameter.
#' @export
#' @examples
#' corpusDtm <- DocumentTermMatrix(corpus).
#' ShowFreq(corpusDtm)

ShowFreq <- function(corpusDtm, mode = "data.frame", number = 10, terms = NULL, corpus = NULL, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0, export = FALSE, customTitle = NULL, tipology = NULL, byDate = FALSE, byWebsite = FALSE, stacked = FALSE, relFreq = FALSE, invert = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    if (gtools::invalid(nameOfProject) == TRUE) {
        nameOfProject <- CastarterOptions("nameOfProject")
    }
    if (gtools::invalid(nameOfWebsite) == TRUE) {
        nameOfWebsite <- CastarterOptions("nameOfWebsite")
    }
    if (number == "all") {
        number <- length(dim(corpusDtm)[2])
    }
    if (quanteda::is.dfm(corpusDtm)==TRUE) {
        if (as.character(class(terms))=="dictionary") {
            termsDic <- terms
        } else {
            termsL <- as.list(terms)
            termsL <- setNames(object = termsL, nm = terms)
            termsDic <- quanteda::dictionary(x = termsL)
        }
        if (relFreq == TRUE) {
            corpusDtm <- quanteda::weight(corpusDtm, "relFreq")
        }
        corpusDtm <- quanteda::applyDictionary(corpusDtm, termsDic)
        if (byWebsite==TRUE) {
            namesOfWebsites <- unique(sapply(strsplit(x = quanteda::docnames(corpusDtm), split = ".", fixed = TRUE),function(x) x[2]))
            wordFrequency <- data.frame()
            for (i in seq_along(namesOfWebsites)) {
                temp <- quanteda::topfeatures(x = corpusDtm[grepl(pattern = namesOfWebsites[i], quanteda::docnames(corpusDtm))], n = number)
                tempDF <- data.frame(term = names(temp), freq = temp, type = namesOfWebsites[i])
                wordFrequency <- rbind(wordFrequency, tempDF)
            }
        } else if (byDate == TRUE) {
            dates <- unique(lubridate::year(sapply(strsplit(x = quanteda::docnames(corpusDtm), split = ".", fixed = TRUE),function(x) x[1])))
            wordFrequency <- data.frame()
            for (i in seq_along(dates)) {
                temp <- quanteda::topfeatures(x = corpusDtm[grepl(pattern = dates[i], quanteda::docnames(corpusDtm))], n = number)
                tempDF <- data.frame(term = names(temp), freq = temp, type = dates[i])
                wordFrequency <- rbind(wordFrequency, tempDF)
            }
            wordFrequency$type <- as.factor(wordFrequency$type)
        } else {
            freq <- quanteda::topfeatures(x = corpusDtm, n = number)
        }
    } else {
        if (is.null(terms) == FALSE) {
            corpusDtm <- corpusDtm[,base::match(terms, colnames(corpusDtm), nomatch = 0)]
        }
            freq <- sort(slam::col_sums(corpusDtm, na.rm = TRUE), decreasing = TRUE)
    }
    if (byWebsite==FALSE&byDate==FALSE) {
        wordFrequency <- data.frame(term = names(freq), freq = freq)[1:number, ]
        wordFrequency <- wordFrequency[wordFrequency$freq>minFrequency,]
        wordFrequency <- wordFrequency[is.na(wordFrequency$freq)==FALSE,]
    }
    if (byWebsite==FALSE&byDate==TRUE&quanteda::is.dfm(corpusDtm)==FALSE) {
        time <- as.character(strptime(as.POSIXct(unlist(NLP::meta(corpus, "datetimestamp")), origin = "1970-01-01"), "%Y-%m-%d"))
        dates <- unique(lubridate::year(time))
        corpusDtm <- corpusDtm[,base::match(terms, colnames(corpusDtm))]
        wordFrequency <- data.frame()
        for (i in seq_along(dates)) {
            temp <- sort(slam::col_sums(corpusDtm[lubridate::year(time)==dates[i],], na.rm = TRUE), decreasing = TRUE)
            tempDF <- data.frame(term = names(temp), freq = temp, type = dates[i])
            wordFrequency <- rbind(wordFrequency, tempDF)
        }
        wordFrequency$type <- as.factor(wordFrequency$type)
    }
    if (stemCompletion == TRUE) {
        wordFrequency$term <- tm::stemCompletion(wordFrequency$term, corpusOriginal)
        for (i in 1:length(wordFrequency$term)) {
            if (wordFrequency$term[i] == "") {
                wordFrequency$term[i] <- row.names(wordFrequency)[i]
            }
        }
    }
    if (mode == "barchart" | mode == "graph") {
        if (is.null(tipology) == FALSE) {
            wordFrequency$type <- NA
            tipology$type <- as.character(tipology$type)
            for (i in 1:length(wordFrequency[, 1])) {
                if (is.element(wordFrequency$term[i], tipology$term)) {
                    wordFrequency$type[i] <- tipology$type[tipology$term == wordFrequency$term[i]]
                } else {
                    wordFrequency$type[i] <- NA
                }
            }
            barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq, fill = type)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("") + ggplot2::labs(fill="Type")
            # mostFrequentByWebsite$nameOfWebsite <- reorder(mostFrequentByWebsite$nameOfWebsite, mostFrequentByWebsite$nameOfWebsite)
        } else if (byWebsite == TRUE | byDate == TRUE) {
            if (stacked == TRUE) {
                if (invert == FALSE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq, fill = type)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("") + ggplot2::labs(fill="")
                } else if (invert == TRUE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq, fill = term)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("") + ggplot2::labs(fill="Term")
                }
            } else if (stacked == FALSE) {
                if (invert == FALSE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq, fill = type)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("") + ggplot2::labs(fill="")
                } else if (invert == TRUE) {
                    if (length(terms)==1) {
                        barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("")
                    } else {
                        barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq, fill = term)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("") + ggplot2::labs(fill="Term")
                    }
                }
            }
        } else {
            barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::ylab("Word frequency") + ggplot2::xlab("")
        }
        if (is.null(customTitle) == FALSE) {
            barchart <- barchart + ggplot2::ggtitle(customTitle)
        } else {
            barchart <- barchart + ggplot2::ggtitle("Word frequency barchart")
        }
        if (export == TRUE) {
            if (is.null(customTitle)) {
                customTitle <- paste(nameOfProject, "word frequency", paste(names(terms), collapse = " - "), sep = " - ")
            }
            if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == FALSE) {
                if (file.exists(file.path(nameOfProject, nameOfWebsite, "Outputs")) == FALSE) {
                    dir.create(file.path(nameOfProject, nameOfWebsite, "Outputs"))
                }
                ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".png")))
                print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".svg")))
                print(paste("File saved in", file.path(nameOfProject, nameOfWebsite, "Outputs", paste0(paste(customTitle, nameOfProject, nameOfWebsite, sep = " - "), ".svg"))))
            } else if (is.null(nameOfProject) == FALSE & is.null(nameOfWebsite) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".png")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".svg")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, nameOfProject, sep = " - "), ".svg"))))
            } else {
                if (!file.exists(file.path("Outputs"))) {
                    dir.create(file.path("Outputs"))
                }
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".png")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".svg")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, sep = " - "), ".svg"))))
            }
        }
        barchart
    } else if (mode == "wordcloud") {
        wordcloud::wordcloud(rownames(wordFrequency), wordFrequency$freq, min.freq = minFrequency, random.order=FALSE, colors = "black")
    } else if (mode == "data.frame") {
        wordFrequency
    }
} 
