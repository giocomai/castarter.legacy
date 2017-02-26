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
#' @param export Logical, defaults to FALSE. If TRUE, saves the time series in both png and pdf format. If project and website are provided, in saves the barchart in the "Outputs" subfolder.
#' @param customTitle A character vector, defaults to NULL.It allows to customize the title of the exported barchart file.
#' @param tipology A data.frame of two columns, one named "term" and one named "type". If provided, and if type=="barchart", colors are used to differentiate terms belonging to different types. Example: tipology <- data.frame(term = c("apple", "orange", "bear", "lion"), type = c("fruit", "fruit", "animal", "animal"))
#' @param relFreq Logical, defaults to FALSE. If TRUE, relative word frequency is given in the output. If FALSE, absolute number of occurrences is given.
#' @param percent Logical, defaults to TRUE. If relFreq==TRUE, labels word frequency as a percentage. It is ignored if relFreq==FALSE.
#' @param invert Logical, defaults to FALSE. If TRUE, inverts websites and terms in the barcharts mapping, i.e. terms determine the color while the website are on the y axis.
#' @return A data.frame, barchart, or wordcloud as defined with the 'mode' parameter.
#' @export
#' @examples
#' corpusDtm <- DocumentTermMatrix(corpus).
#' ShowFreq(corpusDtm)

ShowFreq <- function(corpusDtm, mode = "data.frame", number = 10, terms = NULL, corpus = NULL, order = TRUE, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0, export = FALSE, customTitle = NULL, tipology = NULL, byDate = FALSE, byWebsite = FALSE, stacked = FALSE, relFreq = TRUE, percent = TRUE, invert = FALSE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (number == "all") {
        number <- ncol(corpusDtm)
    }
    if (quanteda::is.dfm(corpusDtm)==TRUE) {
        corpusDtmDocnames <- quanteda::docnames(corpusDtm)
        if (as.character(class(terms))=="dictionary") {
            termsDic <- terms
        } else {
            termsL <- as.list(terms)
            termsL <- setNames(object = termsL, nm = terms)
            termsDic <- quanteda::dictionary(x = termsL)
        }
        if (byWebsite == TRUE & byDate == TRUE) {
            namesOfWebsites <- unique(sapply(strsplit(x = corpusDtmDocnames,
                                                      split = ".", fixed = TRUE),function(x) x[2]))
            dates <- unique(lubridate::year(sapply(strsplit(x = corpusDtmDocnames,
                                                            split = ".", fixed = TRUE),function(x) x[1])))
            totalWords <- data.frame(namesOfWebsites = as.character(sapply(namesOfWebsites, rep, times = length(dates))), dates, totalWords = NA)
            if (relFreq == TRUE) {
                for (i in 1:nrow(totalWords)) {
                    totalWords$totalWords[i] <- sum(corpusDtm[grepl(pattern = paste(totalWords$dates[i], totalWords$namesOfWebsites[i], sep = ".*"), corpusDtmDocnames)])
                }
            }
            corpusDtm <- quanteda::applyDictionary(corpusDtm, termsDic)
            wordFrequency <- data.frame()
            for (i in 1:nrow(totalWords)) {
                temp <- quanteda::topfeatures(x = corpusDtm[grepl(pattern = paste(totalWords$dates[i], totalWords$namesOfWebsites[i], sep = ".*"), corpusDtmDocnames)],
                                              n = number)
                tempDF <- data.frame(term = names(temp), freq = temp, type = totalWords$namesOfWebsites[i], dates = totalWords$dates[i])
                wordFrequency <- rbind(wordFrequency, tempDF)
            }
            if (relFreq == TRUE) {
                wordFrequency$freq <- wordFrequency$freq/totalWords$totalWords
            }
            rownames(wordFrequency) <- NULL
            wordFrequency$dates <- as.factor(wordFrequency$dates)
            wordFrequency$dates <- factor(wordFrequency$dates, levels=rev(levels(wordFrequency$dates)))
            colnames(wordFrequency)[colnames(wordFrequency)=="dates"] <- "Year"
        } else if (byWebsite==TRUE & byDate == FALSE) {
            namesOfWebsites <- unique(sapply(strsplit(x = corpusDtmDocnames,
                                                      split = ".", fixed = TRUE),function(x) x[2]))
            if (relFreq == TRUE) {
                totalWords <- data.frame(namesOfWebsites, totalWords = NA)
                for (i in seq_along(namesOfWebsites)) {
                    totalWords$totalWords[i] <- sum(corpusDtm[grepl(pattern = namesOfWebsites[i], corpusDtmDocnames)])
                }
            }
            corpusDtm <- quanteda::applyDictionary(corpusDtm, termsDic)
            wordFrequency <- data.frame()
            for (i in seq_along(namesOfWebsites)) {
                temp <- quanteda::topfeatures(x = corpusDtm[grepl(pattern = namesOfWebsites[i],
                                                                  corpusDtmDocnames)],
                                              n = number)
                tempDF <- data.frame(term = names(temp), freq = temp, type = namesOfWebsites[i])
                wordFrequency <- rbind(wordFrequency, tempDF)
            }
            if (relFreq == TRUE) {
                wordFrequency$freq <- wordFrequency$freq/totalWords$totalWords
            }
        } else if (byDate == TRUE & byWebsite == FALSE) {
            dates <- unique(lubridate::year(sapply(strsplit(x = corpusDtmDocnames,
                                                            split = ".", fixed = TRUE),function(x) x[1])))
            if (relFreq == TRUE) {
                totalWords <- data.frame(dates, totalWords = NA)
                for (i in seq_along(dates)) {
                    totalWords$totalWords[i] <- sum(corpusDtm[grepl(pattern = dates[i], corpusDtmDocnames)])
                }
            }
            corpusDtm <- quanteda::applyDictionary(corpusDtm, termsDic)
            wordFrequency <- data.frame()
            for (i in seq_along(dates)) {
                temp <- quanteda::topfeatures(x = corpusDtm[grepl(pattern = dates[i],
                                                                  corpusDtmDocnames)], n = number)
                tempDF <- data.frame(term = names(temp), freq = temp, type = dates[i])
                wordFrequency <- rbind(wordFrequency, tempDF)
            }
            wordFrequency$type <- as.factor(wordFrequency$type)
            if (relFreq == TRUE) {
                wordFrequency$freq <- wordFrequency$freq/totalWords$totalWords
            }
        } else {
            if (relFreq == TRUE) {
                corpusDtm <- quanteda::weight(corpusDtm, "relFreq")
            }
            corpusDtm <- quanteda::applyDictionary(corpusDtm, termsDic)
            freq <- quanteda::topfeatures(x = corpusDtm, n = number)
        }
    } else {
        if (is.null(terms) == FALSE&relFreq==FALSE) {
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
        time <- as.character(strptime(as.POSIXct(unlist(NLP::meta(corpus, "datetimestamp")),
                                                 origin = "1970-01-01"), "%Y-%m-%d"))
        dates <- unique(lubridate::year(time))
        corpusDtm <- corpusDtm[,base::match(terms, colnames(corpusDtm))]
        wordFrequency <- data.frame()
        for (i in seq_along(dates)) {
            temp <- sort(slam::col_sums(corpusDtm[lubridate::year(time)==dates[i],], na.rm = TRUE),
                         decreasing = TRUE)
            tempDF <- data.frame(term = names(temp), freq = temp, type = dates[i])
            wordFrequency <- rbind(wordFrequency, tempDF)
        }
        wordFrequency$type <- as.factor(wordFrequency$type)
    }
    if (byWebsite==TRUE&byDate==FALSE&quanteda::is.dfm(corpusDtm)==FALSE) {
        namesOfWebsites <- levels(as.factor(unlist(NLP::meta(corpus, "author"))))
        byWebsiteL <- data.frame(matrix(NA, nrow = length(corpus), ncol = length(namesOfWebsites)))
        names(byWebsiteL) <- namesOfWebsites
        for (i in 1:length(namesOfWebsites)) {
            byWebsiteL[, i] <- NLP::meta(corpus, "author") == namesOfWebsites[i]
        }
        mostFrequentByWebsite <- as.data.frame(matrix(nrow = length(byWebsiteL), ncol = length(terms) + 1))
        colnames(mostFrequentByWebsite)[1] <- "website"
        for (i in 1:length(terms)) {
            colnames(mostFrequentByWebsite)[i + 1] <- terms[i]
        }
        for (i in 1:length(byWebsiteL)) {
            dtmTemp <- corpusDtm[byWebsiteL[, i], ]
            mostFrequent <- castarter::ShowFreq(corpusDtm = dtmTemp, mode = "data.frame", number = "all", terms = terms)
            totalWords <- sum(dtmTemp)
            mostFrequentByWebsite[i, 1] <- names(byWebsiteL)[i]
            for (j in 1:length(terms)) {
                value <- mostFrequent$freq[mostFrequent$term == terms[j]]
                if (length(value) == 0) {
                    value <- 0
                } else {
                    if (relFreq == FALSE) {
                        mostFrequentByWebsite[i, j + 1] <- value
                    } else {
                        mostFrequentByWebsite[i, j + 1] <- value/totalWords
                    }
                }
            }
        }
        wordFrequency <- mostFrequentByWebsite
        colnames(wordFrequency) <- c("type", "freq")
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
            wordFrequency$tipology <- NA
            tipology$type <- as.character(tipology$type)
            if (byWebsite == TRUE) {
                for (i in 1:nrow(wordFrequency)) {
                    if (is.element(wordFrequency$type[i], tipology$term)) {
                        wordFrequency$tipology[i] <- tipology$type[tipology$term == wordFrequency$type[i]]
                    } else {
                        wordFrequency$tipology[i] <- NA
                    }
                }
                barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(type, freq), y = freq, fill = tipology)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="Type")
            } else {
                for (i in 1:nrow(wordFrequency)) {
                    if (is.element(wordFrequency$term[i], tipology$term)) {
                        wordFrequency$tipology[i] <- tipology$type[tipology$term == wordFrequency$term[i]]
                    } else {
                        wordFrequency$tipology[i] <- NA
                    }
                }
                barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, freq), y = freq, fill = tipology)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="Type")
            }
        } else if (byWebsite == TRUE | byDate == TRUE) {
            if (stacked == TRUE) {
                if (invert == FALSE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq, fill = type)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="")
                } else if (invert == TRUE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq, fill = term)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="Term")
                }
            } else if (stacked == FALSE) {
                if (invert == FALSE) {
                    barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq, fill = type)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="")
                } else if (invert == TRUE) {
                    if (length(terms)==1) {
                        if (order == TRUE) {
                        barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(type, freq), y = freq)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("")
                        } else {
                            barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("")
                        }
                    } else {
                        barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = type, y = freq, fill = term)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::labs(fill="Term")
                    }
                }
            }
        } else {
            barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = reorder(term, -freq), y = freq)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() + ggplot2::xlab("")
        }
        barchart <- barchart + ggplot2::scale_fill_brewer(palette = "Dark2")
        if (byDate == TRUE&length(terms)==1) {
            if (byWebsite == TRUE) {
                barchart <-  barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = factor(type, levels=rev(levels(type))), y = freq, fill = Year)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse=TRUE))
            } else {
            barchart <-  barchart <- ggplot2::ggplot(data = wordFrequency, ggplot2::aes(x = factor(type, levels=rev(levels(type))), y = freq)) + ggplot2::geom_bar(stat = "identity", position="dodge") + ggplot2::coord_flip() + ggplot2::xlab("")
            }
        }
        if (percent == TRUE&relFreq == TRUE) {
            barchart <- barchart + ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::ylab("Frequency of term as % of all words")
        } else {
            barchart <- barchart + ggplot2::ylab("Word frequency")
        }
        if (is.null(customTitle) == FALSE) {
            barchart <- barchart + ggplot2::ggtitle(customTitle)
        } else {
            barchart <- barchart + ggplot2::ggtitle(paste("Word frequency barchart for", paste(terms, collapse = ", ")))
        }
        if (export == TRUE) {
            if (is.null(customTitle)) {
                customTitle <- paste(project, "word frequency", paste(names(terms), collapse = " - "), sep = " - ")
            }
            if (is.null(project) == FALSE & is.null(website) == FALSE) {
                if (file.exists(file.path(project, website, "Outputs")) == FALSE) {
                    dir.create(file.path(project, website, "Outputs"))
                }
                ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".png")))
                print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".svg")))
                print(paste("File saved in", file.path(project, website, "Outputs", paste0(paste(customTitle, project, website, sep = " - "), ".svg"))))
            } else if (is.null(project) == FALSE & is.null(website) == TRUE) {
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".png")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".png"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".pdf")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".pdf"))))
                ggplot2::ggsave(file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".svg")))
                print(paste("File saved in", file.path("Outputs", paste0(paste(customTitle, project, sep = " - "), ".svg"))))
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
