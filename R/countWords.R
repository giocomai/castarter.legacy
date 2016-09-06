#' Count total words by date and website
#' 
#' Count total words by date and website
#'  
#' @param corpusDtm A document-term matrix created with the 'TM' package or a document-feature matrix of the 'quanteda' type.
#' @param byDay Logical, defaults to FALSE. If TRUE, overrides byYear. 
#' @return A data.frame
#' @export
#' @examples
#' totalWords <- CountWords(corpusDtm)

CountWords <- function(corpusDtm, byWebsite = TRUE, byYear = TRUE, byDay = FALSE)  {
    corpusDtmDocnames <- quanteda::docnames(corpusDtm)
    if (byYear == TRUE) {
        namesOfWebsites <- unique(sapply(strsplit(x = corpusDtmDocnames,
                                                  split = ".", fixed = TRUE),function(x) x[2]))
        dates <- unique(lubridate::year(sapply(strsplit(x = corpusDtmDocnames,
                                                        split = ".", fixed = TRUE),function(x) x[1])))
        totalWords <- data.frame(namesOfWebsites = as.character(sapply(namesOfWebsites, rep, times = length(dates))), dates, totalWords = NA)
    }
    if (byDay == TRUE) {
        namesOfWebsites <- sapply(strsplit(x = corpusDtmDocnames, split = ".", fixed = TRUE),function(x) x[2])
        dates <- lubridate::ymd(sapply(strsplit(x = corpusDtmDocnames, split = ".", fixed = TRUE),function(x) x[1]))
        totalWords <- data.frame(namesOfWebsites, dates, totalWords = NA)
    }
    for (i in 1:nrow(totalWords)) {
        totalWords$totalWords[i] <- sum(corpusDtm[grepl(pattern = paste(totalWords$dates[i], totalWords$namesOfWebsites[i], sep = ".*"), corpusDtmDocnames)])
    }
    return(totalWords)
}