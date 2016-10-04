#' Generates links to index pages listing individual articles.. 
#' 
#' Generates links to index pages listing individual articles.
#'  
#' @param linkFirstChunk First part of index link that does not change in other index pages.
#' @param linkSecondChunk Part of index link appneded after the part of the link that varies. If not relevant, may be left empty. 
#' @param startPage, endPage If the links include a numerical component, define first and last number of the sequence.startPage defaults to 1, endPage defaults to 10.
#' @param increaseBy Defines by how much the number in the link should be increased in the numerical sequence. Defaults to 1.
#' @param dateFormat A charachter string that defines the format of the date to incldue in the link. Available options are: "Y" (e.g. 2015), "Ym" (2015-10), "Ymd" (e.g. 2015-10-24).
#' @param leadingZero Defaults to TRUE. If TRUE, days and months of one digit are preceded by a zero (e.g. 07/05/2014).
#' @param export Exports the links in a .txt file in the folder project/website.
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus. Requires parameters project/website.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Not required if previously set with SetCastarter(project = "project", website = "website")
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Not required if previously set with SetCastarter(project = "project", website = "website")
#' @return A character vector of links to index pages. 
#' @export
#' @examples
#' indexLinks <- CreateLinks("http://www.example.com/news/")
CreateLinks <- function(linkFirstChunk, linkSecondChunk = NULL, startPage = 1, endPage = 10, increaseBy = 1, dateFormat = NULL, 
                        firstYear = "", lastYear = "", leadingZero = TRUE, startDate = "", endDate = "", sortIndexLinks = FALSE, dateSeparator = "/", export = FALSE, 
                        reversedOrder = FALSE, exportParameters = TRUE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (gtools::invalid(dateFormat) == FALSE) {
        if (dateFormat == "ym" | dateFormat == "Ym") {
            years <- firstYear:lastYear
            dates <- vector()
            for (i in years) {
                if (leadingZero == TRUE) {
                    newDates <- base::paste(base::rep(i, 12), base::sprintf("%02d", 1:12), sep = dateSeparator)
                } else {
                    newDates <-base:: paste(rep(i, 12), 1:12, sep = dateSeparator)
                }
                dates <- base::c(dates, newDates)
            }
            indexLinks <- base::paste0(linkFirstChunk, dates)
        } else if (dateFormat == "Y") {
            years <- firstYear:lastYear
            indexLinks <- paste0(linkFirstChunk, years)
        } else if (dateFormat == "ymd" | dateFormat == "Ymd" ) {
            dates <- base::seq(as.Date(startDate), as.Date(endDate), by = "day")
            dates <- base::format(as.Date(dates), paste("%Y", "%m", "%d", sep = dateSeparator))
            indexLinks <- paste0(linkFirstChunk, dates)
        } else if (dateFormat == "dmY") {
            dates <- base::seq(as.Date(startDate), as.Date(endDate), by = "day")
            dates <- base::format(as.Date(dates), paste("%d", "%m", "%Y", sep = dateSeparator))
            indexLinks <- paste0(linkFirstChunk, dates)
        } 
    }
    if (gtools::invalid(linkSecondChunk) == TRUE | gtools::invalid(linkSecondChunk) == TRUE) {
        if (gtools::invalid(dateFormat) == FALSE) {
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        } else {
            listOfNumbers <- base::seq(startPage, endPage, increaseBy)
            if (base::is.element(endPage, listOfNumbers) == FALSE) {
                listOfNumbers <- base::c(listOfNumbers, endPage)
            }
            indexLinks <- base::cbind(rep(linkFirstChunk, length(listOfNumbers)), listOfNumbers)
            indexLinks <- base::paste0(indexLinks[, 1], indexLinks[, 2])
        }
    }
    if (gtools::invalid(linkSecondChunk) == FALSE & gtools::invalid(linkSecondChunk) == FALSE) {
        if (gtools::invalid(dateFormat) == FALSE) {
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        } else {
            listOfNumbers <- base::seq(startPage, endPage, increaseBy)
            indexLinks <- base::cbind(base::rep(linkFirstChunk, base::length(listOfNumbers)), listOfNumbers)
            indexLinks <- base::paste0(indexLinks[, 1], indexLinks[, 2])
            indexLinks <- base::paste0(indexLinks, linkSecondChunk)
        }
    }
    if (sortIndexLinks == TRUE) {
        indexLinks <- gtools::mixedsort(indexLinks)
    }
    if (export == TRUE) {
        base::writeLines(indexLinks, base::file.path(project, website, "Logs", paste(Sys.Date(), website, "indexLinks.txt", sep = " - ")))
    }
    if (exportParameters == TRUE) {
        args <- c("linkFirstChunk", "linkSecondChunk", "startPage", "endPage", "increaseBy", "dateFormat_CreateLinks", "firstYear", "lastYear", "leadingZero", "startDate", "endDate", "sortIndexLinks", "dateSeparator", "export", "reversedOrder", "exportParameters", "project", "website")
        param <- list(linkFirstChunk, linkSecondChunk, startPage, endPage, increaseBy, dateFormat, firstYear, lastYear, leadingZero, startDate, endDate, sortIndexLinks, dateSeparator, export, reversedOrder, exportParameters, project, website)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - "))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")))
    }
    if (reversedOrder == TRUE) {
        base::rev(indexLinks)
    } else {
        indexLinks
    }
}