#' Updates a dataset
#' 
#' Updates a dataset given a previously created parameters export file.
#'  
#' @param dataset A 'castarter' dataset.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A castarter dataset.
#' @export
#' @examples
#' dataset <- UpdateDataset(dataset)
UpdateDataset <- function(dataset, articlesLinks = NULL, numberOfIndexPages = 10, wget = FALSE, wait = 1, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    params <- read.table(file = file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")), stringsAsFactors = FALSE)
    params$param[params$param == "NULL"] <- NA
    params$param[params$param == ""] <- NA
    params$param[params$param == "NA"] <- NA
    params$param[params$param == "FALSE"] <- FALSE
    params$param[params$param == "TRUE"] <- TRUE
    castarter::CreateFolders(project = project, website = website)
    if (base::is.null(articlesLinks)==TRUE) {
        indexLinks <- CreateLinks(linkFirstChunk = params$param[params$args=="linkFirstChunk"], linkSecondChunk = params$param[params$args=="linkSecondChunk"], startPage = as.integer(params$param[params$args=="startPage"]), endPage = sum(as.integer(params$param[params$args=="startPage"]), numberOfIndexPages), increaseBy = as.integer(params$param[params$args=="increaseBy"]), sortIndexLinks  = as.logical(params$param[params$args=="sortIndexLinks"]))
        DownloadContents(links = indexLinks, type = "index", wait = wait)
        indexHtml <- ImportHtml(from = "index")
        articlesLinks <- ExtractLinks(domain = params$param[params$args=="domain"], partOfLink = params$param[params$args=="partOfLink"], indexHtml = indexHtml, containerType = params$param[params$args=="containerTypeExtractLinks"], containerClass = params$param[params$args=="containerClassExtractLinks"], divClass = params$param[params$args=="divClassExtractLinks"], partOfLinkToExclude = strsplit(params$param[params$args=="partOfLinkToExclude"], "§§§"))
    }
    articlesLinksNew <- articlesLinks[is.element(articlesLinks, dataset$articlesLinks)==FALSE]
    previousLength <- length(dataset$articlesLinks)
    articlesLinks <- c(dataset$articlesLinks, articlesLinksNew)
    DownloadContents(links = articlesLinks, type = "articles", project = project, website = website, wget = wget, missingArticles = TRUE, wait = wait, start = sum(previousLength, 1))
    DownloadContents(links = articlesLinks, type = "articles", project = project, website = website, wget = wget, missingArticles = FALSE, wait = wait, start = sum(previousLength, 1))
    articlesHtmlNew <- ImportHtml(from = "articles", project = project, website = website)
    titles <- ExtractTitles(articlesHtml = articlesHtmlNew, articlesLinks = articlesLinksNew, method = params$param[params$args=="method_ExtractTitles"], removePunctuation = params$param[params$args=="removePunctuation_ExtractTitles"], onlyStandardCharacters = params$param[params$args=="onlyStandardCharacters_ExtractTitles"], removeString = params$param[params$args=="removeString_ExtractTitles"], removeEverythingAfter = params$param[params$args=="removeEverythingAfter_ExtractTitles"], customXpath = params$param[params$args=="customXpath_ExtractTitles"], maxCharacters = params$param[params$args=="maxCharacters_ExtractTitles"], exportParameters = FALSE)
    language_ExtractDates <- params$param[params$args=="language_ExtractDates"]
    if (gtools::invalid(language_ExtractDates) == TRUE) {
        language_ExtractDates <- Sys.getlocale(category = "LC_TIME")
    }
    dates <- ExtractDates(articlesHtml = articlesHtmlNew, dateFormat = params$param[params$args=="dateFormat_ExtractDates"], customString = params$param[params$args=="customString_ExtractDates"], divClass = params$param[params$args=="divClass_ExtractDates"], spanClass = params$param[params$args=="spanClass_ExtractDates"], customXpath = params$param[params$args=="customXpath_ExtractDates"], language = language_ExtractDates, keepAllString = params$param[params$args=="keepAllString_ExtractDates"], minDate = params$param[params$args=="minDate"], maxDate = params$param[params$args=="maxDate"], removeEverythingBefore = params$param[params$args=="removeEverythingBefore_ExtractDates"], exportParameters = FALSE)
    id <- ExtractId(project, website)
    language <- dataset$language[1]
    metadata <- ExportMetadata(project = project, website = website, dates = dates, id = id, titles = titles, language = language, articlesLinks = articlesLinksNew)
    contents <- ExtractTxt(articlesHtml = articlesHtmlNew, metadata = metadata, export = FALSE, maxTitleCharacters = params$param[params$args=="maxTitleCharacters"], removeString = unlist(stringr::str_split(string = params$param[params$args=="removeString_ExtractTxt"], pattern = "§§§")), divClass = params$param[params$args=="divClass_ExtractTxt"], divId = params$param[params$args=="divId_ExtractTxt"], removeEverythingAfter = params$param[params$args=="removeEverythingAfter_ExtractTxt"], removeEverythingBefore = params$param[params$args=="removeEverythingBefore_ExtractTxt"], removePunctuationInFilename = params$param[params$args=="removePunctuationInFilename"], keepEverything = params$param[params$args=="keepEverything_ExtractTxt"], removeTitleFromTxt = params$param[params$args=="removeTitleFromTxt"], exportParameters = FALSE)
    dataset <- rbind(dataset, cbind(metadata, contents, stringsAsFactors = FALSE))
    invisible(dataset)
}
                