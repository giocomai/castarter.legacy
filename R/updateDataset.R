#' Updates a dataset
#'
#' Updates a dataset given a previously created parameters export file.
#'
#' @param dataset A 'castarter' dataset.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return A castarter dataset.
#' @export
#' @examples
#' dataset <- UpdateDataset(dataset)
UpdateDataset <- function(dataset, links = NULL, numberOfIndexPages = 10, wget = FALSE, wait = 1, project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    params <- read.table(file = file.path(project, website, "Logs", paste(website, "updateParameters.csv", sep = " - ")), stringsAsFactors = FALSE)
    params$param[params$param == "NULL"] <- NA
    params$param[params$param == ""] <- NA
    params$param[params$param == "NA"] <- NA
    params$param[params$param == "FALSE"] <- FALSE
    params$param[params$param == "TRUE"] <- TRUE
    castarter::CreateFolders(project = project, website = website)
    if (base::is.null(links)==TRUE) {
        indexLinks <- CreateLinks(linkFirstChunk = params$param[params$args=="linkFirstChunk"], linkSecondChunk = params$param[params$args=="linkSecondChunk"], startPage = as.integer(params$param[params$args=="startPage"]), endPage = sum(as.integer(params$param[params$args=="startPage"]), numberOfIndexPages), increaseBy = as.integer(params$param[params$args=="increaseBy"]))
        DownloadContents(links = indexLinks, type = "index", wait = wait)
        indexHtml <- ImportHtml(from = "index")
    #    links <- ExtractLinks(domain = params$param[params$args=="domain"], partOfLink = params$param[params$args=="partOfLink"], html = indexHtml, containerType = params$param[params$args=="containerTypeExtractLinks"], containerClass = params$param[params$args=="containerClassExtractLinks"], divClass = params$param[params$args=="divClassExtractLinks"], partOfLinkToExclude = strsplit(params$param[params$args=="partOfLinkToExclude"], "§§§"))
    }
    linksNew <- links[is.element(links, dataset$link)==FALSE]
    previousLength <- length(dataset$link)
    links <- c(dataset$link, linksNew)
    DownloadContents(links = links, type = "articles", project = project, website = website, wget = wget, missingPages = TRUE, wait = wait, start = sum(previousLength, 1))
    DownloadContents(links = links, type = "articles", project = project, website = website, wget = wget, missingPages = FALSE, wait = wait, start = sum(previousLength, 1))

    #titles <- ExtractTitles(links = linksNew, method = params$param[params$args=="method_ExtractTitles"], removePunctuation = params$param[params$args=="removePunctuation_ExtractTitles"], onlyStandardCharacters = params$param[params$args=="onlyStandardCharacters_ExtractTitles"], removeString = params$param[params$args=="removeString_ExtractTitles"], removeEverythingAfter = params$param[params$args=="removeEverythingAfter_ExtractTitles"], customXpath = params$param[params$args=="customXpath_ExtractTitles"], maxCharacters = params$param[params$args=="maxCharacters_ExtractTitles"], exportParameters = FALSE)
    #language_ExtractDates <- params$param[params$args=="language_ExtractDates"]
    if (is.null(language_ExtractDates) == TRUE) {
        language_ExtractDates <- Sys.getlocale(category = "LC_TIME")
    }
  #  dates <- ExtractDates(dateFormat = params$param[params$args=="dateFormat_ExtractDates"], customString = params$param[params$args=="customString_ExtractDates"], customXpath = params$param[params$args=="customXpath_ExtractDates"], language = language_ExtractDates, keepAllString = params$param[params$args=="keepAllString_ExtractDates"], minDate = params$param[params$args=="minDate"], maxDate = params$param[params$args=="maxDate"], removeEverythingBefore = params$param[params$args=="removeEverythingBefore_ExtractDates"], exportParameters = FALSE)
    id <- ExtractId(project, website)
    language <- dataset$language[1]
    metadata <- ExportMetadata(project = project, website = website, dates = dates, id = id, titles = titles, language = language, links = linksNew)
   # contents <- ExtractText(metadata = metadata, export = FALSE, maxTitleCharacters = params$param[params$args=="maxTitleCharacters"], removeString = unlist(stringr::str_split(string = params$param[params$args=="removeString_ExtractTxt"], pattern = "§§§")), removeEverythingAfter = params$param[params$args=="removeEverythingAfter_ExtractTxt"], removeEverythingBefore = params$param[params$args=="removeEverythingBefore_ExtractTxt"], removePunctuationInFilename = params$param[params$args=="removePunctuationInFilename"], keepEverything = params$param[params$args=="keepEverything_ExtractTxt"], removeTitleFromTxt = params$param[params$args=="removeTitleFromTxt"], exportParameters = FALSE)
    dataset <- rbind(dataset, cbind(metadata, contents, stringsAsFactors = FALSE))
    invisible(dataset)
}
