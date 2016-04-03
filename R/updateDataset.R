#' Updates a dataset
#' 
#' Updates a dataset given a previously created parameters export file.
#'  
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return A castarter dataset.
#' @export
#' @examples
#' dataset <- UpdateDataset(dataset, nameOfProject, nameOfWebsite)
UpdateDataset <- function(dataset, nameOfProject, nameOfWebsite, numberOfIndexPages = 10, wget = FALSE, wait = 3) {
    params <- read.table(file = file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
    params$param[params$param == "NULL"] <- NA
    CreateFolderStructure(nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite)
    indexLinks <- CreateLinks(linkFirstChunk = params$param[params$args=="linkFirstChunk"], linkSecondChunk = params$param[params$args=="linkSecondChunk"], startPage = as.integer(params$param[params$args=="startPage"]), endPage = sum(as.integer(params$param[params$args=="startPage"]), numberOfIndexPages), increaseBy = as.integer(params$param[params$args=="increaseBy"]), sortIndexLinks  = as.logical(params$param[params$args=="sortIndexLinks"]))
    indexHtml <- DownloadIndex(nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite, indexLinks = indexLinks, wget = wget, wait = wait)
    articlesLinks <- ExtractLinks(domain = params$param[params$args=="domain"], partOfLink = params$param[params$args=="partOfLink"], indexHtml = indexHtml, containerType = params$param[params$args=="containerType"], containerClass = params$param[params$args=="containerClass"], divClass = params$param[params$args=="divClassExtractLinks"], partOfLinkToExclude = strsplit(params$param[params$args=="divClassExtractLinks"], "§"))
    articlesLinks <- articlesLinks[is.element(articlesLinks, dataset$articlesLinks)==FALSE]
    articlesLinks <- c(dataset$articlesLinks, articlesLinks)
    articlesHtml <- DownloadArticles(nameOfProject, nameOfWebsite, articlesLinks, start = sum(length(dataset$articlesLinks), 1), wait = wait, wget = wget)
    titles <- ExtractTitles(articlesHtml = articlesHtml, articlesLinks = articlesLinks, titlesExtractMethod = params$param[params$args=="titlesExtractMethod"], removePunctuation = params$param[params$args=="removePunctuationExtractTitle"], onlyStandardCharacters = params$param[params$args=="onlyStandardCharactersExtractTitles"], removeString = params$param[params$args=="removeStringExtractTitles"], removeEverythingAfter = params$param[params$args=="removeEverythingAfterExtractTitles"], customXpath = params$param[params$args=="customXpathExtractTitles"], maxCharacters = params$param[params$args=="maxCharactersExtractTitles"])
    if (extractDatesXpath == TRUE) {
        
    } else {
        dates <- ExtractDates(articlesHtml = articlesHtml, dateFormat = params$param[params$args=="dateFormat"], language = dataset$language[1], customString = params$param[params$args=="customStringExtractDates"], minDate = params$param[params$args=="minDate"], maxDate = params$param[params$args=="maxDate"], removeEverythingBefore = params$param[params$args=="removeEverythingBeforeExtractDates"])
    }
    articlesId <- ExtractArticleId(nameOfProject, nameOfWebsite)
    language <- dataset$language[1]
    metadata <- ExportMetadata(nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite, dates = dates, articlesId = articlesId, titles = titles, language = language, articlesLinks = articlesLinks)
    articlesTxt <- ExtractTxt(articlesHtml = articlesHtml, metadata = metadata, export = params$param[params$args=="exportExtractTxt"], maxTitleCharacters = params$param[params$args=="maxTitleCharacters"], textToBeRemoved = unlist(stringr::str_split(string = params$param[params$args=="textToBeRemovedExtractTxt"], pattern = "§§§")), divClass = params$param[params$args=="divClassExtractTxt"], divID = params$param[params$args=="divIDExtractTxt"], removeEverythingAfter = params$param[params$args=="removeEverythingAfterExtractTxt"], removeEverythingBefore = params$param[params$args=="removeEverythingBeforeExtractTxt"], removePunctuationInFilename = params$param[params$args=="removePunctuationInFilename"], keepEverything = params$param[params$args=="keepEverything"])
    dataset <- rbind(dataset, cbind(metadata, articlesTxt, stringsAsFactors = FALSE))
    dataset
}
                