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
    if (extractDatesXpath == TRUE) {
        
    } else {
        dates <- ExtractDates()
        }
    
    
    
}
                