#' Updates a dataset
#'
#' Updates a dataset given a previously created parameters export file.
#'
#' @param dataset A 'castarter' dataset, defaults to NULL. If not given, and project and website are given (possily, through `SetCastarter(project = ..., website =)`), the latest dataset is automatically loaded.
#' @param links A chacter vector, defaults to NULL. If given, it is checked against the given dataset: new links are downloaded, the correspondent pages extracted, and the result added to the dataset. If not given, the function downloads index pages along the criteria found in the stores param file and continues downloading them as long as there is at least a new link in an index page. Based on this, it then proceeds to update the dataset.
#' @param indexLinks A chacter vector, defaults to NULL. If given, the presence of new articles is checked after downloading the provided index pages.
#' @param maxNumberOfIndexPages An integer, defaults to 1000 to prevent function from running indefinitely (however, this can be set to `Inf`). Maximum number of index pages to be downloaded before the function proceeds with updating the dataset.
#' @param wait Defaults to 1. Number of seconds to wait between downloading one page and the next. Can be increased to reduce server load, or can be set to 0 when this is not an issue.
#' @param exportRds Logical, defaults to TRUE. If TRUE, it stores the updated dataset in the Dataset folder in .rds format.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return A castarter dataset.
#' @export
#' @examples
#' \dontrun{
#' dataset <- UpdateDataset()
#' }
UpdateDataset <- function(dataset = NULL,
                          links = NULL,
                          indexLinks = NULL,
                          maxNumberOfIndexPages = 1000,
                          wait = 1,
                          exportRds = TRUE,
                          use_headless_chromium = FALSE,
                          project = NULL,
                          website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (is.null(dataset) == TRUE) {
        dataset <- LoadDatasets(projectsAndWebsites = paste(project, website, sep = "/"))
    }
    castarter::CreateFolders(project = project, website = website)
    paramsFile <- base::file.path(baseFolder, project, website, "Logs", paste(project, website, "parameters.rds", sep = "-"))
    params <- readRDS(paramsFile)
    if (base::is.null(links)==TRUE) {
        if(is.null(indexLinks)== FALSE) {
            DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
            DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
            DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
            DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)

            newLinks <- ExtractLinks(importParameters = TRUE,
                                     project = project,
                                     website = website)
        } else {
            if (is.null(params$CreateLinks$dateFormat)) {

                message("Downloading index pages as long as they include links to pages not included in the dataset")

                indexLinks <- CreateLinks(linkFirstChunk = params$CreateLinks$linkFirstChunk,
                                          linkSecondChunk = params$CreateLinks$linkSecondChunk,
                                          startPage = params$CreateLinks$startPage,
                                          endPage = params$CreateLinks$startPage,
                                          exportParameters = FALSE)
                DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)

                links <- ExtractLinks(importParameters = TRUE,
                                      project = project,
                                      website = website)

                while(sum(is.element(el = links, set = dataset$link))<length(links)&length(indexLinks)<=maxNumberOfIndexPages) {
                    indexLinks <- CreateLinks(linkFirstChunk = params$CreateLinks$linkFirstChunk,
                                              linkSecondChunk = params$CreateLinks$linkSecondChunk,
                                              startPage = params$CreateLinks$startPage,
                                              endPage = params$CreateLinks$startPage+(params$CreateLinks$increaseBy*length(indexLinks)),
                                              increaseBy = params$CreateLinks$increaseBy,
                                              exportParameters = FALSE)
                    DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                    DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                    DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                    DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)

                    links <- ExtractLinks(importParameters = TRUE,
                                          id = length(indexLinks),
                                          project = project,
                                          website = website)

                }
                if (length(indexLinks)==maxNumberOfIndexPages) {
                    warning("Download of index pages stopped because maxNumberOfIndexPages has been reached. There may be other new pages that have not been downloaded.")
                }
                message("\nExtracting links to new pages")
                newLinks <- ExtractLinks(importParameters = TRUE, id = NULL)
            } else {

                message("\nDownloading index pages between the last day available in the dataset and today.")

                indexLinks <- CreateLinks(linkFirstChunk = params$CreateLinks$linkFirstChunk,
                                          linkSecondChunk = params$CreateLinks$linkSecondChunk,
                                          dateFormat = params$CreateLinks$dateFormat,
                                          startDate = max(dataset$date),
                                          endDate = as.character(as.Date(Sys.Date())),
                                          dateSeparator = params$CreateLinks$dateSeparator,
                                          exportParameters = FALSE)
                DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)
                DownloadContents(links = indexLinks, type = "index", missingPages = FALSE, wait = wait, project = project, website = website, use_headless_chromium = use_headless_chromium)

                message("\nExtracting links to new pages")

                newLinks <- ExtractLinks(importParameters = TRUE,
                                         project = project,
                                         website = website)

            }
        }

    } else {
        newLinks <- links
    }
    newLinks <- newLinks[!is.element(el = newLinks, set = dataset$link)]
    allLinks <- c(dataset$link, newLinks)
    toDownloadL <- is.element(el = allLinks, set = newLinks)
    message(paste(sum(toDownloadL), "new pages founds."))
    Sys.sleep(time = 1)
    DownloadContents(links = allLinks,
                     type = "articles",
                     linksToCheck = toDownloadL,
                     wait = wait, project = project, website = website,
                     use_headless_chromium = use_headless_chromium)

    DownloadContents(links = allLinks,
                     type = "articles",
                     linksToCheck = toDownloadL,
                     wait = wait,
                     project = project,
                     website = website,
                     missingPages = FALSE,
                     use_headless_chromium = use_headless_chromium)

    DownloadContents(links = allLinks,
                     type = "articles",
                     linksToCheck = toDownloadL,
                     wait = wait,
                     project = project,
                     website = website,
                     use_headless_chromium = use_headless_chromium)

    DownloadContents(links = allLinks,
                     type = "articles",
                     linksToCheck = toDownloadL,
                     wait = wait,
                     project = project,
                     website = website,
                     missingPages = FALSE,
                     use_headless_chromium = use_headless_chromium)

    # Extract metadata
    id <- ExtractId(project = project, website = website)
    message("\nExtracting titles")
    titles <- ExtractTitles(id = id,
                            importParameters = TRUE,
                            exportParameters = FALSE,
                            project = project,
                            website = website)
    message("\nExtracting dates")
    dates <- ExtractDates(id = id,
                          importParameters = TRUE,
                          exportParameters = FALSE,
                          project = project,
                          website = website)
    if (sum(is.na(dates))>0) {
        warning(paste("\nThe date could not be extracted for", sum(is.na(dates)), "of the", length(id), "new pages found."))
    }

    language <- dataset$language[1]

    metadata <- ExportMetadata(project = project,
                               website = website,
                               dates = dates,
                               id = id,
                               titles = titles,
                               language = language,
                               links = allLinks)

    message("\nExtracting text")

    text <- ExtractText(id = id,
                        importParameters = TRUE,
                        exportParameters = FALSE)
    newDataset <- ExportDataset(text = text, metadata = metadata, exportRds = FALSE)

    dataset <- dplyr::bind_rows(dataset, newDataset)

    if (exportRds==TRUE) {
        message("\nSaving dataset in .rds format.")
        ExportDataset(dataset = dataset,
                      exportRds = TRUE,
                      project = project,
                      website = website)
    }
    message(paste("\n", sum(toDownloadL), "new pages added to dataset."))
    invisible(dataset)
}
