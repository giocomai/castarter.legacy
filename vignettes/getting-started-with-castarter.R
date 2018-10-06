## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  citation("castarter")

## ----eval=FALSE----------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("giocomai/castarter")
#  library("castarter")

## ----eval=TRUE, include=FALSE--------------------------------------------
library("castarter")

## ------------------------------------------------------------------------
#  SetCastarter(project = "Presidents", website = "Kremlin_en")

## ----eval=FALSE----------------------------------------------------------
#  CreateFolders()

## ------------------------------------------------------------------------
#  indexLinks <- CreateLinks(linkFirstChunk = "http://en.kremlin.ru/events/president/news/page/",
#                            startPage = 1,
#                            endPage = 1082)
#  head(indexLinks)

## ----eval=FALSE----------------------------------------------------------
#  DownloadContents(links = indexLinks, type = "index")

## ----eval=FALSE----------------------------------------------------------
#  DownloadContents(links = indexLinks, type = "index", missingPages = FALSE)

## ------------------------------------------------------------------------
#  
#  links <- ExtractLinks(domain = "http://en.kremlin.ru/",
#                        partOfLink = "president/news/",
#                        partOfLinkToExclude = c("page",
#                                                "photos",
#                                                "videos",
#                                                "special"))
#  

## ------------------------------------------------------------------------
#  length(links)/length(indexLinks)

## ------------------------------------------------------------------------
#  DownloadContents(links = links)

## ------------------------------------------------------------------------
#  DownloadContents(links = links, missingPages = FALSE)

## ------------------------------------------------------------------------
#  #### 1. Preliminary steps ####
#  
#  ## Install castarter (devtools required for installing from github)
#  # install.packages("devtools")
#  devtools::install_github(repo = "giocomai/castarter")
#  
#  ## Load castarter
#  library("castarter")
#  
#  ## Set project and website name.
#  # They will remain in memory in the current R session,
#  # so need need to include them in each function call.
#  SetCastarter(project = "Presidents", website = "Kremlin_en")
#  
#  ## Creates the folder structure where files will be saved within the
#  ## current working directory
#  CreateFolders()
#  
#  #### 2. Download index/archive pages ####
#  
#  # Check out the archive page, e.g. http://en.kremlin.ru/events/president/news/
#  # See what is the last archive page, if they are defined by consecutive numbers
#  
#  indexLinks <- CreateLinks(linkFirstChunk = "http://en.kremlin.ru/events/president/news/page/",
#                            startPage = 1,
#                            endPage = 1082)
#  
#  ## Downloads all html files of the archive pages
#  # This can be interrupted: by default, if the command is issued again it will download only missing pages (i.e. pages that have not been downloaded previously)
#  DownloadContents(links = indexLinks, type = "index")
#  # Downloads again files with oddly small size (usually, errors), if any.
#  DownloadContents(links = indexLinks, type = "index", missingPages = FALSE)
#  
#  #### 3. Extract links to individual pages ####
#  
#  # Find criteria to filter only direct links to news items
#  # Open an archive page at random.
#  # browseURL(url = sample(x = indexLinks, size = 1))
#  
#  links <- ExtractLinks(domain = "http://en.kremlin.ru/",
#                        partOfLink = "president/news/",
#                        partOfLinkToExclude = c("page",
#                                                "photos",
#                                                "videos",
#                                                "special"))
#  # length(links)/length(indexLinks)
#  # Explore links and check if more or less alright, if number realistic
#  # View(links)
#  # head(links)
#  
#  #### 4. Download pages ####
#  
#  DownloadContents(links = links, wait = 3)
#  DownloadContents(links = links, missingPages = FALSE, wait = 3)
#  
#  #### 5. Extract metadata and text ####
#  
#  # open an article at random to find out where metadata are located
#  # browseURL(url = sample(x = links, size = 1))
#  
#  id <- ExtractId()
#  
#  titles <- ExtractTitles(removeEverythingAfter = " •",
#                          id = id)
#  
#  
#  dates <- ExtractDates(container = "time",
#                        attribute = "datetime",
#                        containerInstance = 1,
#                        dateFormat = "Ymd",
#                        id = id)
#  
#  # check how many dates were not captured
#  sum(is.na(dates))
#  links[is.na(dates)]
#  
#  language <- "english"
#  
#  metadata <- ExportMetadata(id = id,
#                             dates = dates,
#                             titles = titles,
#                             language = language,
#                             links = links)
#  
#  ## Extract text
#  text <- ExtractText(container = "div",
#                      containerClass = "read__content",
#                      subElement = "p",
#                      removeEverythingAfter = "\nPublished in:",
#                      id = id)
#  
#  i <- sample(x = 1:length(text), 1)
#  titles[i]
#  dates[i]
#  text[i]
#  links[id][i]
#  #### 6. Save and export ####
#  
#  SaveWebsite(dataset = TRUE)

