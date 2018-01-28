#### 1. Preliminary steps ####

## Install castarter (devtools required for installing from github)
# install.packages("devtools")
devtools::install_github(repo = "giocomai/castarter")

## Load castarter
library("castarter")

## Set project and website name.
# They will remain in memory in the current R session,
# so need need to include them in each function call.
SetCastarter(project = "Presidents", website = "Kremlin_en")

## Creates the folder structure where files will be saved within the
## current working directory
CreateFolders()

#### 2. Download index/archive pages ####

# Check out the archive page, e.g. http://en.kremlin.ru/events/president/news/
# See what is the last archive page, if they are defined by consecutive numbers

indexLinks <- CreateLinks(linkFirstChunk = "http://en.kremlin.ru/events/president/news/page/",
                          startPage = 1,
                          endPage = 1032)

## Downloads all html files of the archive pages
# This can be interrupted: by default, if the command is issued again it will download only missing pages (i.e. pages that have not been downloaded previously)
DownloadContents(links = indexLinks, type = "index", createScript = TRUE, wait = 11)
DownloadContents(links = indexLinks, type = "index")
# Downloads again files with oddly small size (usually, errors), if any.
DownloadContents(links = indexLinks, type = "index", missingPages = FALSE)

#### 3. Extract links to individual pages ####

# Find criteria to filter only direct links to news items
# Open an archive page at random.
# browseURL(url = sample(x = indexLinks, size = 1))

links <- ExtractLinks(domain = "http://en.kremlin.ru/",
                      partOfLink = "president/news",
                      partOfLinkToExclude = c("page", "photos", "videos", "special"),
                      minLength = nchar("http://en.kremlin.ru/events/president/news/"))

links2 <- stringr::str_replace(string = links, pattern = stringr::fixed("news/"), replacement = "news/copy/")

# Explore links and check if more or less alright, if number realistic
# length(links)/length(indexLinks)
# View(links)
# head(links)

#### 4. Download pages ####

DownloadContents(links = links, wait = 3)
DownloadContents(links = links, missingPages = FALSE, wait = 3)

#### 5. Extract metadata and text ####

# open an article at random to find out where metadata are located
# browseURL(url = sample(x = indexLinks, size = 1))

id <- ExtractId()

titles <- ExtractTitles(removeEverythingAfter = " •")


dates <- ExtractDates(container = "time",
                      containerClass = "read__published",
                      attribute = "datetime",
                      dateFormat = "Ymd",
                      keepAllString = TRUE)

# check how many dates were not captured
sum(is.na(dates))
links[is.na(dates)]

language <- "english"

metadata <- ExportMetadata(dates = dates,
                           id = id,
                           titles = titles,
                           language = language,
                           links = links)

## Extract text
text <- ExtractText(metadata = metadata, container = "div",
                    containerClass = "read__content",
                    removeEverythingAfter = "Publication status Published",
                    removeString = "    Events ")

i <- sample(x = 1:length(text), 1)
titles[i]
dates[i]
text[i]

#### 6. Save and export ####

SaveWebsite(dataset = TRUE)

### Examples of further exports

# dataset <- LoadDatasets() %>% filter(date<as.Date("2018-01-01")) %>% arrange(date)
#
# saveRDS(object = dataset, file = "2018-01-01-Kremlin_en-dataset.rds")
# saveRDS(object = dataset %>% tidytext::unnest_tokens(output = sentence,
#                                                      input = text, token = "sentences", to_lower = FALSE), file = "2018-01-01-Kremlin_en-dataset_bySentence.rds")
#
# write_csv(x = dataset, path = "2018-01-01-Kremlin_en-dataset.csv")
# write_csv(x = dataset %>% select(-text), path = "2018-01-01-Kremlin_en-metadata.csv")
#
# writeLines(paste(paste("Date:", dataset$date), paste("Title:", dataset$title), paste("Link:", dataset$link), paste("ID:", dataset$id), dataset$text, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"),
#            file.path("2018-01-01-Kremlin_en-dataset.txt"))

