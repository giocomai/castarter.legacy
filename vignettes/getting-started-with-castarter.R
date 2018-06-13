## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  citation("castarter")

## ----eval=FALSE----------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("giocomai/castarter")
#  library("castarter")

## ------------------------------------------------------------------------
SetCastarter(project = "Presidents", website = "Kremlin_en")

## ------------------------------------------------------------------------
CreateFolders()

## ------------------------------------------------------------------------
indexLinks <- CreateLinks(linkFirstChunk = "http://en.kremlin.ru/events/president/news/page/",
                          startPage = 1,
                          endPage = 1060)
head(indexLinks)

## ------------------------------------------------------------------------
DownloadContents(links = indexLinks, type = "index")

## ------------------------------------------------------------------------
DownloadContents(links = indexLinks, type = "index", missingPages = FALSE)

