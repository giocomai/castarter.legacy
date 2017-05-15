#' Extracts direct links to individual articles from index pages.
#'
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @param partOfLink Part of URL found only in links of individual articles to be downloaded. If more than one provided, it includes all links that contains either of the strings provided.
#' @param partOfLinkToExclude If an URL includes this string, it is excluded from the output. One or more strings may be provided.
#' @param indexLinks A character vector, defaults to NULL. If provided, indexLinks are removed from the extracted articlesLinks.
#' @param containerType Type of html container from where links are to be extracted, such as "ul", "div", and others. containerClass must also be provided.
#' @param attributeType Type of attribute to extract from links, when different from href.
#' @param minLength If a link is shorter than the number of characters given in minLength, it is excluded from the output.
#' @param maxLength If a link is longer than the number of characters given in maxLength, it is excluded from the output.
#' @param sortLinks Defaults to TRUE. If TRUE, links are sorted in aphabetical order.
#' @param linkTitle Defaults to TRUE. If TRUE, text of links is included as names of the vector.
#' @param appendString If provided, appends given string to the extracted articles. Typically used to create links for print or mobile versions of the extracted page.
#' @param removeString If provided, remove given string (or strings) from links.
#' @param progressBar Logical, defaults to TRUE. If FALSE, progress bar is not shown (useful for example when including scripts in rmarkdown)
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the project/website folder. They can be used to update the corpus.
#' @return A named character vector of links to articles. Name of the link may be the article title.
#' @export
#' @examples
#' articlesLinks <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/", html)
ExtractLinks <- function(domain, partOfLink, html, containerType = NULL, containerClass = NULL, divClass = NULL, attributeType = NULL, partOfLinkToExclude = NULL, minLength = NULL, maxLength = NULL, indexLinks = NULL, sortLinks = TRUE, linkTitle = TRUE, export = FALSE, appendString = NULL, removeString = NULL, progressBar = TRUE, exportParameters = TRUE, project = NULL, website = NULL) {
    if (gtools::invalid(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (gtools::invalid(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    numberOfIndexPages <- length(html)
    if (linkTitle==TRUE) {
        allLinks <- data.frame()
    } else {
        allLinks <- vector(mode = "character")
    }
    if (progressBar == TRUE) {
        pb <- txtProgressBar(min = 0, max = numberOfIndexPages, style = 3, title = "Extracting links")
    }
    for (i in 1:numberOfIndexPages) {
        if (html[i] != "") {
            indexPageHtmlParsed <- XML::htmlTreeParse(html[i], useInternalNodes = T, encoding = "UTF-8")
            if (gtools::invalid(divClass) == FALSE) {
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a/@href"))
                if (linkTitle==TRUE) {
                    titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a"), XML::xmlValue)
                }
            } else if (gtools::invalid(containerType) == FALSE) {
                if (gtools::invalid(containerClass) == TRUE) {
                  stop("containerClass must be defined if containerType is defined.")
                }
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//", containerType, "[@class='", containerClass, "']", "//a/@href"))
                if (linkTitle==TRUE) {
                    titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//", containerType, "[@class='", containerClass, "']", "//a"), XML::xmlValue)
                }
            } else if (gtools::invalid(attributeType) == FALSE) {
                XMLnodes <- XML::getNodeSet(indexPageHtmlParsed, "//a")
                links <- as.character(sapply(XMLnodes, XML::xmlGetAttr, attributeType))
                if (linkTitle==TRUE) {
                    titles <- sapply(XMLnodes, XML::xmlValue)
                }
            } else {
                XMLnodes <- XML::getNodeSet(indexPageHtmlParsed, "//a")
                links <- as.character(sapply(XMLnodes, XML::xmlGetAttr, "href"))
                if (linkTitle==TRUE) {
                    titles <- sapply(XMLnodes, XML::xmlValue)
                }
            }
            if (linkTitle==TRUE) {
                links <- cbind(links, titles)
                allLinks <- rbind(allLinks, links)
            } else {
                allLinks <- c(allLinks, links)
            }
        }
        if (progressBar == TRUE) {
            setTxtProgressBar(pb, i)
        }
    }
    if (progressBar == TRUE) {
        close(pb)
    }
    if (linkTitle==TRUE) {
        allLinks <- as.data.frame(allLinks, stringsAsFactors = FALSE)
        allLinks <- unique(allLinks)
        allLinksFiltered <- allLinks[0,]
    } else {
        allLinks <- unique(unlist(allLinks))
        allLinksFiltered <- vector(mode = "character")
    }
    for (i in 1:length(partOfLink)) {
        if (linkTitle==TRUE) {
        allLinksTemp <- allLinks[grepl(partOfLink[i], allLinks$links, fixed = TRUE), ]
        allLinksFiltered <- rbind(allLinksFiltered, allLinksTemp)
        } else {
            allLinksTemp <- allLinks[grepl(pattern = partOfLink[i], allLinks, fixed = TRUE)]
            allLinksFiltered <- c(allLinksFiltered, allLinksTemp)
        }
    }
    allLinks <- unique(allLinksFiltered)
    if (gtools::invalid(partOfLinkToExclude) == FALSE) {
        if (linkTitle==TRUE) {
            for (i in 1:length(partOfLinkToExclude)) {
                allLinks <- allLinks[!grepl(partOfLinkToExclude[i], allLinks$links, fixed = TRUE), ]
            }
        }  else {
            for (i in 1:length(partOfLinkToExclude)) {
                allLinks <- allLinks[!grepl(partOfLinkToExclude[i], allLinks, fixed = TRUE)]
            }
        }
    }
    if (gtools::invalid(minLength)==FALSE) {
        if (linkTitle==TRUE) {
            allLinks <- allLinks[nchar(as.character(allLinks$links)) > minLength-nchar(domain),  ]
        } else {
            allLinks <- allLinks[nchar(as.character(allLinks)) > minLength-nchar(domain)]
        }
    }
    if (gtools::invalid(maxLength)==FALSE) {
        if (linkTitle==TRUE) {
            allLinks <- allLinks[nchar(as.character(allLinks$links)) < maxLength-nchar(domain),  ]
        } else {
            allLinks <- allLinks[nchar(as.character(allLinks)) < maxLength-nchar(domain)]
        }
    }
    if (linkTitle==TRUE) {
        allLinks$links <- as.character(allLinks$links)
    }
    if (gtools::invalid(removeString)==FALSE) {
        if (linkTitle==TRUE) {
            for (i in seq_along(removeString)) {
                allLinks$links <- gsub(pattern = removeString[i], replacement = "", x = allLinks$links, fixed = TRUE)
            }
        } else {
            for (i in seq_along(removeString)) {
                allLinks <- gsub(pattern = removeString[i], replacement = "", x = allLinks, fixed = TRUE)
            }
        }
    }
    if (linkTitle==TRUE) {
        allLinks$links <- paste0(domain, allLinks$links)
        allLinks$links <- gsub("//", "/", allLinks$links, fixed = TRUE)
        allLinks$links <- gsub("http:/", "http://", allLinks$links, fixed = TRUE)
        allLinks$links <- gsub("https:/", "https://", allLinks$links, fixed = TRUE)
    } else {
        allLinks <- paste0(domain, allLinks)
        allLinks <- gsub("//", "/", allLinks, fixed = TRUE)
        allLinks <- gsub("http:/", "http://", allLinks, fixed = TRUE)
        allLinks <- gsub("https:/", "https://", allLinks, fixed = TRUE)
    }
    if (is.null(indexLinks) == FALSE) {
        if (linkTitle==TRUE) {
            allLinks <- allLinks[!is.element(allLinks$links, indexLinks), ]
        } else {
            allLinks <- allLinks[!is.element(allLinks, indexLinks)]
        }
    }
    if (linkTitle==TRUE) {
        allLinks <- allLinks[!duplicated(allLinks[, "links"], fromLast = TRUE), ]
    }
    if (sortLinks == TRUE) {
        if (linkTitle==TRUE) {
            allLinks <- allLinks[gtools::mixedorder(allLinks$links), ]
        } else {
            allLinks <- allLinks[gtools::mixedorder(allLinks)]
        }
    }
    if (linkTitle==TRUE) {
        links <- as.character(allLinks$links)
        links <- paste0(links, appendString)
        titles <- as.character(allLinks$titles)
        links <- setNames(links, titles)
    } else {
        links <- paste0(allLinks, appendString)
    }
    if (export == TRUE) {
        writeLines(links, file.path(project, website, "Logs", paste(Sys.Date(), website, "articlesLinks.txt", sep = " - ")))
    }
    if (exportParameters == TRUE) {
        args <- c("domain", "partOfLink", "html", "containerTypeExtractLinks", "containerClassExtractLinks", "divClassExtractLinks", "attributeTypeExtractLinks", "partOfLinkToExclude", "minLength", "maxLength", "indexLinks","sortLinks", "export", "appendStringExtractLinks", "removeStringExtractLinks", "exportParameters", "project", "website")
        param <- list(domain, partOfLink, "html", containerType, containerClass, divClass, attributeType, paste(partOfLinkToExclude, collapse = "§§§"), minLength, maxLength, "indexLinks", sortLinks, export, appendString, paste(removeString, collapse = "§§§"), exportParameters, project, website)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- as.data.frame(cbind(args, param), stringsAsFactors = FALSE)
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
    links
}
