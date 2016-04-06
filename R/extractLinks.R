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
#' @param appendString If provided, appends given string to the extracted articles. Typically used to create links for print or mobile versions of the extracted page.
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the nameOfProject/nameOfWebsite folder. They can be used to update the corpus. 
#' @return A named character vector of links to articles. Name of the link may be the article title. 
#' @export
#' @examples
#' articlesLinks <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/", indexHtml)
ExtractLinks <- function(domain, partOfLink, indexHtml, containerType = NULL, containerClass = NULL, divClass = NULL, attributeType = NULL, partOfLinkToExclude = NULL, minLength = NULL, maxLength = NULL, indexLinks = NULL,
    sortLinks = TRUE, export = FALSE, appendString = NULL, exportParameters = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
    numberOfIndexPages <- length(indexHtml)
    allLinks <- data.frame()
    for (i in 1:numberOfIndexPages) {
        if (indexHtml[i] != "") {
            indexPageHtmlParsed <- XML::htmlTreeParse(indexHtml[i], useInternalNodes = T, encoding = "UTF-8")
            if (is.null(divClass) == FALSE & is.na(divClass) == FALSE) {
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a/@href"))
                titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a"), XML::xmlValue)
            } else if (is.null(containerType) == FALSE & is.na(containerType) == FALSE) {
                if (is.null(containerClass) == TRUE | is.na(containerClass) == TRUE) {
                  stop("containerClass must be defined if containerType is defined.")
                }
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//", containerType, "[@class='", containerClass, "']", "//a/@href"))
                titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//", containerType, "[@class='", containerClass, "']", "//a"), XML::xmlValue)
            } else if (is.null(attributeType) == FALSE) {
                XMLnodes <- XML::getNodeSet(indexPageHtmlParsed, "//a")
                links <- as.character(sapply(XMLnodes, XML::xmlGetAttr, attributeType))
                titles <- sapply(XMLnodes, XML::xmlValue)
            } else {
                XMLnodes <- XML::getNodeSet(indexPageHtmlParsed, "//a")
                links <- as.character(sapply(XMLnodes, XML::xmlGetAttr, "href"))
                titles <- sapply(XMLnodes, XML::xmlValue)
            }
            links <- cbind(links, titles)
            allLinks <- rbind(links, allLinks)
        }
    }
    allLinks <- as.data.frame(allLinks, stringsAsFactors = FALSE)
    allLinks <- unique(allLinks)
    allLinksFiltered <- allLinks[0,]
    for (i in 1:length(partOfLink)) {
        allLinksTemp <- allLinks[grepl(partOfLink[i], allLinks$links, fixed = TRUE), ]
        allLinksFiltered <- rbind(allLinksFiltered, allLinksTemp)
    }
    allLinks <- unique(allLinksFiltered)
    if (is.null(partOfLinkToExclude) == FALSE & is.na(partOfLinkToExclude) == FALSE) {
        for (i in 1:length(partOfLinkToExclude)) {
            allLinks <- allLinks[!grepl(partOfLinkToExclude[i], allLinks$links, fixed = TRUE), ]
        }
    }
    if (is.null(minLength)==FALSE) {
        allLinks <- allLinks[nchar(as.character(allLinks$links)) > minLength-nchar(domain),  ]
    }
    if (is.null(maxLength)==FALSE) {
        allLinks <- allLinks[nchar(as.character(allLinks$links)) < maxLength-nchar(domain),  ]
    }
    allLinks <- allLinks[gtools::mixedorder(nchar(as.character(allLinks$titles))), ]
    allLinks$links <- as.character(allLinks$links)
    allLinks$links <- paste0(domain, allLinks$links)
    allLinks$links <- gsub("//", "/", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("http:/", "http://", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("https:/", "https://", allLinks$links, fixed = TRUE)
    if (is.null(indexLinks) == FALSE) {
        allLinks <- allLinks[!is.element(allLinks$links, indexLinks), ]
    }
    allLinks <- allLinks[!duplicated(allLinks[, "links"], fromLast = TRUE), ]
    if (sortLinks == TRUE) {
        allLinks <- allLinks[gtools::mixedorder(allLinks$links), ]
    }
    links <- as.character(allLinks$links)
    links <- paste0(links, appendString)
    titles <- as.character(allLinks$titles)
    links <- setNames(links, titles)
    if (export == TRUE) {
        writeLines(links, file.path(nameOfProject, nameOfWebsite, paste0(nameOfWebsite, "articlesLinks.txt")))
    }
    if (exportParameters == TRUE) {
        args <- c("domain", "partOfLink", "indexHtml", "containerTypeExtractLinks", "containerClassExtractLinks", "divClassExtractLinks", "attributeTypeExtractLinks", "partOfLinkToExclude", "minLength", "maxLength", "indexLinks","sortLinks", "export", "appendString", "exportParameters", "nameOfProject", "nameOfWebsite")
        param <- list(domain, partOfLink, "indexHtml", containerType, containerClass, divClass, attributeType, paste(partOfLinkToExclude, collapse = "§§§"), minLength, maxLength, "indexLinks", sortLinks, export, appendString, exportParameters, nameOfProject, nameOfWebsite)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- as.data.frame(cbind(args, param), stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
                if (is.element(updateParametersTemp$args[i], updateParameters$args) == FALSE) {
                    updateParameters <- rbind(updateParameters, updateParametersTemp[i,] )
                }
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    links
} 
