ExtractArticlesLinks <- function(domain, partOfDirectLink, indexPagesHtml, containerType = "", containerClass = "", divClass = "", partOfDirectLinkToExclude = "", 
    sort = TRUE, export = FALSE) {
    numberOfIndexPages <- length(indexPagesHtml)
    allLinks <- data.frame()
    for (i in 1:numberOfIndexPages) {
        if (indexPagesHtml[i] != "") {
            indexPageHtmlParsed <- htmlTreeParse(indexPagesHtml[i], useInternalNodes = T, encoding = "UTF-8")
            if (divClass != "") {
                links <- xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a/@href"))
                titles <- xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a"), xmlValue)
            } else if (containerType == "ul") {
                if (containerClass == "") {
                  stop("containerClass must be defined for containerType = 'ul'")
                }
                links <- xpathSApply(indexPageHtmlParsed, paste0("//ul[@class='", containerClass, "']", "//a/@href"))
                titles <- xpathSApply(indexPageHtmlParsed, paste0("//ul[@class='", containerClass, "']", "//a"), xmlValue)
            } else {
                links <- xpathSApply(indexPageHtmlParsed, "//a/@href")
                titles <- xpathSApply(indexPageHtmlParsed, "//a", xmlValue)
            }
            links <- cbind(links, titles)
            allLinks <- rbind(links, allLinks)
        }
    }
    allLinks <- as.data.frame(allLinks, stringsAsFactors = FALSE)
    allLinks <- unique(allLinks)
    allLinks <- allLinks[grepl(partOfDirectLink, allLinks$links, fixed = TRUE), ]
    if (partOfDirectLinkToExclude != "") {
        allLinks <- allLinks[!grepl(partOfDirectLinkToExclude, allLinks$links, fixed = TRUE), ]
    }
    allLinks <- allLinks[mixedorder(nchar(as.character(allLinks$titles))), ]
    allLinks$links <- as.character(allLinks$links)
    allLinks$links <- paste0(domain, allLinks$links)
    allLinks$links <- gsub("//", "/", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("http:/", "http://", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("https:/", "https://", allLinks$links, fixed = TRUE)
    allLinks <- allLinks[!duplicated(allLinks[, "links"], fromLast = TRUE), ]
    if (sort == TRUE) {
        allLinks <- allLinks[mixedorder(allLinks$links), ]
    }
    links <- as.character(allLinks$links)
    titles <- as.character(allLinks$titles)
    links <- setNames(links, titles)
    if (export == TRUE) {
        writeLines(links, file.path(nameOfProject, nameOfWebsite, paste0(nameOfWebsite, "articlesLinks.txt")))
    }
    links
} 
