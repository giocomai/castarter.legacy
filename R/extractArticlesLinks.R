#' Extracts direct links to individual articles from index pages. 
#' 
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'  
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @return A named character vector of links to articles. Name of the link may be the article title. 
#' @export
#' @examples
#' articlesLinks <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/", indexHtml)
ExtractLinks <- function(domain, partOfLink, indexHtml, containerType = "", containerClass = "", divClass = "", partOfLinkToExclude = "", 
    sort = TRUE, export = FALSE) {
    numberOfIndexPages <- length(indexHtml)
    allLinks <- data.frame()
    for (i in 1:numberOfIndexPages) {
        if (indexHtml[i] != "") {
            indexPageHtmlParsed <- XML::htmlTreeParse(indexHtml[i], useInternalNodes = T, encoding = "UTF-8")
            if (divClass != "") {
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a/@href"))
                titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//div[@class='", divClass, "']", "//a"), XML::xmlValue)
            } else if (containerType == "ul") {
                if (containerClass == "") {
                  stop("containerClass must be defined for containerType = 'ul'")
                }
                links <- XML::xpathSApply(indexPageHtmlParsed, paste0("//ul[@class='", containerClass, "']", "//a/@href"))
                titles <- XML::xpathSApply(indexPageHtmlParsed, paste0("//ul[@class='", containerClass, "']", "//a"), XML::xmlValue)
            } else {
                links <- XML::xpathSApply(indexPageHtmlParsed, "//a/@href")
                titles <- XML::xpathSApply(indexPageHtmlParsed, "//a", XML::xmlValue)
            }
            links <- cbind(links, titles)
            allLinks <- rbind(links, allLinks)
        }
    }
    allLinks <- as.data.frame(allLinks, stringsAsFactors = FALSE)
    allLinks <- unique(allLinks)
    allLinks <- allLinks[grepl(partOfLink, allLinks$links, fixed = TRUE), ]
    if (partOfLinkToExclude != "") {
        allLinks <- allLinks[!grepl(partOfLinkToExclude, allLinks$links, fixed = TRUE), ]
    }
    allLinks <- allLinks[gtools::mixedorder(nchar(as.character(allLinks$titles))), ]
    allLinks$links <- as.character(allLinks$links)
    allLinks$links <- paste0(domain, allLinks$links)
    allLinks$links <- gsub("//", "/", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("http:/", "http://", allLinks$links, fixed = TRUE)
    allLinks$links <- gsub("https:/", "https://", allLinks$links, fixed = TRUE)
    allLinks <- allLinks[!duplicated(allLinks[, "links"], fromLast = TRUE), ]
    if (sort == TRUE) {
        allLinks <- allLinks[gtools::mixedorder(allLinks$links), ]
    }
    links <- as.character(allLinks$links)
    titles <- as.character(allLinks$titles)
    links <- setNames(links, titles)
    if (export == TRUE) {
        writeLines(links, file.path(nameOfProject, nameOfWebsite, paste0(nameOfWebsite, "articlesLinks.txt")))
    }
    links
} 
