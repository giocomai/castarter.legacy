#' Extracts direct links to individual articles from index pages. 
#' 
#' Extracts direct links to individual articles from index pages according to a selcted pattern.
#'  
#' @param domain Web domain of the website. Will be added at the beginning of each link found.If links in the page already include the full web address this should be ignored. Defaults to "".
#' @param partOfLink Part of URL found only in links of individual articles to be downloaded.
#' @param exportParameters Defaults to FALSE. If TRUE, function parameters are exported in the nameOfProject/nameOfWebsite folder. They can be used to update the corpus. 
#' @return A named character vector of links to articles. Name of the link may be the article title. 
#' @export
#' @examples
#' articlesLinks <- ExtractLinks(domain = "http://www.example.com/", partOfLink = "news/", indexHtml)
ExtractLinks <- function(domain, partOfLink, indexHtml, containerType = "", containerClass = "", divClass = "", partOfLinkToExclude = "", 
    sort = TRUE, export = FALSE, exportParameters = FALSE, nameOfProject = NULL, nameOfWebsite = NULL) {
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
    if (exportParameters == TRUE) {
        args <- c("domain", "partOfLink", "indexHtml", "containerType", "containerClass", "divClassExtractLinks", "partOfLinkToExclude", "sort", "export", "exportParameters", "nameOfProject", "nameOfWebsite")
        param <- list(domain, partOfLink, "indexHtml", containerType, containerClass, divClass, partOfLinkToExclude, sort, export, exportParameters, nameOfProject, nameOfWebsite)
        for (i in 1:length(param)) {
            if (is.null(param[[i]])==TRUE) {
                param[[i]] <- "NULL"
            }
        }
        param <- unlist(param)
        updateParametersTemp <- data.frame(args, param, stringsAsFactors = FALSE)
        if (file.exists(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-"))) == TRUE) {
            updateParameters <- utils::read.table(base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")), stringsAsFactors = FALSE)
            for (i in 1:length(updateParametersTemp$args)) {
                updateParameters$param[updateParameters$args == updateParametersTemp$args[i]] <- updateParametersTemp$param[i]
            }
        } else {
            updateParameters <- updateParametersTemp 
        }
        write.table(updateParameters, file = base::file.path(nameOfProject, nameOfWebsite, paste(nameOfWebsite, "updateParameters.csv", sep = "-")))
    }
    links
} 
