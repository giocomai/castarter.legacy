#' Prepares combinations of words to be changed
#' 
#' Prepares combinations of words to be changed.
#'  
#' @param originalCombination, toBeUsed A character vector.of length 1.
#' @param importExport Defaults to FALSE. If TRUE, saves stopwords in a txt file in the project folder, and imports changes made in the txt file.
#' @return .A data.frame of word combinations. 
#' @export
#' @examples
#' originalCombination <- "European Union"
#' toBeUsed <- "europeanunion"
#' wordCombinations <- AddWordCombinations(originalCombination, toBeUsed, nameOfProject = "")
AddWordCombinations <- function(wordCombinations = NULL, originalCombination = NULL, toBeUsed = NULL, nameOfProject = "", dfedit = FALSE, importExport = FALSE) {
    if (importExport == TRUE & file.exists(file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv"))) == TRUE) {
        wordCombinations <- read.csv(file = file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv")), header = TRUE, quote = "", stringsAsFactors = FALSE)
    } else {
        if (is.null(wordCombinations) == TRUE) {
            wordCombinations <- data.frame(originalCombination = character(0), toBeUsed = character(0))
        }
    }
    if (is.null(originalCombination) == FALSE) {
        wordCombinations <- rbind(wordCombinations, c(originalCombination, toBeUsed))
    }
    wordCombinations <- unique(wordCombinations)
    if (dfedit == TRUE) {
        wordCombinations <- dfedit(wordCombinations)
    }
    names(wordCombinations) <- c("originalCombination", "toBeUsed")
    wordCombinations <- unique(wordCombinations)
    wordCombinations <- data.frame(lapply(wordCombinations, as.character), stringsAsFactors = FALSE)
    if (importExport == TRUE) {
        write.csv(wordCombinations, file = file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv")), row.names = FALSE, quote = FALSE)
    }
    wordCombinations
}

#' Combine words in a corpus
#' 
#' Combine words in a corpus according to word combinations provided by the user..
#'  
#' @param corpus A corpus created with the 'tm' package.
#' @return A corpus with words combined.
#' @export
#' @examples
#' corpus <- CombineWords(corpus, wordCombinations)
CombineWords <- function(corpus, wordCombinations) {
    for (i in 1:length(wordCombinations[, 2])) {
        wordCombinations[i, 1] <- paste0("\\<", wordCombinations[i, 1], "\\>")
    }
    Combine <- tm::content_transformer(function(x, originalCombination, toBeUsed) gsub(originalCombination, toBeUsed, x))
    for (i in 1:length(wordCombinations[, 1])) {
        corpus <- tm::tm_map(corpus, Combine, wordCombinations$originalCombination[i], wordCombinations$toBeUsed[i])
    }
    corpus
}

#' Adds stopwords to the default list 
#' 
#' Adds stopwords to the default list.
#'  
#' @param newStopwords A character vector of words.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param importExport Defaults to FALSE. If TRUE, saves stopwords in a txt file in the project folder, and imports changes made in the txt file.
#' @param includeDefault Logical, defaults to TRUE. Includes default list of stopwords included in the 'tm' package. 
#' @param language Passed to the stopwords function of the TM package to provide a default list of stopwords. 
#' @param importExport If TRUE exports the stopword list to a txt file in the project folder. The txt can be edited and re-imported re-running the function. 
#' @return A character vector.
#' @export
#' @examples
#' newStopwords <- c("will", "also", "can")
#' stopwords <- AddStopwords(newStopwords, nameOfProject, includeDefault = TRUE, language = "en")

AddStopwords <- function(newStopwords = NULL, nameOfProject = NULL, includeDefault = TRUE, language = "en", importExport = FALSE) {
    if (is.null(nameOfProject)==FALSE & importExport == TRUE) {
        if (file.exists(file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))) {
            stopwords <- as.vector(readLines(con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt"))))
        } else {
            writeLines(newStopwords, con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))
        }
    }
    if (is.null(newStopwords) == FALSE) {
        if (exists("stopwords") == TRUE) {
            if (class(stopwords)=="character") {
                stopwords <- c(stopwords, newStopwords)
            }
        } else {
            stopwords <- newStopwords
        }
    }
    if (includeDefault == TRUE) {
        if (exists("stopwords") == TRUE) {
            if (class(stopwords)=="character") {
                stopwords <- c(stopwords, tm::stopwords(language))
            } else {
                stopwords <- tm::stopwords(language)
            } 
        }
    }
    stopwords <- unique(as.character(stopwords[stopwords != ""]))
    if (is.null(nameOfProject)==FALSE & importExport == TRUE) {
        writeLines(stopwords, con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))
    }
    stopwords
}

#' Remove stopwords from corpus
#' 
#' Remove stopwords from corpus.
#'  
#' @param corpus A corpus created with the 'tm' package.
#' @return A corpus without stopwords.
#' @export
#' @examples
#' corpus <- RemoveStopwords(corpus, stopwords)
RemoveStopwords <- function(corpus, stopwords) {
    corpus <- tm::tm_map(corpus, removeWords, stopwords)
    corpus
}

#' Calls various 'tm' functions to clean up the corpus. 
#' 
#' It applies to a corpus common operations to prepare it for further analysis using functions of the 'tm' package.
#'  
#' @param corpus A corpus as created by the 'tm' package including metadata.
#' @return A corpus as created by the 'tm' package including metadata, after all enabled transformations are applied.
#' @export
#' @examples
#' corpus <- CleanCorpus(corpus)
CleanCorpus <- function(corpus, stripWhitespace = TRUE, toLowerCase = TRUE, removeNumbers = TRUE, removePunctuation = TRUE, removeControlCharacters = TRUE, 
    removeCharacter = "") {
    if (toLowerCase == TRUE) {
        corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
    }
    if (removeNumbers == TRUE) {
        corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) gsub("[[:digit:]]", " ", x)))
    }
    if (removePunctuation == TRUE) {
        corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) gsub("[[:punct:]]", " ", x)))
    }
    if (removeControlCharacters == TRUE) {
        corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) gsub("[[:cntrl:]]", " ", x)))
    }
    if (stripWhitespace == TRUE) {
        corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) gsub("[[:space:]]+", " ", x)))
    }
    if (removeCharacter[1] != "") {
        for (i in 1:length(removeCharacter)) {
            corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) gsub(removeCharacter[i], "", x)))
        }
    }
    corpus
}

#' Exports the stemming dictionary
#' 
#' Allows to export a stemming dictionary in csv format.
#'  
#' @param corpusDtm A document-term matrix.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param stopwords A character vector of stopwords. 
#' @return .A data.frame with words before and after stemming.
#' @export
#' @examples
#' stemmingDictionary <- ExportStemmingDictionary(corpus, nameOfProject)
ExportStemmingDictionary <- function(corpusDtm, nameOfProject, stopwords = "", language = "english") {
    stemmingDictionary <- data.frame(row.names = colnames(corpusDtm), occurrences = slam::col_sums(corpusDtm), stemmedTerm = SnowballC::wordStem(colnames(corpusDtm), language), 
                                     stopword = ifelse(colnames(corpusDtm) %in% stopwords, "stopword", ""), stringsAsFactors = FALSE)
    stemmingDictionary[Terms(dtm) %in% stopwords, "stemmedTerm"] <- ""
    #if (!file.exists(file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")))) {
        write.csv(stemmingDictionary, file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), row.names = TRUE)
    #}
#     if (stemmingEditMode != "none") {
#         stemmingDictionary0 <- stemmingDictionary
#         stemmingDictionary1 <- read.csv(file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), header = TRUE, row.names = 1)
#         stemmingDictionaryMerged <- merge(stemmingDictionary0, stemmingDictionary1, by = "row.names", all.x = TRUE)
#         stemmingDictionaryMissing <- is.na(stemmingDictionaryMerged$stemmedTerm.y)
#         for (i in 1:length(stemmingDictionaryMissing)) {
#             if (stemmingDictionaryMissing[i] == FALSE) 
#                 stemmingDictionary$stemmedTerm[i] <- as.character(stemmingDictionaryMerged$stemmedTerm.y[i]) else stemmingDictionary$stemmedTerm[i] <- as.character(stemmingDictionaryMerged$stemmedTerm.x[i])
#         }
#         if (stemmingEditMode == "dfedit") {
#             stemmingDictionary <- dfedit(stemmingDictionary)
#         }
#         write.csv(stemmingDictionary, file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), row.names = TRUE)
#     }
    stemmingDictionary
}

StemCorpusDtm <- function(corpusDtm, stemmingDictionary) {
    dtm <- tm::DocumentTermMatrix(corpus)
    dtm <- rollup(dtm, 2, stemmingDictionary[["stemmedTerm"]])
    dtm <- dtm[, Terms(dtm) != ""]
    dtm
} 

#' Creates a Document Term Matrix (DTM).
#'  
#' @param corpus A corpus as created by the 'tm' package including metadata.
#' @param removeSparseTerms A value between 0 and 1, to be passed to the removeSparseTerms function of the 'tm' function.
#' @return A Document Term Matrix (DTM).
#' @keywords tm
#' @export
#' @examples
#' dtm <- CreateDtm(corpus)
CreateDtm <- function(corpus, removeSparseTerms = NULL){
    corpusDtm <- tm::DocumentTermMatrix(corpus)
    if (is.null(removeSparseTerms) == FALSE) {
        corpusDtm <- tm::removeSparseTerms(corpusDtm, removeSparseTerms)
    }
}