AddWordCombinations <- function(originalCombination = "", toBeUsed = "", nameOfProject = "", dfedit = FALSE) {
    if (file.exists(file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv"))) == TRUE) {
        wordCombinations <- read.csv(file = file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv")), header = TRUE, quote = "", 
            stringsAsFactors = FALSE)
    } else {
        wordCombinations <- data.frame(originalCombination = character(0), toBeUsed = character(0))
    }
    if (originalCombination != "") {
        wordCombinations <- rbind(wordCombinations, c(originalCombination, toBeUsed))
    }
    wordCombinations <- unique(wordCombinations)
    if (dfedit == TRUE) {
        wordCombinations <- dfedit(wordCombinations)
    }
    names(wordCombinations) <- c("originalCombination", "toBeUsed")
    wordCombinations <- unique(wordCombinations)
    wordCombinations <- data.frame(lapply(wordCombinations, as.character), stringsAsFactors = FALSE)
    write.csv(wordCombinations, file = file.path(nameOfProject, paste(nameOfProject, "wordCombinations.csv")), row.names = FALSE, quote = FALSE)
    wordCombinations
}

CombineWords <- function(corpus, wordCombinations) {
    for (i in 1:length(wordCombinations[, 2])) {
        wordCombinations[i, 1] <- paste0("\\<", wordCombinations[i, 1], "\\>")
    }
    # ## Export word combinations allowing editing in an external editor if (!file.exists(file.path(nameOfProject, paste(nameOfProject,
    # 'wordCombinations.csv')))) { write.csv(wordCombinations, file = file.path(nameOfProject, paste(nameOfProject, 'wordCombinations.csv')),
    # row.names = FALSE) } ## Import back word combinations wordCombinations <- read.csv(file = file.path(nameOfProject, paste(nameOfProject,
    # 'wordCombinations.csv')), header = TRUE, stringsAsFactors = FALSE)
    Combine <- content_transformer(function(x, originalCombination, toBeUsed) gsub(originalCombination, toBeUsed, x))
    for (i in 1:length(wordCombinations[, 1])) {
        corpus <- tm_map(corpus, Combine, wordCombinations$originalCombination[i], wordCombinations$toBeUsed[i])
    }
    corpus
}

# AddWordsToBeUnited <- function(wordsToBeCombined, nameOfProject) { if (file.exists(file.path(nameOfProject, paste(nameOfProject,
# 'wordsToBeUnited.txt')))) { wordCombinations <- as.vector(readLines(con = file.path(nameOfProject, paste(nameOfProject,
# 'wordsToBeUnited.txt')))) } else { writeLines(wordsToBeCombined, con = file.path(nameOfProject, paste(nameOfProject,
# 'wordsToBeUnited.txt'))) } if (exists('wordsToBeUnited') == FALSE) { wordCombinations <- wordsToBeCombined } else { wordCombinations <-
# c(wordCombinations, wordsToBeCombined) } wordCombinations <- unique(as.character(wordCombinations[wordCombinations != '']))
# writeLines(wordCombinations, con = file.path(nameOfProject, paste(nameOfProject, 'wordsToBeUnited.txt'))) wordCombinations } UniteWords <-
# function(corpus, wordCombinations) { # Export metadata author <- vector() datetimestamp <- vector() heading <- vector() id <- vector()
# language <- vector() origin <- vector() for (i in 1:length(corpus)) { author[i] <- meta(corpus[[i]], tag = 'author') datetimestamp[i] <-
# meta(corpus[[i]], tag = 'datetimestamp') heading[i] <- meta(corpus[[i]], tag = 'heading') id[i] <- meta(corpus[[i]], tag = 'id')
# language[i] <- meta(corpus[[i]], tag = 'language') origin[i] <- meta(corpus[[i]], tag = 'origin') } wordCombined <- gsub('\\s+','',
# wordCombinations) for (i in 1:length(wordCombinations)) { word1 <- wordCombinations[i] word2 <- wordCombined[i] for (i in 1:length(corpus))
# { corpus[[i]] <- gsub(word1, word2, corpus[[i]], fixed = TRUE) } } corpus <- VCorpus(VectorSource(corpus)) for (i in 1:length(corpus)) {
# meta(corpus[[i]], tag = 'author') <- author[i] meta(corpus[[i]], tag = 'datetimestamp') <- datetimestamp[i] meta(corpus[[i]], tag =
# 'heading') <- heading[i] meta(corpus[[i]], tag = 'id') <- id[i] meta(corpus[[i]], tag = 'language') <- language[i] meta(corpus[[i]], tag =
# 'origin') <- origin[i] } corpus }

AddStopwords <- function(newStopwords, nameOfProject, includeDefaultList = FALSE, language = "en") {
    if (file.exists(file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))) {
        stopwords <- as.vector(readLines(con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt"))))
    } else {
        writeLines(newStopwords, con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))
    }
    if (exists("stopwords") == FALSE) {
        stopwords <- newStopwords
    } else {
        stopwords <- c(stopwords, newStopwords)
    }
    if (includeDefaultList == TRUE) {
        stopwords <- c(stopwords, stopwords(language))
    }
    stopwords <- unique(as.character(stopwords[stopwords != ""]))
    writeLines(stopwords, con = file.path(nameOfProject, paste(nameOfProject, "stopwords.txt")))
    stopwords
}

RemoveStopwords <- function(corpus, stopwords) {
    corpus <- tm_map(corpus, removeWords, stopwords)
    corpus
}

CleanCorpus <- function(corpus, stripWhitespace = TRUE, toLowerCase = TRUE, removeNumbers = TRUE, removePunctuation = TRUE, removeControlCharacters = TRUE, 
    removeCharacter = "") {
    if (toLowerCase == TRUE) {
        corpus <- tm_map(corpus, content_transformer(tolower))
    }
    if (removeNumbers == TRUE) {
        corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:digit:]]", " ", x)))
    }
    if (removePunctuation == TRUE) {
        corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:punct:]]", " ", x)))
    }
    if (removeControlCharacters == TRUE) {
        corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:cntrl:]]", " ", x)))
    }
    if (stripWhitespace == TRUE) {
        corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:space:]]+", " ", x)))
    }
    if (removeCharacter[1] != "") {
        for (i in 1:length(removeCharacter)) {
            corpus <- tm_map(corpus, content_transformer(function(x) gsub(removeCharacter[i], "", x)))
        }
    }
    corpus
}

EditStemmingDictionary <- function(corpus, nameOfProject, stemmingEditMode = "fromCsv") {
    dtm <- DocumentTermMatrix(corpus)
    stemmingDictionary <- data.frame(row.names = colnames(dtm), occurrences = col_sums(dtm), stemmedTerm = wordStem(colnames(dtm), language), 
        stopword = ifelse(colnames(dtm) %in% stopwords, "stopword", ""), stringsAsFactors = FALSE)
    stemmingDictionary[Terms(dtm) %in% stopwords, "stemmedTerm"] <- ""
    if (!file.exists(file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")))) {
        write.csv(stemmingDictionary, file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), row.names = TRUE)
    }
    if (stemmingEditMode != "none") {
        stemmingDictionary0 <- stemmingDictionary
        stemmingDictionary1 <- read.csv(file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), header = TRUE, row.names = 1)
        stemmingDictionaryMerged <- merge(stemmingDictionary0, stemmingDictionary1, by = "row.names", all.x = TRUE)
        stemmingDictionaryMissing <- is.na(stemmingDictionaryMerged$stemmedTerm.y)
        for (i in 1:length(stemmingDictionaryMissing)) {
            if (stemmingDictionaryMissing[i] == FALSE) 
                stemmingDictionary$stemmedTerm[i] <- as.character(stemmingDictionaryMerged$stemmedTerm.y[i]) else stemmingDictionary$stemmedTerm[i] <- as.character(stemmingDictionaryMerged$stemmedTerm.x[i])
        }
        if (stemmingEditMode == "dfedit") {
            stemmingDictionary <- dfedit(stemmingDictionary)
        }
        write.csv(stemmingDictionary, file = file.path(nameOfProject, paste(nameOfProject, "stemmingDictionary.csv")), row.names = TRUE)
    }
    stemmingDictionary
}

StemCorpusDtm <- function(corpusDtm, stemmingDictionary) {
    dtm <- DocumentTermMatrix(corpus)
    dtm <- rollup(dtm, 2, stemmingDictionary[["stemmedTerm"]])
    dtm <- dtm[, Terms(dtm) != ""]
    dtm
} 
