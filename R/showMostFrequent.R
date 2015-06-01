
ShowMostFrequent <- function(corpus, mode = "graph", number = 10, stemCompletion = FALSE, corpusOriginal = "", minFrequency = 0) {
  corpusDtm <- DocumentTermMatrix(corpus)
  freq <- sort(colSums(as.matrix(corpusDtm)), decreasing=TRUE)
  if (number == "all") {
    number <- length(freq)
  }
  wordFrequency <- data.frame(word=names(freq), freq=freq)[1:number, ]
  if (stemCompletion == TRUE) {
    wordFrequency$word <- stemCompletion(wordFrequency$word, corpusOriginal)
    for (i in 1:length(wordFrequency$word)) {
      if (wordFrequency$word[i] == "") {
        wordFrequency$word[i] <- row.names(wordFrequency)[i]
      }
    }
  }
  if (mode == "barchart") {
    ggplot(data = wordFrequency, aes(x=reorder(word, -freq), y=freq)) + geom_bar(stat="identity") + coord_flip()
  } else if (mode == "wordcloud") {
    wordcloud(rownames(wordFrequency), wordFrequency$freq, min.freq=minFrequency, colors=brewer.pal(6, "Dark2"))    
  } else if (mode == "vector") {
    wordFrequency
  }
}
