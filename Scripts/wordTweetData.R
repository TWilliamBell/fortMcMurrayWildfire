setwd("../Data")
library(data.table)

direct <- dir()
j <- 1

noGrammar <- function(data) gsub('[[:punct:] ]+','', data, perl = T)

for (i in seq_len(33)) {
  tweets <- strsplit(fread(paste0("./", direct[i]))$text, split = " ")
  wordsEach <- rapply(tweets, length)
  wordStart <- c(0, cumsum(wordsEach))
  totalWords <- sum(wordsEach)
  totalTweets <- length(wordsEach)
  key <- rep(NA, totalWords)
  words <- rep(NA, totalWords)
  for (j in seq_len(totalTweets)) {
    key[(wordStart[j]+1):wordStart[j+1]] <- j
    words[(wordStart[j]+1):wordStart[j+1]] <- tweets[[j]]
  }
  words <- noGrammar(words)
  wordsTweet <- data.frame(tweets = key, words = words)
  write.csv(wordsTweet, paste0("../Results/words/words-", direct[i]), row.names = F)
  break
}
