library(tidytext)
library(dplyr)
library(data.table)

setwd("./Results/wordfreq")
direct <- dir()

substrtoend <- function(str, start) substr(str, start, stop = nchar(str))

for (i in seq_len(33)) {
  wordfreq <- fread(paste0("./", direct[i]))
  wordsentiment <- left_join(wordfreq, sentiments, by = c("words" = "word"))
  sentimentresult <- wordsentiment[ , c(1, 2, 3)]
  write.csv(sentimentresult, paste0("../sentiments/", "sentiment", substrtoend(direct[i], start = 6)), row.names = F)
}
