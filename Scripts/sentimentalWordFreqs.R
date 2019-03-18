source("./Scripts/Default_Package_of_Functions.R")

include("readr")
include("wordcloud")
include("lubridate")

direct <- dir("./Results/sentiments")

sentimentTweetCounts <- list()

for (i in seq_along(direct)) {
  sentimentTweetCounts[[i]] <- readr::read_csv(paste0("./Results/sentiments/", direct[i]))
}

sentimentTweetCounts <- lapply(sentimentTweetCounts, function(x) table(unique(x[complete.cases(x), ][ , -3])[ , -1]))

sentimentTweetCounts <- lapply(sentimentTweetCounts, function(x) {y = as.data.frame(cbind(names(x), unname(x))); y[ , 2] <- as.integer(y[ , 2]); colnames(y) <- c("Word", "Frequency"); y})

months <- seq(from = lubridate::ymd("2016-05-01"), to = lubridate::ymd("2019-01-01"), by = "months")

for (i in seq_along(direct)) {
  write.csv(sentimentTweetCounts[[i]], paste0("./Results/sentimentalWordFreqs/word-freqs-", direct[i]), row.names = F)
  pdf(paste0("./Data Vis/wordcloud/", months[i], ".pdf"))
  wordcloud::wordcloud(sentimentTweetCounts[[i]][ , 1], sentimentTweetCounts[[i]][ , 2], min.freq = max(median(sentimentTweetCounts[[i]][ , 2]), 20), colors = rainbow(min(10, nrow(sentimentTweetCounts[[i]]))))
  dev.off()
}

