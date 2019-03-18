## Analyze sentiments over time

#setwd("./Results")
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

direct <- paste0("./sentiments/", dir("./sentiments"))
sentiments <- list()
freqSent <- list()
all.sentiments <- character()

for (i in seq_along(direct)) {
  sentiments[[i]] <- read_csv(direct[i]) ## Read in the sentiment table for each month
  sentiments[[i]] <- sentiments[[i]][complete.cases(sentiments[[i]]), ] ## Remove non-sentimental words
  all.sentiments <- union(all.sentiments, sentiments[[i]]$sentiment) ## Establish all the sentiments found
  freqSent[[i]] <- table(sentiments[[i]]$sentiment)
}

set.seed(12)
sentCol <- sample(colours(distinct = T)[seq_along(all.sentiments)+8])

months <- seq(from = ymd("2016-05-01"), to = ymd("2019-01-01"), by = "months")

names(sentiments) <- months
freqSent <- lapply(freqSent, function(x) {y <- as_tibble(x); colnames(y) <- c("sentiment", "count"); y})

for (i in seq_along(direct)) {
  write.csv(freqSent[[i]], paste0("./sentimentcounts/count", dir("./sentiments")[i]), row.names = F)
}

for (i in seq_along(direct)) {
  freqSent[[i]] <- cbind(month = rep(months[i], nrow(freqSent[[i]])), freqSent[[i]])
  colnames(freqSent[[i]])[1] <- "month"
}

freqSent <- bind_rows(freqSent)
freqSent$sentiment <- str_to_title(freqSent$sentiment)

pdf("../Data Vis/sentimentMonthlyProp.pdf")

ggplot(freqSent[1:104, ], aes(fill = sentiment, x = month, y = count)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_manual(values = sentCol) +
  xlab("Months during 2016") + 
  ylab("Proportions of each Sentiment Expressed by Word") + 
  ggtitle("Sentiment Analysis of #ymmfire Tweets by Proportions") + 
  labs(fill = "Sentiment") +
  scale_x_date(breaks = ymd(months[1:8]), date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
