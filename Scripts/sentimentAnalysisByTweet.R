source("./Scripts/Default_Package_of_Functions.R")

include("readr")
include("dplyr")
include("lubridate")
include("stringr")
library("ggplot2")

direct <- paste0("./Results/sentiments/", dir("./Results/sentiments"))

sentimentsByTweet <- list()

for (i in seq_along(direct)) {
  sentimentsByTweet[[i]] <- readr::read_csv(direct[i])
}

byMonth <- seq(from = lubridate::ymd("2016-05-01"), to = lubridate::ymd("2019-01-01"), by = "months")

sentimentsByTweet <- lapply(sentimentsByTweet, function(x) unique(x[complete.cases(x), ]))

for (i in seq_along(direct)) {
  sentimentsByTweet[[i]] <- cbind(month = rep(byMonth[i], nrow(sentimentsByTweet[[i]])), sentimentsByTweet[[i]])
}

sentimentsByTweet <- dplyr::bind_rows(sentimentsByTweet)

sentimentsByTweet$sentiment <- stringr::str_to_sentence(sentimentsByTweet$sentiment)

set.seed(12)
sentCol <- sample(colours(distinct = T)[seq_along(unique(sentimentsByTweet$sentiment))+8])

is2016 <- grepl("2016", sentimentsByTweet$month)

pdf("./Data Vis/sentimentsByTweet.pdf")

ggplot(sentimentsByTweet[is2016, ], aes(fill = sentiment, x = month)) +
  geom_bar(stat = 'count', position = 'fill') +
  scale_fill_manual(values = sentCol) +
  xlab("Months during 2016") +
  ylab("Proportions of each Sentiment Expressed by Tweet") +
  ggtitle("Sentiment Analysis of #ymmfire Tweets by Proportions") +
  labs(fill = "Sentiment") +
  scale_x_date(breaks = byMonth, date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

rm(is2016)
