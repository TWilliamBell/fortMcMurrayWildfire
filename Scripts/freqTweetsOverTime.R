library(readr)
setwd('./Data')
list <- dir()

nHist <- rep(NA, 33)

for (i in seq_along(list)) {
  sheet <- read_csv(list[i])
  nHist[i] <- nrow(sheet)
}

barplot(nHist[1:12], xlab = "Months", ylab = "Tweets", main = "#ymmfire Hashtag use during Fort McMurray Wildires")

list <- gsub(".csv", "", list)

results <- data.frame(month = list, count = nHist)

write.csv(results, "../Results/countymmfireMonthly.csv", row.names = F)
