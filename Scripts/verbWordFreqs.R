source("./Scripts/Default_Package_of_Functions.R")

include("dplyr")
include("tidytext")
include("readr")

verbs <- tidytext::parts_of_speech[grep("Verb", as.data.frame(tidytext::parts_of_speech)[ , 2]), ]

colnames(verbs)[1] <- "words"

words <- list()

for (file in dir("./Results/words")) {
  words[[file]] <- readr::read_csv(paste0("./Results/words/", file))
  words[[file]] <- dplyr::inner_join(words[[file]], verbs, by = "words")
  freqs <- table(words[[file]][ , 2])
  pdf(paste0("./Data Vis/wordcloud verbs/", substr(file, 7, 16), "-wordcloudVerbs.pdf"))
  wordcloud::wordcloud(names(freqs), unname(freqs), min.freq = mean(unname(freqs)))
  dev.off()
}
