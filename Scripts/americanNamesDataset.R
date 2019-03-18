source("./Scripts/Default_Package_of_Functions.R")

direct <- paste0("./Supplementary Data/names/", dir("./Supplementary Data/names"))

nameData <- data.frame()

for (i in seq_along(direct)[-1]) {
  nameData <- rbind(nameData, read.csv(direct[i], header = F, stringsAsFactors = F))
}

nameData <- data.frame(names = unique(nameData[ , 1]))

write.csv(nameData, "./Supplementary Data/americanNames.csv", row.names = F)
