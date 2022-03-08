#load packages and data
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
library(brms)
load("analysis/shuffling_model/shuffling_data.RData")

#data is zero inflated (8.5% of values)
length(which(shuffling_data$prob_score == 0))/nrow(shuffling_data)

#drop zeroes from the data
shuffling_data <- shuffling_data[-which(shuffling_data$prob_score == 0), ]

#convert variables to factors and binarize complexity
shuffling_data$time <- as.factor(shuffling_data$time)
shuffling_data$space <- as.factor(shuffling_data$space)
shuffling_data$brand <- as.factor(shuffling_data$brand)
shuffling_data$brand_num <- as.numeric(shuffling_data$brand)
shuffling_data$mixed <- ifelse(shuffling_data$mixed, 1, 0)
shuffling_data$complexity <- shuffling_data$complexity-2

#scale and log transform the probability score
shuffling_data$prob_score <- as.numeric(scale(log(shuffling_data$prob_score)))

#run mixed model
mixed_model <- REPLACE

#save output
save(mixed_model, file = "mixed_model.RData")
