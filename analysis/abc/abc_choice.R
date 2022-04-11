#load packages
library(data.table)
library(ggplot2)
library(ggfortify)
library(abcrf)
library(EasyABC)
library(abc)
library(tuneRanger)
library(cowplot)

#set working directory, load data, etc.
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
#setwd(system("pwd", intern = T))
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
source("cattlebrandABM.R")

#load data
load("analysis/choice_simulations/choice_simulations_1.RData")
a <- choice_simulations
load("analysis/choice_simulations/choice_simulations_2.RData")
b <- choice_simulations
load("analysis/choice_simulations/choice_simulations_3.RData")
c <- choice_simulations
choice_simulations <- list(priors = rbind(a$priors, b$priors, c$priors), sum_stats = c(a$sum_stats, b$sum_stats, c$sum_stats))
rm(list = c("a", "b", "c"))

#mix locations and years for 2008-2016
to_mix <- brands[which(brands[, 10] != 1990), ]
to_mix[, 9] <- sample(to_mix[, 9], length(to_mix[, 9]))
to_mix[, 10] <- sample(to_mix[, 10], length(to_mix[, 10]))
to_mix <- to_mix[order(to_mix[, 10]), ]

#separate brands data by year
brands_1990 <- data.table::data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])
brands_2008_mixed <- data.table::data.table(to_mix[which(to_mix[, 10] == 2008), 1:9])
brands_2014_mixed <- data.table::data.table(to_mix[which(to_mix[, 10] == 2014), 1:9])
brands_2015_mixed <- data.table::data.table(to_mix[which(to_mix[, 10] == 2015), 1:9])
brands_2016_mixed <- data.table::data.table(to_mix[which(to_mix[, 10] == 2016), 1:9])

#get vector of observed summary statistics
obs_stats <- c(get_sum_stats(as.matrix(brands_2008), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2014), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2015), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2016), components, all_zips, angles = FALSE))
obs_stats_mixed <- c(get_sum_stats(as.matrix(brands_2008_mixed), components, all_zips, angles = FALSE),
                     get_sum_stats(as.matrix(brands_2014_mixed), components, all_zips, angles = FALSE),
                     get_sum_stats(as.matrix(brands_2015_mixed), components, all_zips, angles = FALSE),
                     get_sum_stats(as.matrix(brands_2016_mixed), components, all_zips, angles = FALSE))

#convert to data frame with same structure as vector of observed summary statistics
sum_stats <- do.call("rbind", lapply(1:length(choice_simulations$sum_stats), function(x){c(t(as.matrix(choice_simulations$sum_stats[[x]])))}))

#create reference table
ref_table <- data.frame(index = as.factor(c(rep("null", 10000), rep("copy", 10000), rep("dist", 10000))), sum_stats = sum_stats)

#restructure obs stats
obs_stats <- as.data.frame(t(obs_stats))
obs_stats_mixed <- as.data.frame(t(obs_stats_mixed))
names(obs_stats) <- names(ref_table)[-1]
names(obs_stats_mixed) <- names(ref_table)[-1]

#set sample size for random forest (80% of data)
sample_size <- 0.8*nrow(sum_stats)

#construct random forest for model choice
abc_choice <- abcrf(index ~ ., data = ref_table, group = list("null", "copy", "dist"), sampsize = sample_size, ntree = 1000)

#predict model of observed sum stats
unmixed <- predict(abc_choice, obs = obs_stats, training = ref_table, sampsize = sample_size, ntree = 1000)
mixed <- predict(abc_choice, obs = obs_stats_mixed, training = ref_table, sampsize = sample_size, ntree = 1000)
abc_choice_prediction <- list(unmixed = unmixed, mixed = mixed)
save(abc_choice_prediction, file = "analysis/abc/abc_choice_prediction.RData")
