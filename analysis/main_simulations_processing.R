#set working directory, load data, etc.
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
source("cattlebrandABM.R")

#load and combine simulation results into a single object
load(paste0("analysis/main_simulations_1.RData"))
a <- main_simulations
load(paste0("analysis/main_simulations_2.RData"))
b <- main_simulations
load(paste0("analysis/main_simulations_3.RData"))
c <- main_simulations
load(paste0("analysis/main_simulations_4.RData"))
d <- main_simulations
load(paste0("analysis/main_simulations_5.RData"))
e <- main_simulations
main_simulations <- list(priors = rbind(a$priors, b$priors, c$priors, d$priors, e$priors),
                         sum_stats = c(a$sum_stats, b$sum_stats, c$sum_stats, d$sum_stats, e$sum_stats))
rm(list = c("a", "b", "c", "d", "d"))

#separate brands data by year
brands_1990 <- data.table::data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#get vector of observed summary statistics
obs_sum_stats <- c(get_sum_stats(as.matrix(brands_2008), components, all_zips, angles = FALSE),
                   get_sum_stats(as.matrix(brands_2014), components, all_zips, angles = FALSE),
                   get_sum_stats(as.matrix(brands_2015), components, all_zips, angles = FALSE),
                   get_sum_stats(as.matrix(brands_2016), components, all_zips, angles = FALSE))

#convert to data frame with same structure as vector of observed summary statistics
sum_stats <- do.call("rbind", lapply(1:length(main_simulations$sum_stats), function(x){c(t(as.matrix(main_simulations$sum_stats[[x]])))}))
