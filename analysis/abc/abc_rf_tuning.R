#load packages
library(data.table)
library(tuneRanger)

#set working directory, load data, etc.
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
source("cattlebrandABM.R")

#load data
load("analysis/main_simulations/main_simulations_1.RData")
a <- main_simulations
load("analysis/main_simulations/main_simulations_2.RData")
b <- main_simulations
load("analysis/main_simulations/main_simulations_3.RData")
c <- main_simulations
load("analysis/main_simulations/main_simulations_4.RData")
d <- main_simulations
load("analysis/main_simulations/main_simulations_5.RData")
e <- main_simulations
main_simulations <- list(priors = rbind(a$priors, b$priors, c$priors, d$priors, e$priors), sum_stats = c(a$sum_stats, b$sum_stats, c$sum_stats, d$sum_stats, e$sum_stats))
rm(list = c("a", "b", "c", "d", "e"))

#separate brands data by year
brands_1990 <- data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table(brands[which(brands[, 10] == 2016), 1:9])

#get vector of observed summary statistics
obs_stats <- c(get_sum_stats(as.matrix(brands_2008), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2014), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2015), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2016), components, all_zips, angles = FALSE))

#convert to data frame with same structure as vector of observed summary statistics
sum_stats <- do.call("rbind", lapply(1:length(main_simulations$sum_stats), function(x){c(t(as.matrix(main_simulations$sum_stats[[x]])))}))

#restructure obs stats
obs_stats <- as.data.frame(t(obs_stats))

#log transform complexity and strength params, and logit transform radii (functions adopted from abc package)
logit_bounds <- c(0, 690)
logit <- function(param, logit_bounds){
  temp <- (param - logit_bounds[1])/(logit_bounds[2] - logit_bounds[1])
  return(log(temp/(1 - temp)))
}
inv_logit <- function(param, logit_bounds){
  temp <- exp(param)/(1 + exp(param))
  return((temp*(logit_bounds[2] - logit_bounds[1])) + logit_bounds[1])
}
main_simulations$priors[, 1] <- log(main_simulations$priors[, 1])
main_simulations$priors[, 2] <- logit(main_simulations$priors[, 2], logit_bounds)
main_simulations$priors[, 3] <- logit(main_simulations$priors[, 3], logit_bounds)
main_simulations$priors[, 4] <- log(main_simulations$priors[, 4])
main_simulations$priors[, 5] <- log(main_simulations$priors[, 5])

#set sample size for random forest (80% of data)
sample_fraction <- 0.8

#set sample used for tuning (10% of data)
tuning_sample <- sample(nrow(sum_stats), nrow(sum_stats)*0.10)

#number of cores
ncores <- detectCores() - 1

#construct object to hold tuning settings
abc_rf_tuning <- list()

#for each parameter in the model
for(i in 1:5){
  #set seed
  set.seed(i)
  
  #construct data frame for random forest abc
  abcrf_data <- data.frame(main_simulations$priors[, i], sum_stats = sum_stats)
  colnames(abcrf_data)[1] <- "param"
  colnames(obs_stats) <- colnames(abcrf_data)[-1]
  
  #tuning for best random forest values
  task <- makeRegrTask(data = abcrf_data[tuning_sample,], target = "param")
  abc_rf_tuning[[i]] <- tuneRanger(task, num.trees = 500, parameters = list(sample.fraction = sample_fraction), 
                            tune.parameters = c("mtry", "min.node.size"), num.threads = ncores)
  
  #print status
  cat(paste0(i, "..."))
  
  #save tuning
  save(abc_rf_tuning, file = "old_code/abc_rf_tuning.RData")
}

#save recommended parameters
abc_rf_tuning <- lapply(1:length(abc_rf_tuning), function(x){abc_rf_tuning[[x]]$recommended.pars})
save(abc_rf_tuning, file = "analysis/abc/abc_rf_tuning.RData")
