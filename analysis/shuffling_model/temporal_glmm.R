#load packages and data
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
library(performance)
library(DHARMa)
library(lme4)
library(specr)
library(cowplot)
library(fitdistrplus)
library(brms)
load("analysis/shuffling_model/subsets.RData")

#combine o and y data for ne
ne_data <- as.data.frame(rbind(subsets$o_ne_2, subsets$o_ne_3, subsets$y_ne_2, subsets$y_ne_3))[, 1:4]
ne_data$brand <- sapply(1:nrow(ne_data), function(x){paste(as.numeric(ne_data[x, 1:4])[-which(as.numeric(ne_data[x, 1:4]) == 0)], collapse = ", ")})
ne_data$time <- c(rep("o", nrow(subsets$o_ne_2)), rep("o", nrow(subsets$o_ne_3)), rep("y", nrow(subsets$y_ne_2)), rep("y", nrow(subsets$y_ne_3)))

#calculate dists
ne_dists <- adist(sapply(1:nrow(ne_data), function(x){intToUtf8(as.numeric(ne_data[x, 1:4]))}))

#iterate through the upper triangle and convert to a list of data frames, then combine
ne_data <- lapply(1:(nrow(ne_data)-1), function(i){
  temp_dists <- ne_dists[i, (i+1):nrow(ne_data)]
  temp_brand_a <- rep(ne_data$brand[i], length(temp_dists))
  temp_brand_b <- ne_data$brand[(i+1):nrow(ne_data)]
  temp_time <- paste0(ne_data$time[i], ne_data$time[(i+1):nrow(ne_data)])
  return(data.frame(dist = temp_dists, brand_a = temp_brand_a, brand_b = temp_brand_b, time = temp_time))
})
ne_data <- do.call(rbind, ne_data)
ne_data$space <- rep("ne", nrow(ne_data))

#combine o and y data for se
se_data <- as.data.frame(rbind(subsets$o_se_2, subsets$o_se_3, subsets$y_se_2, subsets$y_se_3))[, 1:4]
se_data$brand <- sapply(1:nrow(se_data), function(x){paste(as.numeric(se_data[x, 1:4])[-which(as.numeric(se_data[x, 1:4]) == 0)], collapse = ", ")})
se_data$time <- c(rep("o", nrow(subsets$o_se_2)), rep("o", nrow(subsets$o_se_3)), rep("y", nrow(subsets$y_se_2)), rep("y", nrow(subsets$y_se_3)))

#calculate dists
se_dists <- adist(sapply(1:nrow(se_data), function(x){intToUtf8(as.numeric(se_data[x, 1:4]))}))

#iterate through the upper triangle and convert do a list of data frames, then combine
se_data <- lapply(1:(nrow(se_data)-1), function(i){
  temp_dists <- se_dists[i, (i+1):nrow(se_data)]
  temp_brand_a <- rep(se_data$brand[i], length(temp_dists))
  temp_brand_b <- se_data$brand[(i+1):nrow(se_data)]
  temp_time <- paste0(se_data$time[i], se_data$time[(i+1):nrow(se_data)])
  return(data.frame(dist = temp_dists, brand_a = temp_brand_a, brand_b = temp_brand_b, time = temp_time))
})
se_data <- do.call(rbind, se_data)
se_data$space <- rep("se", nrow(se_data))

#combine o and y data for nw
nw_data <- as.data.frame(rbind(subsets$o_nw_2, subsets$o_nw_3, subsets$y_nw_2, subsets$y_nw_3))[, 1:4]
nw_data$brand <- sapply(1:nrow(nw_data), function(x){paste(as.numeric(nw_data[x, 1:4])[-which(as.numeric(nw_data[x, 1:4]) == 0)], collapse = ", ")})
nw_data$time <- c(rep("o", nrow(subsets$o_nw_2)), rep("o", nrow(subsets$o_nw_3)), rep("y", nrow(subsets$y_nw_2)), rep("y", nrow(subsets$y_nw_3)))

#calculate dists
nw_dists <- adist(sapply(1:nrow(nw_data), function(x){intToUtf8(as.numeric(nw_data[x, 1:4]))}))

#iterate through the upper triangle and convert do a list of data frames, then combine
nw_data <- lapply(1:(nrow(nw_data)-1), function(i){
  temp_dists <- nw_dists[i, (i+1):nrow(nw_data)]
  temp_brand_a <- rep(nw_data$brand[i], length(temp_dists))
  temp_brand_b <- nw_data$brand[(i+1):nrow(nw_data)]
  temp_time <- paste0(nw_data$time[i], nw_data$time[(i+1):nrow(nw_data)])
  return(data.frame(dist = temp_dists, brand_a = temp_brand_a, brand_b = temp_brand_b, time = temp_time))
})
nw_data <- do.call(rbind, nw_data)
nw_data$space <- rep("nw", nrow(nw_data))

#combine o and y data for sw
sw_data <- as.data.frame(rbind(subsets$o_sw_2, subsets$o_sw_3, subsets$y_sw_2, subsets$y_sw_3))[, 1:4]
sw_data$brand <- sapply(1:nrow(sw_data), function(x){paste(as.numeric(sw_data[x, 1:4])[-which(as.numeric(sw_data[x, 1:4]) == 0)], collapse = ", ")})
sw_data$time <- c(rep("o", nrow(subsets$o_sw_2)), rep("o", nrow(subsets$o_sw_3)), rep("y", nrow(subsets$y_sw_2)), rep("y", nrow(subsets$y_sw_3)))

#calculate dists
sw_dists <- adist(sapply(1:nrow(sw_data), function(x){intToUtf8(as.numeric(sw_data[x, 1:4]))}))

#iterate through the upper triangle and convert do a list of data frames, then combine
sw_data <- lapply(1:(nrow(sw_data)-1), function(i){
  temp_dists <- sw_dists[i, (i+1):nrow(sw_data)]
  temp_brand_a <- rep(sw_data$brand[i], length(temp_dists))
  temp_brand_b <- sw_data$brand[(i+1):nrow(sw_data)]
  temp_time <- paste0(sw_data$time[i], sw_data$time[(i+1):nrow(sw_data)])
  return(data.frame(dist = temp_dists, brand_a = temp_brand_a, brand_b = temp_brand_b, time = temp_time))
})
sw_data <- do.call(rbind, sw_data)
sw_data$space <- rep("sw", nrow(sw_data))

#combine into one big data frame
data <- rbind(ne_data, se_data, nw_data, sw_data)
rm(list = c("subsets", "ne_data", "ne_dists", "se_data", "se_dists", "nw_data", "nw_dists", "sw_data", "sw_dists"))

#create dummy variables for time
data$oo <- 0
data$yy <- 0
data$oo[which(data$time == "oo")] <- 1
data$yy[which(data$time == "yy")] <- 1

#convert brands to numeric ids
data$brand_a <- as.factor(data$brand_a)
data$brand_b <- as.factor(data$brand_b)

# #binomial is likely best, because it is count data with a maximum, let's test it: https://stats.stackexchange.com/questions/278344/how-can-i-test-if-my-observed-pdf-follows-a-binomial-distribution
# binom_fit <- fitdist(data = data$dist, dist = "binom", fix.arg = list(size = 3), start = list(prob = 0.5))
# plot(binom_fit)

# #frequentist version
# temporal_glmm <- glmer(cbind(dist, 3-dist) ~ (1|brand_a) + (1|brand_b) + oo + yy, data = data, family = binomial())
# save(temporal_glmm, file = "analysis/shuffling_model/temporal_glmm.RData")
# 
# #get confidence intervals
# confint(temporal_glmm, parm = c("oo", "yy"), method = "Wald")

#bayesian version
temporal_bayesian_glmm <- brm(data = data, family = binomial(),
                              dist | trials(3) ~ (1|brand_a) + (1|brand_b) + oo + yy,
                              prior = c(prior(normal(0, 1), class = Intercept),
                                        prior(normal(0, 0.5), class = b)),
                              algorithm = "meanfield", output_samples = 5000, iter = 50000, tol_rel_obj = 0.00001)
save(temporal_bayesian_glmm, file = "analysis/shuffling_model/temporal_bayesian_glmm.RData")


