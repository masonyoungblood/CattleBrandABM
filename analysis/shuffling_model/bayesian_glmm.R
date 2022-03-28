#load packages and data
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM/analysis/shuffling_model")
load("shuffling_data.RData")
library(brms)

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

# #run brms model
# bayesian_glmm <- brm(data = shuffling_data, family = gaussian(),
#                      prob_score ~ complexity + actual*mixed + (1|brand),
#                      prior = c(prior(normal(0, 1), class = Intercept),
#                                prior(normal(0, 0.5), class = b, coef = "actual"),
#                                prior(normal(0, 0.5), class = b, coef = "mixed"),
#                                prior(normal(0, 0.5), class = b, coef = "actual:mixed"),
#                                prior(normal(0, 0.5), class = b, coef = "complexity"),
#                                prior(exponential(3), class = sigma)),
#                      algorithm = "meanfield", output_samples = 5000, iter = 50000, tol_rel_obj = 0.00001)
# save(bayesian_glmm, file = "bayesian_glmm.RData")
load("bayesian_glmm.RData")

posterior_summary(bayesian_glmm, variable = c("b_Intercept", "b_complexity", "b_actual", "b_mixed", "b_actual:mixed", "sigma"))
summary(bayesian_glmm)

