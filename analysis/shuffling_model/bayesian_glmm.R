#load packages and data
setwd(system("pwd", intern = T))
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
shuffling_data$inv_complexity <- shuffling_data$complexity*-1

#scale and log transform the probability score
shuffling_data$prob_score <- as.numeric(scale(log(shuffling_data$prob_score)))

#run brms model
bayesian_glmm <- brm(data = shuffling_data, family = gaussian(),
                     prob_score ~ inv_complexity + actual*mixed + (1|brand),
                     prior = c(prior(normal(0, 1), class = Intercept),
                               prior(normal(0, 0.5), class = b, coef = "actual"),
                               prior(normal(0, 0.5), class = b, coef = "mixed"),
                               prior(normal(0, 0.5), class = b, coef = "actual:mixed"),
                               prior(lognormal(0, 1), class = b, coef = "inv_complexity"),
                               prior(exponential(3), class = sigma)),
                     cores = 4, iter = 4000, normalize = FALSE, backend = "cmdstanr")
save(bayesian_glmm, file = "bayesian_glmm.RData")
