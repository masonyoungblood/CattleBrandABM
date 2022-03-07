#load packages and data
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
library(performance)
library(DHARMa)
library(lme4)
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

#run all models
space_model <- lmer(prob_score ~ (1|space), data = shuffling_data)
time_model <- lmer(prob_score ~ (1|time), data = shuffling_data)
brand_model <- lmer(prob_score ~ (1|brand), data = shuffling_data)
complexity_model <- lmer(prob_score ~ complexity + (1|brand), data = shuffling_data)
actual_model <- lmer(prob_score ~ actual + complexity + (1|brand), data = shuffling_data)
mixed_model <- lmer(prob_score ~ complexity + actual*mixed + (1|brand), data = shuffling_data)

#get intra-class coefficients for each random effect
performance::icc(space_model)
performance::icc(time_model)
performance::icc(brand_model)

#compare complexity model with random effects
compare_performance(brand_model, complexity_model, metrics = "AIC")

#compare complexity and actual model
compare_performance(complexity_model, actual_model, metrics = "AIC")

#compare actual and mixed model
compare_performance(actual_model, mixed_model, metrics = "AIC")

#get model estimates from mixed model
summary(mixed_model)
mixed_model_confints <- confint(mixed_model)
mixed_model_confints

#do traditional checks (look good)
plot(mixed_model)
qqnorm(residuals(mixed_model))

#do DHARMa checks (do not look good, so we should re-run a bayesian version of the full model)
simres <- simulateResiduals(mixed_model)
plot(simres)
