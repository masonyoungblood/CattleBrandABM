#load packages and data
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
library(performance)
library(DHARMa)
library(lme4)
library(specr)
library(cowplot)
load("analysis/shuffling_model/shuffling_data_order.RData")

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

#run all random intercept models
space_model <- lmer(prob_score ~ (1|space), data = shuffling_data)
time_model <- lmer(prob_score ~ (1|time), data = shuffling_data)
brand_model <- lmer(prob_score ~ (1|brand), data = shuffling_data)

#get intra-class coefficients for each random effect
performance::icc(space_model)
performance::icc(time_model)
performance::icc(brand_model)

#add fixed effects and controls to models
complexity_model <- lmer(prob_score ~ complexity + (1|brand), data = shuffling_data)
#actual_model <- lmer(prob_score ~ complexity + actual + (1|brand), data = shuffling_data)
actual_model <- lmer(prob_score ~ actual + (1|brand), data = shuffling_data)
#mixed_model <- lmer(prob_score ~ complexity + actual*mixed + (1|brand), data = shuffling_data)
mixed_model <- lmer(prob_score ~ actual*mixed + (1|brand), data = shuffling_data)

#get WAIC values for competing models
#brand_waic <- blmeco::WAIC(brand_model)
#complexity_waic <- blmeco::WAIC(complexity_model)
#actual_waic <- blmeco::WAIC(actual_model)
#mixed_waic <- blmeco::WAIC(mixed_model)
#waic <- list(brand_waic = brand_waic, complexity_waic = complexity_waic, actual_waic = actual_waic, mixed_waic = mixed_waic)
#save(waic, file = "analysis/shuffling_model/waic.RData")
load("analysis/shuffling_model/waic.RData")

#compare WAIC values
waic$brand_waic$WAIC1 - waic$complexity_waic$WAIC1
waic$complexity_waic$WAIC1 - waic$actual_waic$WAIC1
waic$actual_waic$WAIC1 - waic$mixed_waic$WAIC1

#get model estimates from mixed model
summary(mixed_model)
mixed_model_confints <- confint(mixed_model, method = "boot", nsim = 50, oldNames = FALSE)
mixed_model_confints

#do traditional checks (look good)
plot(mixed_model)
qqnorm(residuals(mixed_model))

#specification curve analysis: https://masurp.github.io/specr/articles/random_effects.html
null <- function(formula, data, ...){
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula)
  lm(formula, data)
}
brand <- function(formula, data, ...){
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula, "+ (1|brand)")
  lmer(formula, data)
}
time <- function(formula, data, ...){
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula, "+ (1|time)")
  lmer(formula, data)
}
space <- function(formula, data, ...){
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula, "+ (1|space)")
  lmer(formula, data)
}

#run models
spec_curve_models <- run_specs(df = shuffling_data,
                               y = c("prob_score"),
                               x = c("actual", "mixed"),
                               model = c("null", "brand", "space", "time"),
                               controls = c("complexity"))

#save plot
png("analysis/shuffling_model/spec_curve.png", units = "in", width = 5, height = 4.2, res = 300)
plot_specs(spec_curve_models, choices = c("x", "model", "controls"))
dev.off()
