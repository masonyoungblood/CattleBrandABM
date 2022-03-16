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

#modify function from abcrf::densityPlot.regAbcrf for weight extraction
extract_weights <- function(object, obs, training, paral=FALSE, ncores= if(paral) max(detectCores()-1,1) else 1, ...){
  x <- obs
  
  mf <- match.call(expand.dots=FALSE)
  mf <- mf[1]
  mf$formula <- object$formula
  
  mf$data <- training
  
  training <- mf$data
  
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame() )
  mt <- attr(mf, "terms")
  resp <- model.response(mf)
  
  obj <- object$model.rf
  inbag <- matrix(unlist(obj$inbag.counts, use.names=FALSE), ncol=obj$num.trees, byrow=FALSE)
  
  obj[["origNodes"]] <- predict(obj, training, predict.all=TRUE, num.threads=ncores)$predictions
  obj[["origObs"]] <- model.response(mf)
  
  #####################
  
  origObs <- obj$origObs
  origNodes <- obj$origNodes
  
  nodes <- predict(obj, x, predict.all=TRUE, num.threads=ncores )$predictions
  if(is.null(dim(nodes))) nodes <- matrix(nodes, nrow=1)
  ntree <- obj$num.trees
  nobs <- object$model.rf$num.samples
  nnew <- nrow(x)
  
  weights <- abcrf:::findweights(origNodes, nodes, inbag, as.integer(nobs), as.integer(nnew), as.integer(ntree)) # cpp function call
  weights.std <- weights/ntree
  
  return(weights.std[, 1])
}

#load data
load("analysis/abc/abc_rf_tuning.RData")
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
brands_1990 <- data.table::data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#get vector of observed summary statistics
obs_stats <- c(get_sum_stats(as.matrix(brands_2008), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2014), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2015), components, all_zips, angles = FALSE),
               get_sum_stats(as.matrix(brands_2016), components, all_zips, angles = FALSE))

#convert to data frame with same structure as vector of observed summary statistics
sum_stats <- do.call("rbind", lapply(1:length(main_simulations$sum_stats), function(x){c(t(as.matrix(main_simulations$sum_stats[[x]])))}))

#restructure obs stats
obs_stats <- as.data.frame(t(obs_stats))

#store priors in new object for transformation for random forest
transf_priors <- main_simulations$priors

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
transf_priors[, 1] <- log(transf_priors[, 1])
transf_priors[, 2] <- logit(transf_priors[, 2], logit_bounds)
transf_priors[, 3] <- logit(transf_priors[, 3], logit_bounds)
transf_priors[, 4] <- log(transf_priors[, 4])
transf_priors[, 5] <- log(transf_priors[, 5])

#set sample size for random forest (80% of data)
sample_size <- 0.8*nrow(sum_stats)

#construct object to hold predictions
abc_rf_predictions <- list()

#set number of cores
ncores <- detectCores() - 1

#set value of i
i <- 1

#run loop (between comments below) manually
# -------------------------------------------------------------------------

#set random seed
set.seed(i)

#construct data frame for random forest abc
abcrf_data <- data.frame(transf_priors[, i], sum_stats = sum_stats)
colnames(abcrf_data)[1] <- "param"
colnames(obs_stats) <- colnames(abcrf_data)[-1]

#run random forest with recommended values
reg_abcrf <- regAbcrf(formula = param ~ ., data = abcrf_data, ntree = 1000, mtry = abc_rf_tuning[[i]]$mtry, min.node.size = abc_rf_tuning[[i]]$min.node.size, sampsize = sample_size, paral = TRUE, ncores = ncores)

#add all output to the predictions object
abc_rf_predictions[[i]] <- list(OOB_MSE = reg_abcrf$model.rf$prediction.error, OOB_NMAE = reg_abcrf$model.rf$NMAE, prediction = predict(object = reg_abcrf, obs = obs_stats, training = abcrf_data, paral = TRUE, ncores = ncores),
                                var_importance = sort(reg_abcrf$model.rf$variable.importance, decreasing = TRUE), weights = extract_weights(object = reg_abcrf, obs = obs_stats, training = abcrf_data, paral = TRUE, ncores = ncores))

#save predictions
save(abc_rf_predictions, file = "analysis/abc/abc_rf_predictions.RData")

#add 1 to i
i <- i + 1

# -------------------------------------------------------------------------

#load saved predictions
load("analysis/abc/abc_rf_predictions.RData")

#construct predictions table, with mse because (1) nmae is inf for radii and (2) it can be easily back-transformed
predictions_table <- data.frame(median = c(exp(as.numeric(abc_rf_predictions[[1]]$prediction$med)),
                                           inv_logit(as.numeric(abc_rf_predictions[[2]]$prediction$med), logit_bounds),
                                           inv_logit(as.numeric(abc_rf_predictions[[3]]$prediction$med), logit_bounds),
                                           exp(as.numeric(abc_rf_predictions[[4]]$prediction$med)),
                                           exp(as.numeric(abc_rf_predictions[[5]]$prediction$med))),
                                low_quant = c(exp(as.numeric(abc_rf_predictions[[1]]$prediction$quantiles[1])),
                                              inv_logit(as.numeric(abc_rf_predictions[[2]]$prediction$quantiles[1]), logit_bounds),
                                              inv_logit(as.numeric(abc_rf_predictions[[3]]$prediction$quantiles[1]), logit_bounds),
                                              exp(as.numeric(abc_rf_predictions[[4]]$prediction$quantiles[1])),
                                              exp(as.numeric(abc_rf_predictions[[5]]$prediction$quantiles[1]))),
                                high_quant = c(exp(as.numeric(abc_rf_predictions[[1]]$prediction$quantiles[2])),
                                               inv_logit(as.numeric(abc_rf_predictions[[2]]$prediction$quantiles[2]), logit_bounds),
                                               inv_logit(as.numeric(abc_rf_predictions[[3]]$prediction$quantiles[2]), logit_bounds),
                                               exp(as.numeric(abc_rf_predictions[[4]]$prediction$quantiles[2])),
                                               exp(as.numeric(abc_rf_predictions[[5]]$prediction$quantiles[2]))),
                                rmsle = c(sqrt(as.numeric(abc_rf_predictions[[1]]$OOB_MSE)),
                                          sqrt(as.numeric(abc_rf_predictions[[2]]$OOB_MSE)),
                                          sqrt(as.numeric(abc_rf_predictions[[3]]$OOB_MSE)),
                                          sqrt(as.numeric(abc_rf_predictions[[4]]$OOB_MSE)),
                                          sqrt(as.numeric(abc_rf_predictions[[5]]$OOB_MSE))))

#plot
xlabs <- list(expression(Complexity~"("*italic(paste(lambda))*")"), expression(Copy~Radius~"("*italic(R[C])*")"), expression(Dist~Radius~"("*italic(R[D])*")"), expression(Copy~Strength~"("*italic(C)*")"), expression(Dist~Strength~"("*italic(D)*")"))
xlims <- list(c(0, 8), c(1, 689), c(1, 689), c(0, 10), c(0, 10))
group_lines <- c("1" = "dotted", "0" = "solid")
for(i in 1:ncol(transf_priors)){
  if(i %in% c(1, 4, 5)){
    temp <- density(exp(transf_priors[, i]), weights = abc_rf_predictions[[i]]$weights)
    prior <- data.frame(value = density(exp(transf_priors[, i]))$x, density = density(exp(transf_priors[, i]))$y, id = 0, lt = 1)
  } else{
    temp <- density(inv_logit(transf_priors[, i], logit_bounds = logit_bounds), weights = abc_rf_predictions[[i]]$weights)
    prior <- data.frame(value = density(inv_logit(transf_priors[, i], logit_bounds = logit_bounds))$x, density = density(inv_logit(transf_priors[, i], logit_bounds = logit_bounds))$y, id = 0, lt = 1)
  }
  posterior <- data.frame(value = temp$x, density = temp$y, id = 1, lt = 0)
  combined <- rbind(posterior, prior)
  plot <- ggplot(combined) + geom_line(mapping = aes(x = value, y = density, group = id, linetype = as.factor(lt))) + 
    labs(x = xlabs[[i]], y = "Density") + xlim(xlims[[i]]) + 
    theme_linedraw() + 
    scale_linetype_manual(name = NULL, values = group_lines, labels = c("Prior", "Posterior"))
  assign(names(transf_priors)[i], plot)
}

#combine plots and print to file
png("analysis/abc/abc_rf_posteriors.png", units = "in", width = 11, height = 7, res = 300)
legend <- get_legend(complexity)
plot_grid(complexity + theme(legend.position="none"),
          copy_radius + theme(legend.position="none"),
          dist_radius + theme(legend.position="none"),
          copy_strength + theme(legend.position="none"),
          dist_strength + theme(legend.position="none"),
          legend, labels = c("A", "B", "C", "D", "E"), align = "hv")
dev.off()
