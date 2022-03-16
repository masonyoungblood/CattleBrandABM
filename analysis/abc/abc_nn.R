#load packages
library(data.table)
library(ggplot2)
library(ggfortify)
library(abcrf)
library(EasyABC)
library(abc)
library(tuneRanger)
library(parallel)
library(cowplot)

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

#NOTES
#sizenet choice: https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw
#actually sizenet should be equal to number of parameters, according to Blum et al. (2013) in Statistical Science
#nn outcompetes loclinear: https://link.springer.com/content/pdf/10.1007/s11222-009-9116-0.pdf
#nn outcompetes rf in terms of accuracy: https://academic.oup.com/bioinformatics/article/35/10/1720/5132692
#transforms recommended by: https://link.springer.com/content/pdf/10.1007/s11222-009-9116-0.pdf
#also by Blum: "Regression Approaches for ABC" in Routledge Handbook of ABC
#also recommended for RF! will implement there too... https://stackoverflow.com/questions/28239980/log-transform-dependent-variable-for-regression-tree
#weight decay link: https://towardsdatascience.com/this-thing-called-weight-decay-a7cd4bcfccab
#another: https://machinelearningmastery.com/how-to-reduce-overfitting-in-deep-learning-with-weight-regularization/

#set bounds of logit transformation
logit_bounds <- matrix(data = c(0, 0, 0, 0, 0, 0, 690, 690, 0, 0), ncol = 2)

#tolerance levels to explore
tols <- c(0.1, 0.05, 0.01, 0.005, 0.001)
tol_labels <- c(bquote(italic(paste(epsilon))~'='~0.1),
                bquote(italic(paste(epsilon))~'='~0.05),
                bquote(italic(paste(epsilon))~'='~0.01),
                bquote(italic(paste(epsilon))~'='~0.005),
                bquote(italic(paste(epsilon))~'='~0.001))

#run cross validation to determine optimal tolerance level
#error here is mean squared error divided by the variance, which i believe is the inverse of R^2: https://stats.stackexchange.com/questions/32596/what-is-the-difference-between-coefficient-of-determination-and-mean-squared
abc_nn_cv <- cv4abc(param = main_simulations$priors, sumstat = sum_stats, method = "neuralnet", nval = 100,
                    tols = tols, sizenet = 5, numnet = 10, transf = c("log", "logit", "logit", "log", "log"), logit.bounds = logit_bounds)
save(abc_nn_cv, file = "analysis/abc/abc_nn_cv.RData")

#load cross validation
load("analysis/abc/abc_nn_cv.RData")

#run full neural net ABC analysis at each tolerance level
abc_nn_predictions <- list()
abc_nn_predictions <- mclapply(1:length(tols), function(x){
  abc(target = obs_stats, param = main_simulations$priors, sumstat = sum_stats, method = "neuralnet",
      tol = tols[x], sizenet = 5, numnet = 100, transf = c("log", "logit", "logit", "log", "log"), logit.bounds = logit_bounds)
}, mc.cores = 5)
names(abc_nn_predictions) <- tols
save(abc_nn_predictions, file = "analysis/abc/abc_nn_predictions.RData")

#load predictions
load("analysis/abc/abc_nn_predictions.RData")

#plot
xlabs <- list(expression(Complexity~"("*italic(paste(lambda))*")"), expression(Copy~Radius~"("*italic(R[C])*")"), expression(Dist~Radius~"("*italic(R[D])*")"), expression(Copy~Strength~"("*italic(C)*")"), expression(Dist~Strength~"("*italic(D)*")"))
xlims <- list(c(0, 5), c(1, 689), c(1, 689), c(0, 20), c(0, 20))
group_colors <- c("0" = "black", "1" = "#0072B2", "2" = "#D55E00", "3" = "#009E73", "4" = "#CC79A7", "5" = "#EDDF1D")
for(i in 1:ncol(main_simulations$priors)){
  prior <- data.frame(value = density(main_simulations$priors[, i])$x, density = density(main_simulations$priors[, i])$y, id = 0, lt = 1)
  posterior <- do.call(rbind, lapply(1:length(abc_nn_predictions), function(x){
    data.frame(value = density(abc_nn_predictions[[x]]$adj.values[, i])$x, density = density(abc_nn_predictions[[x]]$adj.values[, i])$y, id = x, lt = 0)
  }))
  combined <- rbind(posterior, prior)
  plot <- ggplot(combined) + geom_line(mapping = aes(x = value, y = density, group = id, color = as.factor(id), linetype = as.factor(lt))) + 
    labs(x = xlabs[[i]], y = "Density") + xlim(xlims[[i]]) + 
    theme_linedraw() +
    scale_color_manual(values = group_colors, name = NULL, labels = c("Prior", tol_labels), guide = guide_legend(override.aes = list(linetype = c(2, 1, 1, 1, 1, 1)))) + 
    guides(linetype = "none")
  assign(names(main_simulations$priors)[i], plot)
}

#combine plots and print to file
png("analysis/abc/abc_nn_posteriors.png", units = "in", width = 11, height = 7, res = 300)
legend <- get_legend(complexity)
plot_grid(complexity + theme(legend.position="none"),
          copy_radius + theme(legend.position="none"),
          dist_radius + theme(legend.position="none"),
          copy_strength + theme(legend.position="none"),
          dist_strength + theme(legend.position="none"),
          legend, labels = c("A", "B", "C", "D", "E"), align = "hv")
dev.off()
