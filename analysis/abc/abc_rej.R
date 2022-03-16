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
library(umap)

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

#convert summary statistics to two dimensional umap embedding
config <- umap.defaults
config$umap_learn_args <- 0.4 #set umap version
sum_stats_umap <- umap(sum_stats, method = "umap-learn", config = config)
obs_stats_umap <- predict(sum_stats_umap, as.matrix(obs_stats))

#save umap embedding
abc_umap <- list(sum_stats_umap = sum_stats_umap, obs_stats_umap = obs_stats_umap)
save(abc_umap, file = "analysis/abc/abc_umap.RData")

#load umap embedding
load("analysis/abc/abc_umap.RData")

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
abc_rej_cv <- cv4abc(param = main_simulations$priors, sumstat = abc_umap$sum_stats_umap$layout, method = "rejection", nval = 100,
                     tols = tols, transf = c("log", "logit", "logit", "log", "log"), logit.bounds = logit_bounds)
save(abc_rej_cv, file = "analysis/abc/abc_rej_cv.RData")

#load cross validation
load("analysis/abc/abc_rej_cv.RData")

#run full rejection ABC analysis at each tolerance level
abc_rej_predictions <- list()
abc_rej_predictions <- mclapply(1:length(tols), function(x){
  abc(target = abc_umap$obs_stats_umap, param = main_simulations$priors, sumstat = abc_umap$sum_stats_umap$layout, method = "rejection",
      tol = tols[x], transf = c("log", "logit", "logit", "log", "log"), logit.bounds = logit_bounds)
}, mc.cores = 5)
names(abc_rej_predictions) <- tols
save(abc_rej_predictions, file = "analysis/abc/abc_rej_predictions.RData")

#load predictions
load("analysis/abc/abc_rej_predictions.RData")

#plot
xlabs <- list(expression(Complexity~"("*italic(paste(lambda))*")"), expression(Copy~Radius~"("*italic(R[C])*")"), expression(Dist~Radius~"("*italic(R[D])*")"), expression(Copy~Strength~"("*italic(C)*")"), expression(Dist~Strength~"("*italic(D)*")"))
xlims <- list(c(0, 25), c(1, 689), c(1, 689), c(0, 8), c(0, 8))
group_colors <- c("0" = "black", "1" = "#0072B2", "2" = "#D55E00", "3" = "#009E73", "4" = "#CC79A7", "5" = "#EDDF1D")
for(i in 1:ncol(main_simulations$priors)){
  prior <- data.frame(value = density(main_simulations$priors[, i])$x, density = density(main_simulations$priors[, i])$y, id = 0, lt = 1)
  posterior <- do.call(rbind, lapply(1:length(abc_rej_predictions), function(x){
    data.frame(value = density(abc_rej_predictions[[x]]$unadj.values[, i])$x, density = density(abc_rej_predictions[[x]]$unadj.values[, i])$y, id = x, lt = 0)
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
png("analysis/abc/abc_rej_posteriors.png", units = "in", width = 11, height = 7, res = 300)
legend <- get_legend(complexity)
plot_grid(complexity + theme(legend.position="none"),
          copy_radius + theme(legend.position="none"),
          dist_radius + theme(legend.position="none"),
          copy_strength + theme(legend.position="none"),
          dist_strength + theme(legend.position="none"),
          legend, labels = c("A", "B", "C", "D", "E"), align = "hv")
dev.off()






