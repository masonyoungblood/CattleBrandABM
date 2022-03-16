#set working directory, load data, etc.
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
library(umap)
library(ggplot2)
library(cowplot)
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
source("cattlebrandABM.R")

#load simulations
load("analysis/choice_simulations/choice_simulations_1.RData")
null_simulations <- choice_simulations
load("analysis/choice_simulations/choice_simulations_2.RData")
copy_simulations <- choice_simulations
load("analysis/choice_simulations/choice_simulations_3.RData")
dist_simulations <- choice_simulations
rm(choice_simulations)

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
sum_stats_a <- do.call("rbind", lapply(1:length(null_simulations$sum_stats), function(x){c(t(as.matrix(null_simulations$sum_stats[[x]])))}))
sum_stats_b <- do.call("rbind", lapply(1:length(copy_simulations$sum_stats), function(x){c(t(as.matrix(copy_simulations$sum_stats[[x]])))}))
sum_stats_c <- do.call("rbind", lapply(1:length(dist_simulations$sum_stats), function(x){c(t(as.matrix(dist_simulations$sum_stats[[x]])))}))

#do pca
pca <- prcomp(rbind(sum_stats_a, sum_stats_b, sum_stats_c), scale = TRUE)
obs_predicted_pca <- predict(pca, data.frame(t(c(obs_sum_stats))))
#pca$sdev^2 #percentage of explained variance for each principal component

#do umap
config <- umap.defaults
config$umap_learn_args <- 0.4 #set umap version
umap <- umap(rbind(sum_stats_a, sum_stats_b, sum_stats_c), method = "umap-learn", config = config)
obs_predicted_umap <- predict(umap, t(as.matrix(obs_sum_stats)))

#opacity
opacity <- 0.03

#create pca plot
pca_plot_data <- as.data.frame(rbind(pca$x[, 1:2], t(as.matrix(obs_predicted_pca[1:2]))))
pca_plot_data$group <- factor(c(rep("Null", nrow(sum_stats_a)), rep("Copy", nrow(sum_stats_b)), rep("Dist", nrow(sum_stats_c)), "Obs"), levels = c("Null", "Copy", "Dist", "Obs"))
pca_plot_data$type <- as.factor(c(rep(0, nrow(sum_stats_a)), rep(0, nrow(sum_stats_b)), rep(0, nrow(sum_stats_c)), 1))
group_colors <- c(scales::alpha("#0072B2", opacity), scales::alpha("#D55E00", opacity), scales::alpha("#009E73", opacity), "black")
pca_plot <- ggplot(pca_plot_data, aes(x = PC1, y = PC2)) + geom_point(aes(color = group, shape = type, size = type)) + 
  scale_color_manual(values = group_colors, name = NULL, guide = guide_legend(override.aes = list(color = c("#0072B2", "#D55E00", "#009E73", "black"), shape = c(19, 19, 19, 17), size = c(1, 1, 1, 5)))) +
  scale_shape_manual(guide = "none", values = c(19, 17)) + scale_size_manual(guide = "none", values = c(1, 5)) +
  labs(x = "PC1", y = "PC2") + theme_linedraw()

#create umap plot
umap_plot_data <- as.data.frame(rbind(umap$layout, obs_predicted_umap))
umap_plot_data$group <- factor(c(rep("Null", nrow(sum_stats_a)), rep("Copy", nrow(sum_stats_b)), rep("Dist", nrow(sum_stats_c)), "Obs"), levels = c("Null", "Copy", "Dist", "Obs"))
umap_plot_data$type <- as.factor(c(rep(0, nrow(sum_stats_a)), rep(0, nrow(sum_stats_b)), rep(0, nrow(sum_stats_c)), 1))
group_colors <- c(scales::alpha("#0072B2", opacity), scales::alpha("#D55E00", opacity), scales::alpha("#009E73", opacity), "black")
umap_plot <- ggplot(umap_plot_data, aes(x = V1, y = V2)) + geom_point(aes(color = group, shape = type, size = type)) + 
  scale_color_manual(values = group_colors, name = NULL, guide = guide_legend(override.aes = list(color = c("#0072B2", "#D55E00", "#009E73", "black"), shape = c(19, 19, 19, 17), size = c(1, 1, 1, 5)))) +
  scale_shape_manual(guide = "none", values = c(19, 17)) + scale_size_manual(guide = "none", values = c(1, 5)) +
  labs(x = "UMAP1", y = "UMAP2") + theme_linedraw()

#save plot
png("analysis/choice_simulations/choice_simulations.png", units = "in", width = 8, height = 3.5, res = 300)
legend <- get_legend(pca_plot)
plot_grid(pca_plot + theme(legend.position="none"), umap_plot + theme(legend.position="none"),
          legend, labels = c("A", "B", NULL), align = "hv", nrow = 1, rel_widths = c(2, 2, 0.5))
dev.off()
