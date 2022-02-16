#set working directory, load data, etc.
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
load("analysis/prior_simulations.RData")
load("analysis/prior_simulations_angles.RData")
source("cattlebrandABM.R")

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
sum_stats_a <- do.call("rbind", lapply(1:length(prior_simulations$sum_stats), function(x){c(t(as.matrix(prior_simulations$sum_stats[[x]])))}))
sum_stats_b <- do.call("rbind", lapply(1:length(prior_simulations_angles$sum_stats), function(x){c(t(as.matrix(prior_simulations_angles$sum_stats[[x]])))}))

#do pca
pca <- prcomp(rbind(sum_stats_a, sum_stats_b), scale = TRUE)
predicted <- predict(pca, data.frame(t(c(obs_sum_stats))))

#opacity
opacity <- 0.03

#create plot
png("analysis/prior_simulations.png", units = "in", width = 4.5, height = 4.5, res = 300)
par(mar = c(4.5, 4.5, 1, 1))
plot(pca$x[,1], pca$x[,2], col = c(rep(scales::alpha("blue", opacity), nrow(sum_stats_a)), rep(scales::alpha("red", opacity), nrow(sum_stats_a))), pch = 16, xlab = "PC1", ylab = "PC2")
points(predicted[,1], predicted[,2], col = "black", pch = 8, cex = 4)
legend("topleft", c("without angles", "with angles"), col = c("blue", "red"), pch = 16, pt.cex = 1, cex = 0.8)
dev.off()


