#set working directory, load data, source code
setwd(system("pwd", intern = T))
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
load("location_data/zip_dists.RData")
load("abc_rf_predictions.RData")
source("cattlebrandABM.R")

#random seed
set.seed(12345)

#probability of rotation (proportion of rotated brands)
rot_prob <- as.numeric(1-(table(brands[, 5:8])[1]/sum(table(brands[, 5:8]))))

#separate brands data by year
brands_1990 <- data.table::data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#average number of new brands and old brands per year
n_new <- mean(c((nrow(data.table::fsetdiff(brands_2008, brands_1990))/18),
                (nrow(data.table::fsetdiff(brands_2014, brands_2008))/6),
                nrow(data.table::fsetdiff(brands_2015, brands_2014)),
                nrow(data.table::fsetdiff(brands_2016, brands_2015))))
n_old <- mean(c((nrow(data.table::fsetdiff(brands_1990, brands_2008))/18),
                (nrow(data.table::fsetdiff(brands_2008, brands_2014))/6),
                nrow(data.table::fsetdiff(brands_2014, brands_2015)),
                nrow(data.table::fsetdiff(brands_2015, brands_2016))))

#number of simulations
n_sim <- 10000

#load simulation data
load("main_simulations_1.RData")
a <- main_simulations
load("main_simulations_2.RData")
b <- main_simulations
load("main_simulations_3.RData")
c <- main_simulations
load("main_simulations_4.RData")
d <- main_simulations
load("main_simulations_5.RData")
e <- main_simulations
main_simulations <- list(priors = rbind(a$priors, b$priors, c$priors, d$priors, e$priors), sum_stats = c(a$sum_stats, b$sum_stats, c$sum_stats, d$sum_stats, e$sum_stats))
rm(list = c("a", "b", "c", "d", "e"))

#store priors in new object for transformation
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

#generate posterior distributions to sample
posteriors <- list()
for(i in 1:ncol(transf_priors)){
  if(i %in% c(1, 4, 5)){
    temp <- density(exp(transf_priors[, i]), weights = abc_rf_predictions[[i]]$weights)
  } else{
    temp <- density(inv_logit(transf_priors[, i], logit_bounds = logit_bounds), weights = abc_rf_predictions[[i]]$weights)
  }
  posteriors[[i]] <- data.frame(value = temp$x, density = temp$y)
}
names(posteriors) <- names(main_simulations$priors)

#set priors by sampling from posteriors
priors <- data.frame(complexity = sample(posteriors$complexity$value, n_sim, replace = TRUE, prob = posteriors$complexity$density),
                     copy_radius = sample(posteriors$copy_radius$value, n_sim, replace = TRUE, prob = posteriors$copy_radius$density),
                     dist_radius = sample(posteriors$dist_radius$value, n_sim, replace = TRUE, prob = posteriors$dist_radius$density),
                     copy_strength = sample(posteriors$copy_strength$value, n_sim, replace = TRUE, prob = posteriors$copy_strength$density),
                     dist_strength = sample(posteriors$dist_strength$value, n_sim, replace = TRUE, prob = posteriors$dist_strength$density))

#wrap cattlebrandABM in a simpler function for slurm
cattlebrandABM_slurm <- function(complexity, copy_radius, dist_radius, copy_strength, dist_strength){
  #run full version of cattlebrandABM
  cattlebrandABM(init_brands = as.matrix(brands_1990), components, all_zips, zip_dists,
                 init_year = 1990, sampling_years = c(2008, 2014, 2015, 2016), n_new, n_old,
                 rot_prob, complexity = complexity, copy_radius = copy_radius,
                 copy_strength = copy_strength, dist_radius = dist_radius,
                 dist_strength = dist_strength, angles = FALSE, edit_dist_prop = 0.1)
}

#store required packages
pkgs <- unique(getParseData(parse("cattlebrandABM.R"))$text[getParseData(parse("cattlebrandABM.R"))$token == "SYMBOL_PACKAGE"])

#run simulations without angles
slurm <- rslurm::slurm_apply(cattlebrandABM_slurm, priors, jobname = "posteriors",
                             nodes = 4, cpus_per_node = 25, pkgs = pkgs, global_objects = objects())

#get output and clean files
sum_stats <- rslurm::get_slurm_out(slurm)
rslurm::cleanup_files(slurm)

#save output
posterior_simulations <- list(priors = priors, sum_stats = sum_stats)
save(posterior_simulations, file = "posterior_simulations.RData")
