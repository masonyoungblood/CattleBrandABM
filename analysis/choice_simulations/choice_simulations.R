#set working directory, load data, source code
setwd(system("pwd", intern = T))
load("converted_brands.RData")
load("components.RData")
load("location_data/all_zips.RData")
load("location_data/zip_dists.RData")
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

#get minimum and maximum distances in data
min_dist <- ceiling(min(zip_dists[which(zip_dists != 0)]))
max_dist <- ceiling(max(zip_dists))

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

#run simulations, 
for(i in 1:3){
  #number of simulations
  n_sim <- 10000
  
  #set null priors
  if(i == 1){priors <- data.frame(complexity = rgamma(n_sim, shape = 0.9, rate = 0.2), copy_radius = rep(max_dist, n_sim), dist_radius = rep(max_dist, n_sim), copy_strength = 0, dist_strength = 0)}
  
  #set copy priors
  if(i == 2){priors <- data.frame(complexity = rgamma(n_sim, shape = 0.9, rate = 0.2), copy_radius = rep(max_dist, n_sim), dist_radius = rep(max_dist, n_sim), copy_strength = 1, dist_strength = 0)}
  
  #set dist priors
  if(i == 3){priors <- data.frame(complexity = rgamma(n_sim, shape = 0.9, rate = 0.2), copy_radius = rep(max_dist, n_sim), dist_radius = rep(max_dist, n_sim), copy_strength = 0, dist_strength = 1)}
  
  #run simulations without angles
  slurm <- rslurm::slurm_apply(cattlebrandABM_slurm, priors, jobname = "main",
                               nodes = 4, cpus_per_node = 25, pkgs = pkgs, global_objects = objects())
  
  #get output and clean files
  sum_stats <- rslurm::get_slurm_out(slurm)
  rslurm::cleanup_files(slurm)
  
  #save output
  choice_simulations <- list(priors = priors, sum_stats = sum_stats)
  save(choice_simulations, file = paste0("choice_simulations_", i, ".RData"))
  
  #remove temporary objects
  rm(list = c("priors", "slurm", "sum_stats", "choice_simulations"))
}
