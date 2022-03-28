library(data.table)
library(parallel)
library(lme4)
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")

#load files
load("analysis/shuffling_model/subsets.RData")

#create shuffling model function
shuffling_model <- function(matrix, n_components){
  if(n_components == 2){
    #subset matrix to only contain first two columns
    real_matrix <- matrix[, 1:2]
    
    #reorder components within rows so order matters
    #real_matrix <- do.call(rbind, lapply(1:nrow(real_matrix), function(x){sort(real_matrix[x, ])}))
    
    #simulate all possible brands given the components in subset
    sim_matrix <- expand.grid(sort(unique(c(real_matrix))), sort(unique(c(real_matrix))))
    
    #reorder components within rows so order matters
    #sim_matrix <- do.call(rbind, lapply(1:nrow(sim_matrix), function(x){sort(as.numeric(sim_matrix[x, ]))}))
    
    #iterate through simulated brands
    output <- lapply(1:nrow(sim_matrix), function(x){
      #store components from simulated brand
      components <- sim_matrix[x, ]
      
      #store whether simulated brand actually exists (1 = exists)
      exists <- prodlim::row.match(as.data.frame(matrix(components, nrow = 1)), as.data.frame(real_matrix), nomatch = 0)
      exists[which(exists > 1)] <- 1
      
      #if it exists, get number of times it exists
      if(exists > 0){
        n_exists <- length(which(sapply(1:nrow(real_matrix), function(y){identical(components, real_matrix[y, ])})))
        
        #get final term, which is total number in subset excluding the focal brand
        N <- nrow(real_matrix) - n_exists
      } else{
        #get final term, which is total number in subset excluding the focal brand
        N <- nrow(real_matrix)
      }
      
      #subset brands without component A
      wo_A <- real_matrix[-unique(which(real_matrix == components[1], arr.ind = TRUE)[, 1]), ]
      
      #subset brands without component B
      wo_B <- real_matrix[-unique(which(real_matrix == components[2], arr.ind = TRUE)[, 1]), ]
      
      #get proportions of each component among brands that don't have the other component
      F_A <- length(unique(which(wo_B == components[1], arr.ind = TRUE)[, 1]))/nrow(wo_B)
      F_B <- length(unique(which(wo_A == components[2], arr.ind = TRUE)[, 1]))/nrow(wo_A)
      
      #store predicted prevalence (count)
      S <- F_A*F_B*N
      
      return(data.frame(S = S, exists = exists, brand = paste0(components[1], ", ", components[2])))
    })
  }
  
  if(n_components == 3){
    #subset matrix to only contain first two columns
    real_matrix <- matrix[, 1:3]
    
    #reorder components within rows so order matters
    real_matrix <- do.call(rbind, lapply(1:nrow(real_matrix), function(x){sort(real_matrix[x, ])}))
    
    #simulate all possible brands given the components in subset
    sim_matrix <- expand.grid(sort(unique(c(real_matrix))), sort(unique(c(real_matrix))), sort(unique(c(real_matrix))))
    
    #reorder components within rows so order matters
    sim_matrix <- do.call(rbind, lapply(1:nrow(sim_matrix), function(x){sort(as.numeric(sim_matrix[x, ]))}))
    
    #iterate through simulated brands
    output <- lapply(1:nrow(sim_matrix), function(x){
      #store components from simulated brand
      components <- sim_matrix[x, ]
      
      #store whether simulated brand actually exists (1 = exists)
      exists <- prodlim::row.match(as.data.frame(matrix(components, nrow = 1)), as.data.frame(real_matrix), nomatch = 0)
      exists[which(exists > 1)] <- 1
      
      #if it exists, get number of times it exists
      if(exists > 0){
        n_exists <- length(which(sapply(1:nrow(real_matrix), function(y){identical(components, real_matrix[y, ])})))
        
        #get final term, which is total number in subset excluding the focal brand
        N <- nrow(real_matrix) - n_exists
      } else{
        #get final term, which is total number in subset excluding the focal brand
        N <- nrow(real_matrix)
      }
      
      #subset brands without component A or B
      wo_AB <- real_matrix[-unique(c(which(real_matrix == components[1], arr.ind = TRUE)[, 1], which(real_matrix == components[2], arr.ind = TRUE)[, 1])), ]
      
      #subset brands without component A or C
      wo_AC <- real_matrix[-unique(c(which(real_matrix == components[1], arr.ind = TRUE)[, 1], which(real_matrix == components[3], arr.ind = TRUE)[, 1])), ]
      
      #subset brands without component B or C
      wo_BC <- real_matrix[-unique(c(which(real_matrix == components[2], arr.ind = TRUE)[, 1], which(real_matrix == components[3], arr.ind = TRUE)[, 1])), ]
      
      #get proportions of each component among brands that don't have the other components
      F_A <- length(unique(which(wo_BC == components[1], arr.ind = TRUE)[, 1]))/nrow(wo_BC)
      F_B <- length(unique(which(wo_AC == components[2], arr.ind = TRUE)[, 1]))/nrow(wo_AC)
      F_C <- length(unique(which(wo_AB == components[3], arr.ind = TRUE)[, 1]))/nrow(wo_AB)
      
      #store predicted prevalence (count)
      S <- F_A*F_B*F_C*N
      
      return(data.frame(S = S, exists = exists, brand = paste0(components[1], ", ", components[2], ", ", components[3])))
    })
  }
  
  #return output
  return(do.call(rbind, output))
}

#detect cores
ncores <- detectCores() - 1

#go through files and run shuffling model
shuffling_data <- mclapply(1:length(subsets), function(i){
  #split name to extra information
  split <- strsplit(names(subsets)[i], "_")[[1]]
  
  #run shuffling model
  output <- shuffling_model(subsets[[i]], as.numeric(split[3]))
  
  #structure and return data frame
  result <- data.frame(time = rep(split[1], nrow(output)), space = rep(split[2], nrow(output)),
                       complexity = rep(as.numeric(split[3]), nrow(output)),
                       brand = output[, 3], mixed = rep(ifelse(length(split) == 4, TRUE, FALSE), nrow(output)),
                       actual = output[, 2], prob_score = output[, 1])
  return(result)
}, mc.cores = ncores)

#rbind all of the data
shuffling_data <- do.call(rbind, shuffling_data)

#save it
save(shuffling_data, file = "analysis/shuffling_model/shuffling_data_order.RData")
