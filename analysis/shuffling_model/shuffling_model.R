#load libraries and set working directory
library(data.table)
library(parallel)
library(lme4)
library(ggplot2)
library(cowplot)
setwd("~/Documents/Work/Summer_2021/Cattle_Brands/CattleBrandABM")

#load files
load("components.RData")
load("analysis/shuffling_model/subsets.RData")

#create shuffling model function
shuffling_model <- function(matrix, n_components, total_components){
  if(n_components == 2){
    #subset matrix to only contain first two columns
    real_matrix <- matrix[, 1:2]
    
    #reorder components within rows so order matters
    real_matrix <- do.call(rbind, lapply(1:nrow(real_matrix), function(x){sort(real_matrix[x, ])}))
    
    #simulate all possible brands given all components in the dataset
    sim_matrix <- expand.grid(1:total_components, 1:total_components)
    
    #reorder components within rows so order matters, and then remove duplicated rows
    sim_matrix <- do.call(rbind, lapply(1:nrow(sim_matrix), function(x){sort(as.numeric(sim_matrix[x, ]))}))
    sim_matrix <- sim_matrix[-which(duplicated(sim_matrix)), ]
    
    #iterate through simulated brands
    output <- lapply(1:nrow(sim_matrix), function(x){
      #store components from simulated brand
      components <- sim_matrix[x, ]
      
      #if any of those components don't exist in the matrix, then assign a zero for S, otherwise, run the shuffling model
      if(length(which(components %in% c(real_matrix))) < n_components){
        return(data.frame(S = 0, exists = 0, brand = paste0(components[1], ", ", components[2])))
      } else{
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
      }
    })
  }
  
  if(n_components == 3){
    #subset matrix to only contain first two columns
    real_matrix <- matrix[, 1:3]
    
    #reorder components within rows so order matters
    real_matrix <- do.call(rbind, lapply(1:nrow(real_matrix), function(x){sort(real_matrix[x, ])}))
    
    #simulate all possible brands given the components in subset
    sim_matrix <- expand.grid(1:total_components, 1:total_components, 1:total_components)
    
    #reorder components within rows so order matters, and then remove duplicated rows
    sim_matrix <- do.call(rbind, lapply(1:nrow(sim_matrix), function(x){sort(as.numeric(sim_matrix[x, ]))}))
    sim_matrix <- sim_matrix[-which(duplicated(sim_matrix)), ]
    
    #iterate through simulated brands
    output <- lapply(1:nrow(sim_matrix), function(x){
      #store components from simulated brand
      components <- sim_matrix[x, ]
      
      #if any of those components don't exist in the matrix, then assign a zero for S, otherwise, run the shuffling model
      if(length(which(components %in% c(real_matrix))) < n_components){
        return(data.frame(S = 0, exists = 0, brand = paste0(components[1], ", ", components[2], ", ", components[3])))
      } else{
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
      }
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
  output <- shuffling_model(subsets[[i]], as.numeric(split[3]), nrow(components))
  
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
save(shuffling_data, file = "analysis/shuffling_model/shuffling_data.RData")

# PLOT --------------------------------------------------------------------

#load data
load("analysis/shuffling_model/shuffling_data.RData")

#subset to two-component brands
shuffling_data <- shuffling_data[which(shuffling_data$complexity == 2), ]

#subset to Y-SE
shuffling_data <- shuffling_data[which(shuffling_data$time == "y" & shuffling_data$space == "se"), ]

#extract two components
split <- strsplit(as.character(shuffling_data$brand), ", ")
shuffling_data$x <- as.numeric(sapply(1:length(split), function(x){split[[x]][1]}))
shuffling_data$y <- as.numeric(sapply(1:length(split), function(x){split[[x]][2]}))

#get rid of diagonal
shuffling_data <- shuffling_data[-which(sapply(1:nrow(shuffling_data), function(x){shuffling_data$x[x] == shuffling_data$y[x]})), ]

#store whether each is a true/false positive/negative
shuffling_data$result <- NA
shuffling_data$result[which(shuffling_data$actual == 1 & shuffling_data$prob_score >= 1)] <- 1 #true positive
shuffling_data$result[which(shuffling_data$actual == 0 & shuffling_data$prob_score < 1)] <- 2 #true negative
shuffling_data$result[which(shuffling_data$actual == 0 & shuffling_data$prob_score >= 1)] <- 3 #false positive
shuffling_data$result[which(shuffling_data$actual == 1 & shuffling_data$prob_score < 1)] <- 4 #false negative

#subset into mixed and actual brands
shuffling_data_unmixed <- shuffling_data[which(shuffling_data$mixed == FALSE), ]
shuffling_data_mixed <- shuffling_data[which(shuffling_data$mixed == TRUE), ]

#create two plots
colors <- c("#009E73", "#56B4E9", "#D55E00", "#F0E442")
labels <- c("True +", "True -", "False +", "False -")
unmixed_plot <- ggplot(shuffling_data_unmixed, aes(x = y, y = x, fill = as.factor(result))) + 
  scale_fill_manual(values = colors, labels = labels) + scale_y_reverse(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + geom_tile() + theme_void() + 
  theme(legend.title = element_blank(), axis.title.x = element_text(margin = margin(t = 3)), axis.title.y = element_text(angle = 90, margin = margin(r = 3)), panel.border = element_rect(fill = NA), plot.margin = margin(0, 1, 1, 1, "mm"), plot.title = element_text(face = "bold")) + xlab("Component A") + ylab("Component B") + ggtitle("Structured")
mixed_plot <- ggplot(shuffling_data_mixed, aes(x = y, y = x, fill = as.factor(result))) + 
  scale_fill_manual(values = colors, labels = labels) + scale_y_reverse(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + geom_tile() + theme_void() +
  theme(legend.title = element_blank(), axis.title.x = element_text(margin = margin(t = 3)), axis.title.y = element_text(angle = 90, margin = margin(r = 3)), panel.border = element_rect(fill = NA), plot.margin = margin(0, 1, 1, 1, "mm"), plot.title = element_text(face = "bold")) + xlab("Component A") + ylab(" ") + ggtitle("Mixed")

#combine plots and print to file
png("analysis/shuffling_model/shuffling_model_example.png", units = "in", width = 6, height = 2.8, res = 300)
plot_grid(unmixed_plot + theme(legend.position = "none"),
          mixed_plot + theme(legend.position = "none"),
          get_legend(unmixed_plot),
          rel_widths = c(3, 3, 1), nrow = 1)
dev.off()
