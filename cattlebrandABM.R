#brand generator, which is called within the cattle brand ABM
brand_generator <- function(brands, components, zip, zip_dists, rot_prob, complexity, copy_radius, copy_strength, dist_radius, dist_strength, angles = FALSE){
  #generate frequencies of components within the copy radius, where missing components are assigned 0
  copy_components <- as.numeric(table(factor(c(brands[which(brands[, 9] %in% names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= copy_radius))), 1:4]), levels = 1:nrow(components))))
  
  #generate frequencies of components within the dist radius, where missing components are assigned 0
  dist_components <- as.numeric(table(factor(c(brands[which(brands[, 9] %in% names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= dist_radius))), 1:4]), levels = 1:nrow(components))))
  
  #sample up to four components, weighted by the copy and dist probability function, where the number of components is drawn from a poisson distribution with a lambda equal to "complexity"
  sim_brand <- sample(1:nrow(components), sample(1:4, 1, prob = dpois(c(1, 2, 3, 4), complexity)), replace = TRUE, prob = ((copy_components+1)^copy_strength)*((1/(dist_components+1))^dist_strength))

  #if angles are to be considered
  if(angles){
    #generate frequencies of components within the copy radius, where missing components are assigned 0
    copy_angles <- as.numeric(table(factor(c(brands[which(brands[, 9] %in% names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= copy_radius))), 5:8]), levels = 1:9)))
    
    #generate frequencies of components within the copy radius, where missing components are assigned 0
    dist_angles <- as.numeric(table(factor(c(brands[which(brands[, 9] %in% names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= dist_radius))), 5:8]), levels = 1:9)))
    
    #generate empty vector to fill with angles
    sim_angles <- rep(0, 4)
    
    #determine which components will be rotated
    to_rot <- which(components$rotatable[sim_brand] & sample(c(TRUE, FALSE), length(sim_brand), replace = TRUE, prob = c(rot_prob, 1-rot_prob)))
    
    #rotate components
    sim_angles[to_rot] <- sample(1:9, length(to_rot), replace = TRUE, prob = ((copy_angles+1)^copy_strength)*((1/(dist_angles+1))^dist_strength))
  }
  
  #return simulated brand
  if(!angles){
    #return the brand with empty components filled with zeros and with empty angles
    return(c(c(sim_brand, rep(0, 4-length(sim_brand))), rep(0, 4)))
  } else{
    #return the brands with empty components filled with zeros and angles
    return(c(c(sim_brand, rep(0, 4-length(sim_brand))), sim_angles))
  }
}

#cattle brand ABM, which is basically an iterating wrapper for the brand generator
cattlebrandABM <- function(init_brands, components, all_zips, zip_dists, init_year, sampling_years, n_new, n_old, rot_prob, complexity, copy_radius, copy_strength, dist_radius, dist_strength, angles = TRUE){
  #get number of timesteps
  t_steps <- max(sampling_years) - init_year
  
  #create empty output matrix to store the summary statistics (columns) from each sampling year (rows)
  output <- matrix(NA, nrow = length(sampling_years), ncol = 16)
  
  running_brands <- init_brands
  
  for(i in 1:t_steps){
    #generate zip codes prior to generating brands (to iterate through), weighted by the frequency of those zip codes in the overall dataset
    new_zips <- sample(all_zips$zip, n_new, replace = TRUE, prob = all_zips$freq)
    
    #generate new brands
    new_brands <- t(sapply(1:n_new, function(x){brand_generator(brands = running_brands, components = components, zip = new_zips[x], zip_dists = zip_dists, rot_prob = rot_prob, complexity = complexity, copy_radius = copy_radius, copy_strength = copy_strength, dist_radius = dist_radius, dist_strength = dist_strength, angles = angles)}))
    new_brands <- cbind(new_brands, new_zips)
    
    running_brands <- rbind(running_brands, new_brands)
    
    #randomly remove N overall brands
    running_brands <- running_brands[-sample(1:nrow(running_brands), n_old), ]
    
    #if it is a sampling year then calculate frequency distributions and summary statistics
    if(i %in% c(sampling_years - init_year)){
      #if angles are not considered, then generate component frequencies without them (and generate brand frequencies with respective columns selected)
      if(!angles){
        #get component frequencies
        comp_freqs <- as.numeric(sort(table(c(running_brands[, 1:4])[which(c(running_brands[, 1:4]) > 0)]), decreasing = TRUE))
        
        #get brand frequencies (for now just combining them as a string)
        all_brands <- sapply(1:nrow(running_brands), function(x){paste(running_brands[x, 1:4], collapse = " ")})
        brand_freqs <- as.numeric(sort(table(all_brands), decreasing = TRUE))
        
        #construct components by zip codes matrix
        comp_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          as.numeric(table(factor(c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4]), levels = 1:nrow(components))))
        }))
        if(length(which(colSums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[, -which(colSums(comp_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[-which(rowSums(comp_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct components by counties matrix
        comp_beta_county_mat <- t(sapply(1:length(unique(all_zips$county)), function(x){
          as.numeric(table(factor(c(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 1:4]), levels = 1:nrow(components))))
        }))
        if(length(which(colSums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[, -which(colSums(comp_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[-which(rowSums(comp_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by zip codes matrix
        brand_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4], ncol = 4) #construct temp matrix of brands in the corresponding zip code
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique(all_brands)))) #return frequency vector
          } else{ #if not
            rep(0, length(unique(all_brands))) #return same number of zeros
          }
        }))
        if(length(which(colSums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[, -which(colSums(brand_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[-which(rowSums(brand_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by counties matrix
        brand_beta_county_mat <- t(sapply(1:length(unique(all_zips$county)), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 1:4], ncol = 4) #construct temp matrix of brands in the corresponding county
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique(all_brands)))) #return frequency vector
          } else{ #if not
            rep(0, length(unique(all_brands))) #return same number of zeros
          }
        }))
        if(length(which(colSums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[, -which(colSums(brand_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[-which(rowSums(brand_beta_county_mat) == 0), ] #remove empty rows
        }
      }
      
      #if angles are considered, then generate component frequencies with them
      if(angles){
        #extract combinations of components and angles (for now just combining them as a string and remove "0 0")
        comp_angle_combos <- c(sapply(1:nrow(running_brands), function(x){paste(c(running_brands[x, 1:4]), c(running_brands[x, 5:8]))}))
        comp_angle_combos <- comp_angle_combos[-which(comp_angle_combos == "0 0")]
        
        #get component frequencies (where rotated components are treated as unique)
        comp_freqs <- as.numeric(sort(table(comp_angle_combos), decreasing = TRUE))
        
        #get brand frequencies (for now just combining them as a string)
        all_brands <- sapply(1:nrow(running_brands), function(x){paste(running_brands[x, 1:8], collapse = " ")})
        brand_freqs <- as.numeric(sort(table(all_brands), decreasing = TRUE))
        
        #construct components by zip codes matrix
        comp_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          as.numeric(table(factor(paste(c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4]), c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 5:8])), levels = unique(comp_angle_combos))))
        }))
        if(length(which(colSums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[, -which(colSums(comp_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[-which(rowSums(comp_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct components by counties matrix
        comp_beta_county_mat <- t(sapply(1:length(unique(all_zips$county)), function(x){
          as.numeric(table(factor(paste(c(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 1:4]), c(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 5:8])), levels = unique(comp_angle_combos))))
        }))
        if(length(which(colSums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[, -which(colSums(comp_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[-which(rowSums(comp_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by zip codes matrix
        brand_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:8], ncol = 8) #construct temp matrix of brands in the corresponding zip code
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique(all_brands)))) #return frequency vector
          } else{ #if not
            rep(0, length(unique(all_brands))) #return same number of zeros
          }
        }))
        if(length(which(colSums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[, -which(colSums(brand_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[-which(rowSums(brand_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by counties matrix
        brand_beta_county_mat <- t(sapply(1:length(unique(all_zips$county)), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 1:8], ncol = 8) #construct temp matrix of brands in the corresponding county
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique(all_brands)))) #return frequency vector
          } else{ #if not
            rep(0, length(unique(all_brands))) #return same number of zeros
          }
        }))
        if(length(which(colSums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[, -which(colSums(brand_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(rowSums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[-which(rowSums(brand_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #remove temporary variables
        rm(list = c("comp_angle_combos", "all_brands"))
      }
      
      #calculate component summary statistics
      comp_sum_stats <- c(max(comp_freqs)/sum(comp_freqs), #proportion of the most common component
                          min(comp_freqs)/sum(comp_freqs), #proportion of the least common component
                          hillR::hill_taxa(comp_freqs, q = 1), #shannon's diversity index
                          hillR::hill_taxa(comp_freqs, q = 2), #simpson's diversity index
                          #exp(summary(nls(y ~ SSasymp(x, yf, y0, log_alpha), data = data.frame(y = comp_freqs, x = 1:length(comp_freqs))))$coefficients[3]), #rate constant of exponential decay curve (singularity issues)
                          hillR::hill_taxa_parti(comp_beta_zip_mat, q = 0)$region_similarity, #jaccard index (zip codes)
                          hillR::hill_taxa_parti(comp_beta_zip_mat, q = 2)$local_similarity, #morisita-horn index (zip codes)
                          hillR::hill_taxa_parti(comp_beta_county_mat, q = 0)$region_similarity, #jaccard index (counties)
                          hillR::hill_taxa_parti(comp_beta_county_mat, q = 2)$local_similarity) #morisita-horn index (counties)
      
      #calculate brand summary statistics
      brand_sum_stats <- c(max(brand_freqs)/sum(brand_freqs), #proportion of the most common brand
                           min(brand_freqs)/sum(brand_freqs), #proportion of the least common brand
                           hillR::hill_taxa(brand_freqs, q = 1), #shannon's diversity index
                           hillR::hill_taxa(brand_freqs, q = 2), #simpson's diversity index
                           #exp(summary(nls(y ~ SSasymp(x, yf, y0, log_alpha), data = data.frame(y = brand_freqs, x = 1:length(brand_freqs))))$coefficients[3]), #rate constant of exponential decay curve (singularity issues)
                           hillR::hill_taxa_parti(brand_beta_zip_mat, q = 0)$region_similarity, #jaccard index (zip codes)
                           hillR::hill_taxa_parti(brand_beta_zip_mat, q = 2)$local_similarity, #morisita-horn index (zip codes)
                           hillR::hill_taxa_parti(brand_beta_county_mat, q = 0)$region_similarity, #jaccard index (counties)
                           hillR::hill_taxa_parti(brand_beta_county_mat, q = 2)$local_similarity) #morisita-horn index (counties)
      
      #store summary statistics in the output matrix
      output[match(i, sampling_years - init_year), ] <- c(comp_sum_stats, brand_sum_stats)
      
      #remove temporary objects
      rm(list = c("comp_freqs", "brand_freqs", "comp_beta_zip_mat", "comp_beta_county_mat", "comp_sum_stats", "brand_sum_stats"))
    }
    
    #remove temporary objects
    rm(list = c("new_zips", "new_brands"))
  }
  
  return(output)
}
