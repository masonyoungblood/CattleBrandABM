#NOTES
#one peculiar part of this code is that, when data is integer (so for the 
  #components) instead of doing table(factor(levels)) to account for all entries
  #and make sure everything lines up we are using
  #table(c(all, actual))[subtraction and trimming] because it is faster
#we are also storing unique(stuff) as a separate variable each time because
  #it ends up being faster

#faster version of %in%: https://stackoverflow.com/questions/32934933/faster-in-operator
`%fin%` <- function(x, table){
  fastmatch::fmatch(x, table, nomatch = 0) > 0
}

#brand generator, which is called within the cattle brand ABM
brand_generator <- function(brands, components, zip, zip_dists, rot_prob, complexity, copy_radius, copy_strength, dist_radius, dist_strength, angles = FALSE){
  #get rows with zips that are within the copy and dist radii
  copy_rows <- which(brands[, 9] %fin% as.numeric(names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= copy_radius))))
  dist_rows <- which(brands[, 9] %fin% as.numeric(names(which(zip_dists[, which(colnames(zip_dists) == zip)] <= dist_radius))))
  
  #generate frequencies of components within the copy radius, where missing components are assigned 0
  copy_components <- (as.numeric(Rfast::Table(c(0:nrow(components), brands[copy_rows, 1:4])))-1)[-1]
  
  #generate frequencies of components within the dist radius, where missing components are assigned 0
  dist_components <- (as.numeric(Rfast::Table(c(0:nrow(components), brands[dist_rows, 1:4])))-1)[-1]
  
  #sample up to four components, weighted by the copy and dist probability function, where the number of components is drawn from a poisson distribution with a lambda equal to "complexity"
  sim_brand <- sample(1:nrow(components), sample(1:4, 1, prob = dpois(c(1, 2, 3, 4), complexity)), replace = TRUE, prob = ((copy_components+1)^copy_strength)*((1/(dist_components+1))^dist_strength))

  #if angles are to be considered
  if(angles){
    #generate frequencies of angles within the copy radius, where missing angles are assigned 0
    copy_angles <- (as.numeric(Rfast::Table(c(0:9, brands[copy_rows, 5:8])))-1)[-1]
    
    #generate frequencies of angles within the copy radius, where missing angles are assigned 0
    dist_angles <- (as.numeric(Rfast::Table(c(0:9, brands[dist_rows, 5:8])))-1)[-1]
    
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
    if(i %fin% c(sampling_years - init_year)){
      #if angles are not considered, then generate component frequencies without them (and generate brand frequencies with respective columns selected)
      if(!angles){
        #get component frequencies
        comp_freqs <- as.numeric(sort(Rfast::Table(c(running_brands[, 1:4])[which(c(running_brands[, 1:4]) > 0)]), decreasing = TRUE))
        
        #get brand frequencies (for now just combining them as a string)
        all_brands <- sapply(1:nrow(running_brands), function(x){paste(running_brands[x, 1:4], collapse = " ")})
        unique_brands <- unique(all_brands)
        brand_freqs <- as.numeric(sort(Rfast::Table(all_brands), decreasing = TRUE))
        
        #construct components by zip codes matrix
        comp_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          (as.numeric(Rfast::Table(c(0:nrow(components), running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4])))-1)[-1]
        }))
        if(length(which(Rfast::colsums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[, -which(Rfast::colsums(comp_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[-which(Rfast::rowsums(comp_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #get unique counties to save processing time
        unique_counties <- unique(all_zips$county)
        
        #construct components by counties matrix
        comp_beta_county_mat <- t(sapply(1:length(unique_counties), function(x){
          (as.numeric(Rfast::Table(c(0:nrow(components), running_brands[which(running_brands[, 9] %fin% all_zips$zip[which(all_zips$county == unique_counties[x])]), 1:4])))-1)[-1]
        }))
        if(length(which(Rfast::colsums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[, -which(Rfast::colsums(comp_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[-which(Rfast::rowsums(comp_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by zip codes matrix
        brand_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4], ncol = 4) #construct temp matrix of brands in the corresponding zip code
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique_brands))) #return frequency vector
          } else{ #if not
            rep(0, length(unique_brands)) #return same number of zeros
          }
        }))
        if(length(which(Rfast::colsums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[, -which(Rfast::colsums(brand_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[-which(Rfast::rowsums(brand_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by counties matrix
        brand_beta_county_mat <- t(sapply(1:length(unique_counties), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] %fin% all_zips$zip[which(all_zips$county == unique_counties[x])]), 1:4], ncol = 4) #construct temp matrix of brands in the corresponding county
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique_brands))) #return frequency vector
          } else{ #if not
            rep(0, length(unique_brands)) #return same number of zeros
          }
        }))
        if(length(which(Rfast::colsums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[, -which(Rfast::colsums(brand_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[-which(Rfast::rowsums(brand_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #remove temporary variables
        rm(list = c("all_brands", "unique_brands", "unique_counties"))
      }
      
      #if angles are considered, then generate component frequencies with them
      if(angles){
        #extract combinations of components and angles (for now just combining them as a string and remove "0 0")
        comp_angle_combos <- c(sapply(1:nrow(running_brands), function(x){paste(c(running_brands[x, 1:4]), c(running_brands[x, 5:8]))}))
        unique_comp_angle_combos <- unique(comp_angle_combos) #run this before the next line, because "0 0" needs to be accounted for later
        comp_angle_combos <- comp_angle_combos[-which(comp_angle_combos == "0 0")]
        
        #get component frequencies (where rotated components are treated as unique)
        comp_freqs <- as.numeric(sort(Rfast::Table(comp_angle_combos), decreasing = TRUE))
        
        #get brand frequencies (for now just combining them as a string)
        all_brands <- sapply(1:nrow(running_brands), function(x){paste(running_brands[x, 1:8], collapse = " ")})
        unique_brands <- unique(all_brands)
        brand_freqs <- as.numeric(sort(Rfast::Table(all_brands), decreasing = TRUE))
        
        #construct components by zip codes matrix
        comp_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          (as.numeric(Rfast::Table(c(unique_comp_angle_combos, paste(c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4]), c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 5:8])))))-1)[-1]
        }))
        if(length(which(Rfast::colsums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[, -which(Rfast::colsums(comp_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(comp_beta_zip_mat) == 0)) > 0){
          comp_beta_zip_mat <- comp_beta_zip_mat[-which(Rfast::rowsums(comp_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #get unique counties to save processing time
        unique_counties <- unique(all_zips$county)
        
        #construct components by counties matrix
        comp_beta_county_mat <- t(sapply(1:length(unique_counties), function(x){
          (as.numeric(Rfast::Table(c(unique_comp_angle_combos, paste(c(running_brands[which(running_brands[, 9] %fin% all_zips$zip[which(all_zips$county == unique_counties[x])]), 1:4]), c(running_brands[which(running_brands[, 9] %fin% all_zips$zip[which(all_zips$county == unique_counties[x])]), 5:8])))))-1)[-1]
        }))
        if(length(which(Rfast::colsums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[, -which(Rfast::colsums(comp_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(comp_beta_county_mat) == 0)) > 0){
          comp_beta_county_mat <- comp_beta_county_mat[-which(Rfast::rowsums(comp_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by zip codes matrix
        brand_beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:8], ncol = 8) #construct temp matrix of brands in the corresponding zip code
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique_brands))) #return frequency vector
          } else{ #if not
            rep(0, length(unique_brands)) #return same number of zeros
          }
        }))
        if(length(which(Rfast::colsums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[, -which(Rfast::colsums(brand_beta_zip_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(brand_beta_zip_mat) == 0)) > 0){
          brand_beta_zip_mat <- brand_beta_zip_mat[-which(Rfast::rowsums(brand_beta_zip_mat) == 0), ] #remove empty rows
        }
        
        #construct brands by counties matrix
        brand_beta_county_mat <- t(sapply(1:length(unique_counties), function(x){
          temp <- matrix(running_brands[which(running_brands[, 9] %fin% all_zips$zip[which(all_zips$county == unique_counties[x])]), 1:8], ncol = 8) #construct temp matrix of brands in the corresponding county
          if(nrow(temp) > 0){ #if brands exist in that location
            as.numeric(table(factor(sapply(1:nrow(temp), function(x){paste(temp[x, ], collapse = " ")}), levels = unique_brands))) #return frequency vector
          } else{ #if not
            rep(0, length(unique_brands)) #return same number of zeros
          }
        }))
        if(length(which(Rfast::colsums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[, -which(Rfast::colsums(brand_beta_county_mat) == 0)] #remove empty columns
        }
        if(length(which(Rfast::rowsums(brand_beta_county_mat) == 0)) > 0){
          brand_beta_county_mat <- brand_beta_county_mat[-which(Rfast::rowsums(brand_beta_county_mat) == 0), ] #remove empty rows
        }
        
        #remove temporary variables
        rm(list = c("comp_angle_combos", "unique_comp_angle_combos", "all_brands", "unique_brands", "unique_counties"))
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
