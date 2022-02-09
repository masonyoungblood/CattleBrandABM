#NOTES
#one peculiar part of this code is that, when data is integer (so for the 
  #components) instead of doing table(factor(levels)) to account for all entries
  #and make sure everything lines up we are using
  #table(c(all, actual))[subtraction and trimming] because it is faster
#we are also storing unique(stuff) as a separate variable each time because
  #it ends up being faster
#adist (base R pairwise levenshtein function) is, counterintuitively, the most
  #efficient method... textTinyR has a much faster individual levenshtein
  #function, but it does not scale up as efficiently

#faster version of %in%: https://stackoverflow.com/questions/32934933/faster-in-operator
"%fin%" <- function(x, table){
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
    #generate frequencies of angles, nested for each component of the brand, within the copy radius, where missing angles are assigned 0
    #copy_angles <- (as.numeric(Rfast::Table(c(0:9, brands[copy_rows, 5:8])))-1)[-1]
    copy_angles <- sapply(sim_brand, function(x){(as.numeric(Rfast::Table(c(0:9, brands[copy_rows, 5:8][which(brands[copy_rows, 1:4] == x)])))-1)[-1]})
    
    #generate frequencies of angles, nested for each component of the brand, within the copy radius, where missing angles are assigned 0
    #dist_angles <- (as.numeric(Rfast::Table(c(0:9, brands[dist_rows, 5:8])))-1)[-1]
    dist_angles <- sapply(sim_brand, function(x){(as.numeric(Rfast::Table(c(0:9, brands[dist_rows, 5:8][which(brands[dist_rows, 1:4] == x)])))-1)[-1]})
    
    #generate empty vector to fill with angles
    sim_angles <- rep(0, 4)
    
    #determine which components will be rotated
    to_rot <- which(!is.na(components$rotatable)[sim_brand] & sample(c(TRUE, FALSE), length(sim_brand), replace = TRUE, prob = c(rot_prob, 1-rot_prob)))
    
    #if any components are to be rotated
    if(length(to_rot) > 0){
      for(m in to_rot){
        #if there is only one possible angle
        if(length(components$rotatable[[sim_brand[m]]]) == 1){
          #assign it
          sim_angles[m] <- components$rotatable[[sim_brand[m]]]
        } else{ #otherwise
          #rotate them separately, allowing for different angles of rotation for each - if a particular component can be rotated as 5, 7, and 9, then only the frequencies of those angles in the population are considered
          sim_angles[m] <- sample(components$rotatable[[sim_brand[m]]], 1, prob = ((copy_angles[, m][components$rotatable[[sim_brand[m]]]]+1)^copy_strength)*((1/(dist_angles[, m][components$rotatable[[sim_brand[m]]]]+1))^dist_strength))
        }
      }
    }
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
cattlebrandABM <- function(init_brands, components, all_zips, zip_dists, init_year, sampling_years, n_new, n_old, rot_prob, complexity, copy_radius, copy_strength, dist_radius, dist_strength, angles = TRUE, edit_dist_prop = 0){
  #get number of timesteps
  t_steps <- max(sampling_years) - init_year
  
  #create empty output matrix to store the summary statistics (columns) from each sampling year (rows)
  if(edit_dist_prop > 0){output <- matrix(NA, nrow = length(sampling_years), ncol = 9)}
  if(edit_dist_prop == 0){output <- matrix(NA, nrow = length(sampling_years), ncol = 8)}
  
  running_brands <- init_brands
  
  for(i in 1:t_steps){
    #generate zip codes prior to generating brands (to iterate through), weighted by the frequency of those zip codes in the overall dataset
    new_zips <- sample(all_zips$zip, n_new, replace = TRUE, prob = all_zips$freq)
    
    #generate new brands
    new_brands <- t(sapply(1:n_new, function(x){brand_generator(brands = running_brands, components = components, zip = new_zips[x], zip_dists = zip_dists, rot_prob = rot_prob, complexity = complexity, copy_radius = copy_radius, copy_strength = copy_strength, dist_radius = dist_radius, dist_strength = dist_strength, angles = angles)}))
    new_brands <- cbind(new_brands, new_zips)
    
    #add new brands to running brands
    running_brands <- rbind(running_brands, new_brands)
    
    #randomly remove N overall brands
    running_brands <- running_brands[-sample(1:nrow(running_brands), n_old), ]
    
    #if it is a sampling year then calculate frequency distributions and summary statistics
    if(i %fin% c(sampling_years - init_year)){
      #store summary statistics in the output matrix
      output[match(i, sampling_years - init_year), ] <- get_sum_stats(running_brands = running_brands, components = components, all_zips = all_zips, angles = angles, edit_dist_prop = edit_dist_prop)
    }
    
    #remove temporary objects
    rm(list = c("new_zips", "new_brands"))
  }
  
  #add names to output object
  if(edit_dist_prop > 0){colnames(output) <- c("comp_most", "comp_least", "comp_shannon", "comp_simpson", "comp_jac_zip", "comp_mh_zip", "comp_jac_county", "comp_mh_county", "brand_edit")}
  if(edit_dist_prop == 0){colnames(output) <- c("comp_most", "comp_least", "comp_shannon", "comp_simpson", "comp_jac_zip", "comp_mh_zip", "comp_jac_county", "comp_mh_county")}
  rownames(output) <- sampling_years
  
  return(output)
}

#get summary statistics, to be used on both the simulated and observed data
get_sum_stats <- function(running_brands, components, all_zips, angles = FALSE, edit_dist_prop = 0.1){
  #if angles are not considered, then generate component frequencies without them (and generate brand frequencies with respective columns selected)
  if(!angles){
    #if edit distances are to be calculated and included in summary statistics, then calculate them
    if(edit_dist_prop > 0){
      #construct matrix of edit distances
      edit_dist_mat <- adist(sapply(sample(nrow(running_brands), nrow(running_brands)*edit_dist_prop, replace = FALSE), function(x){intToUtf8(running_brands[x, 1:4])}))
    }
    
    #get component frequencies
    comp_freqs <- as.numeric(sort(Rfast::Table(c(running_brands[, 1:4])[which(c(running_brands[, 1:4]) > 0)]), decreasing = TRUE))
    
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
    
    #remove temporary variables
    rm(list = c("unique_counties"))
  }
  
  #if angles are considered, then generate component frequencies with them
  if(angles){
    #if edit distances are to be calculated and included in summary statistics, then calculate them
    if(edit_dist_prop > 0){
      #construct matrix of edit distances
      edit_dist_mat <- adist(sapply(sample(nrow(running_brands), nrow(running_brands)*edit_dist_prop, replace = FALSE), function(x){intToUtf8(as.numeric(paste0(running_brands[x, 1:4], running_brands[x, 5:8])))}))
    }
    
    #extract combinations of components and angles (for now just combining them as a string and remove "0 0")
    comp_angle_combos <- c(sapply(1:nrow(running_brands), function(x){paste(c(running_brands[x, 1:4]), c(running_brands[x, 5:8]))}))
    unique_comp_angle_combos <- unique(comp_angle_combos) #run this before the next line, because "0 0" needs to be accounted for later
    comp_angle_combos <- comp_angle_combos[-which(comp_angle_combos == "0 0")]
    
    #get component frequencies (where rotated components are treated as unique)
    comp_freqs <- as.numeric(sort(Rfast::Table(comp_angle_combos), decreasing = TRUE))
    
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
    
    #remove temporary variables
    rm(list = c("comp_angle_combos", "unique_comp_angle_combos", "unique_counties"))
  }
  
  #calculate summary statistics
  if(edit_dist_prop > 0){
    sum_stats <- c(max(comp_freqs)/sum(comp_freqs), #proportion of the most common component
                   min(comp_freqs)/sum(comp_freqs), #proportion of the least common component
                   hillR::hill_taxa(comp_freqs, q = 1), #shannon's diversity index
                   hillR::hill_taxa(comp_freqs, q = 2), #simpson's diversity index
                   hillR::hill_taxa_parti(comp_beta_zip_mat, q = 0)$region_similarity, #jaccard index (zip codes)
                   hillR::hill_taxa_parti(comp_beta_zip_mat, q = 2)$local_similarity, #morisita-horn index (zip codes)
                   hillR::hill_taxa_parti(comp_beta_county_mat, q = 0)$region_similarity, #jaccard index (counties)
                   hillR::hill_taxa_parti(comp_beta_county_mat, q = 2)$local_similarity, #morisita-horn index (counties)
                   mean(edit_dist_mat[upper.tri(edit_dist_mat)])) #mean edit distance
  }
  if(edit_dist_prop == 0){
    sum_stats <- c(max(comp_freqs)/sum(comp_freqs), #proportion of the most common component
                   min(comp_freqs)/sum(comp_freqs), #proportion of the least common component
                   hillR::hill_taxa(comp_freqs, q = 1), #shannon's diversity index
                   hillR::hill_taxa(comp_freqs, q = 2), #simpson's diversity index
                   hillR::hill_taxa_parti(comp_beta_zip_mat, q = 0)$region_similarity, #jaccard index (zip codes)
                   hillR::hill_taxa_parti(comp_beta_zip_mat, q = 2)$local_similarity, #morisita-horn index (zip codes)
                   hillR::hill_taxa_parti(comp_beta_county_mat, q = 0)$region_similarity, #jaccard index (counties)
                   hillR::hill_taxa_parti(comp_beta_county_mat, q = 2)$local_similarity) #morisita-horn index (counties)
  }
  
  #return summary statistics
  return(sum_stats)
}













