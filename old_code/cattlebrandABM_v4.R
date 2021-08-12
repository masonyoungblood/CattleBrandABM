
# DEFINE FUNCTIONS --------------------------------------------------------

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

# LOAD AND PREPARE DATA ---------------------------------------------------

setwd("~/Documents/Work/Summer_2021/Cattle_Brands")

#all components in the brand book index, as well as some additions (e.g. "1,," and "TX,") and one removal (e.g. "UN", or unidentifiable)
components <- c("#,,", "$,,", "%,,", "(,,", "),,", "*,,", "+,,", "-,,", "-3,",
                "/,,", "1,,", "2,,", "3,,", "4,,", "5,,", "6,,", "7,,", "8,,",
                "9,,", "=,,", "?,,", "A,,", "A1,", "A2,", "A3,", "AH,", "AN,",
                "AP,", "AR,", "AR1", "AR2", "AR3", "AR5", "AR7", "B,,", "BA,",
                "BB,", "BB1", "BB2", "BB3", "BB4", "BB5", "BB6", "BB7", "BE,",
                "BF,", "BI,", "BO,", "BT,", "BU,", "BX,", "BX1", "BX2", "C,,",
                "CA,", "CC,", "CM,", "CO,", "CP,", "CR,", "D,,", "DI,", "E,,",
                "F,,", "F1,", "FB,", "FH,", "FI,", "FL,", "FO,", "G,,", "G2,",
                "G1,", "GU,", "H,,", "H1,", "HA,", "HC,", "HK,", "HR,", "HT,",
                "I,,", "IN,", "J,,", "JA,", "JU,", "JW,", "K,,", "K1,", "KE,",
                "KS,", "L,,", "L1,", "LA,", "LI,", "M,,", "M1,", "MA,", "MI,",
                "MI1", "MI2", "MI3", "MO,", "MU,", "N,,", "N1,", "NT,", "O,,",
                "O1,", "OY,", "P,,", "PI,", "PR,", "Q,,", "QC,", "QC1", "R,,",
                "R1,", "R2,", "RE,", "S,,", "SA,", "SC,", "SD,", "SH,", "SN,",
                "SP,", "SU,", "SK,", "SR,", "SW,", "T,,", "T1,", "TA,", "TK,",
                "TK1", "TK2", "TP,", "TR,", "TR1", "TR2", "TR3", "TU,", "TX,",
                "U,,", "UM,", "V,,", "W,,", "W1,", "WA,", "X,,", "Y,,", "Y1,",
                "Z,,", "[,,", "\\,,", "],,", "^,,", "~,,", "~1,", "~2,", "~3,",
                "~4,", "~5,", "~6,")

#generate all possible components, accounting for rotation, alongside index components so rotated version can be easily converted to non-rotated ones
all_poss_components <- c()
index_components <- c()
for(x in 1:length(components)){
  if(nchar(gsub(",", "", components[x])) < 3){
    all_poss_components <- c(all_poss_components, components[x], paste0(substr(components[x], 1, 2), as.character(1:9)))
    index_components <- c(index_components, components[x], rep(components[x], length(paste0(substr(components[x], 1, 2), as.character(1:9)))))
  }
}
all_poss_components <- data.frame(index = index_components, rot = all_poss_components)
rm(index_components)

#read in brand data
brands <- read.csv("cattle_brand_data/brand_data.csv")[, -1]

#remove brands with length of 12 (incorrectly specified according to manual inspection)
brands <- brands[-which(nchar(brands$brand) == 12), ]

#substring brand codes into vector of four components and one location
brands$brand <- lapply(1:nrow(brands), function(x){substring(brands$brand[[x]], first = c(1, 4, 7, 10, 13), last = c(3, 6, 9, 12, 13))})

#move duplicate code (12th position) into the sixth position of the brand vectors
for(x in 1:nrow(brands)){
  #if fourth position starts with a comma and ends with a number (or O)
  if(substring(brands$brand[[x]][4], 1, 1) == "," & substring(brands$brand[[x]][4], 3) %in% c("O", 0:9)){
    #put duplicate number into the sixth position
    brands$brand[[x]][6] <- gsub(",", "", brands$brand[[x]][4])
    
    #replace any O with 0
    brands$brand[[x]][6] <- gsub("O", 0, brands$brand[[x]][6])
    
    #replace original fourth position with ",,,"
    brands$brand[[x]][4] <- ",,,"
  }
}

#remove all misspecified brands (with components that don't appear in all possible components)
misspecified <- which(sapply(1:nrow(brands), function(x){length(which(brands$brand[[x]][1:4] %in% c(all_poss_components$rot, ",,,")))}) != 4)
brands <- brands[-misspecified,]

#convert brands to numbers + rotations, just in the order in which they appear in the components
converted_brands <- matrix(0, nrow = nrow(brands), ncol = 10)
converted_brands[, 9] <- as.numeric(brands$location)
converted_brands[, 10] <- as.numeric(brands$year)
for(i in 1:nrow(brands)){
  brand_nums <- match(all_poss_components$index[match(brands$brand[[i]][1:4], all_poss_components$rot)], components) #numbers of components (ignoring rotation)
  brand_nums[is.na(brand_nums)] <- 0
  angle_nums <- rep(0, 4)
  for(j in 1:length(brand_nums[!is.na(brand_nums)])){
    temp <- as.numeric(setdiff(strsplit(all_poss_components$rot[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]],
                               strsplit(all_poss_components$index[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]]))
    if(length(temp) > 0){angle_nums[j] <- temp}
  }
  converted_brands[i, 1:4] <- brand_nums
  converted_brands[i, 5:8] <- angle_nums
  rm(list = c("brand_nums", "angle_nums", "temp"))
}

#store converted brands and remove original brands
brands <- converted_brands
rm(converted_brands)

#get all kansas zip codes
data("zip.regions", package = "choroplethrZip")
data("zip_code_db", package = "zipcodeR")
all_zips <- sort(as.numeric(unique(zip.regions[which(zip.regions$state.name == "kansas"),]$region)))
all_zips <- data.table::data.table(zip = all_zips, county = zip.regions$county.fips.numeric[match(all_zips, zip.regions$region)], freq = as.numeric(table(factor(brands[, 9], levels = all_zips))))

#get pairwise distances between zip codes and store
#zip_dists <- proxy::dist(all_zips, method = zipcodeR::zip_distance) #calculate pairwise distances
#zip_dists <- as.matrix(zip_dists) #convert to matrix
#colnames(zip_dists) <- all_zips #add colnames
#rownames(zip_dists) <- all_zips #add rownames
#zip_dists <- zip_dists*1.609344 #miles to kilometers
#save(zip_dists, file = "zip_dists.RData")
load("zip_dists.RData")

#subset brands to only include those from kansas
brands <- brands[which(brands[, 9] %in% all_zips), ]

#probability of rotation (proportion of rotated brands in the full dataset)
rot_prob <- as.numeric(1-(table(brands[, 5:8])[1]/sum(table(brands[, 5:8]))))

#get min and max distances between zip codes (to inform prior range)
round(min(zip_dists[which(zip_dists != 0)]))
round(max(zip_dists))

#reshape components so it includes whether or not a component is rotatable (includes a ",")
components <- data.table::data.table(components = components, rotatable = grepl(",", components))

#separate brands data by year
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#calculate average number of new brands that appear each year and old brands that disappear each year (fsetdiff gets rows of first that are not in second)
n_new <- mean(c((nrow(data.table::fsetdiff(brands_2014, brands_2008))/6),
                nrow(data.table::fsetdiff(brands_2015, brands_2014)),
                nrow(data.table::fsetdiff(brands_2016, brands_2015))))
n_old <- mean(c((nrow(data.table::fsetdiff(brands_2008, brands_2014))/6),
                nrow(data.table::fsetdiff(brands_2014, brands_2015)),
                nrow(data.table::fsetdiff(brands_2015, brands_2016))))

#test out the ABM
start <- Sys.time()
hai <- cattlebrandABM(as.matrix(brands_2008), components, all_zips, zip_dists, 2008, c(2014, 2015, 2016), n_new, n_old, rot_prob, complexity = 3, copy_radius = 80, copy_strength = 1, dist_radius = 20, dist_strength = 1, angles = FALSE)
Sys.time() - start
hai
