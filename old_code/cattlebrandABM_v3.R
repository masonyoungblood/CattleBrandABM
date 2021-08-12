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

#duplicates will have to be handled separately... doing it within the generator raises issues (replacing digit makes duplicates distinct)
brands <- read.csv("cattle_brand_data/brand_data.csv")[, -1]

#brands with length of 12 were not incorrectly scanned, they were incorrectly specified (manually checked)
brands <- brands[-which(nchar(brands$brand) == 12), ]

brands$brand <- lapply(1:nrow(brands), function(x){substring(brands$brand[[x]], first = c(1, 4, 7, 10, 13), last = c(3, 6, 9, 12, 13))})

#if digit 12 is a number and there is also a fourth component, then it's impossible to differentiate between whether than component is rotate or brand is a duplicate
#in these cases, default to component being rotated
#only treat as duplicate if there is no fourth component but digit 12 has a number in it
#some duplicates are 10 or higher, so also search for 0 in 12th position and extract duplicate number by removing commas from string

for(x in 1:nrow(brands)){
  if(substring(brands$brand[[x]][4], 1, 1) == "," & substring(brands$brand[[x]][4], 3) %in% c("O", 0:9)){ #if fourth position starts with a comma and ends with a number (or O)
    brands$brand[[x]][6] <- gsub(",", "", brands$brand[[x]][4]) #put duplicate number into the sixth position
    brands$brand[[x]][6] <- gsub("O", 0, brands$brand[[x]][6]) #replace any O with 0
    brands$brand[[x]][4] <- ",,," #replace original fourth position with ",,,"
  }
}

#remove all misspecified brands (with components that don't appear in all possible components)
misspecified <- which(sapply(1:nrow(brands), function(x){length(which(brands$brand[[x]][1:4] %in% c(all_poss_components$rot, ",,,")))}) != 4)
brands <- brands[-misspecified,]

#convert brands to numbers + rotations, just in the order in which they appear in the components
converted_brands <- matrix(NA, nrow = nrow(brands), ncol = 10)
converted_brands[, 9] <- as.numeric(brands$location)
converted_brands[, 10] <- as.numeric(brands$year)
for(i in 1:nrow(brands)){
  brand_nums <- match(all_poss_components$index[match(brands$brand[[i]][1:4], all_poss_components$rot)], components) #numbers of components (ignoring rotation)
  angle_nums <- rep(NA, 4)
  for(j in 1:length(brand_nums[!is.na(brand_nums)])){
    temp <- as.numeric(setdiff(strsplit(all_poss_components$rot[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]],
                               strsplit(all_poss_components$index[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]]))
    if(length(temp) > 0){angle_nums[j] <- temp}
    if(length(temp) == 0){angle_nums[j] <- 0} #put zero if there is no rotation, because unrotated symbols need to differentiated from no symbols in the angles data
  }
  converted_brands[i, 1:4] <- brand_nums
  converted_brands[i, 5:8] <- angle_nums
  rm(list = c("brand_nums", "angle_nums", "temp"))
}

#set radii (in miles) for copying and distinctiveness
copy_radius <- 80
dist_radius <- 20

#set strength for copying and distinctiveness (for both components and angles, for now)
copy_strength <- 1
dist_strength <- 1

#probability of rotation (proportion of rotated brands in the full dataset)
rot_prob <- as.numeric(1-(table(converted_brands[, 5:8])[1]/sum(table(converted_brands[, 5:8]))))

#complexity parameter is the lambda value of a poisson distribution, where we calculate the probability of a value 1, 2, 3, or 4
#0.1 seems like a good lower bound, and 15 is a good upper bound
#3 is an intermediate value... will require some form a non-uniform prior so that it's not explore a more complex space on average
complexity <- 3

#get all kansas zip codes
data("zip.regions", package = "choroplethrZip")
all_zips <- unique(zip.regions[which(zip.regions$state.name == "kansas"),]$region)

#pre-collect all zip codes within the two radii for each each zip code (should batch collect this and save across prior before doing actuall ABC)
all_zips <- data.table::data.table(zip = all_zips,
                                   county = zip.regions$county.fips.numeric[match(all_zips, zip.regions$region)],
                                   copy = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], copy_radius); temp <- temp[which(temp %in% all_zips)]; as.numeric(temp)}),
                                   dist = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], dist_radius); temp <- temp[which(temp %in% all_zips)]; as.numeric(temp)}))
all_zips$zip <- as.numeric(all_zips$zip)

brands <- converted_brands
n_comp <- length(components)

components <- data.table::data.table(components = components, rotatable = grepl(",", components))

brand_generator <- function(components, brands, complexity, rot_prob, zip, all_zips, copy_strength, dist_strength, spatial = FALSE, angles = FALSE){
  #if spatial distribution of components is ignored
  if(!spatial){
    sim_brand <- sample(nrow(components), sample(1:4, 1, prob = dpois(c(1, 2, 3, 4), replace = TRUE, complexity))) #randomly sample up to four components, where the number of components is drawn from a poisson distribution with a lambda equal to "complexity"
    
    #if angles are to be considered
    if(angles){
      sim_angles <- rep(NA, 4) #generate empty vector to fill with angles
      
      #go through each component in the simulated brand
      for(x in 1:length(sim_brand)){
        #if the component is rotatable, and if a sampled boolent (weighted by "rot_prob") is TRUE
        if(components$rotatable[sim_brand[x]] & sample(c(TRUE, FALSE), 1, prob = c(rot_prob, 1-rot_prob))){
          sim_angles[x] <- sample(9, 1) #then randomly sample an angle
        } else{
          sim_angles[x] <- 0 #else put a zero into the vector
        }
      }
    }
  }
  
  #if spatial distribution of components is considered
  if(spatial){
    copy_components <- c(brands[which(brands[, 9] %in% all_zips$copy[[which(all_zips$zip == zip)]]), 1:4]) #get all components present within the copy radius
    copy_components <- copy_components[-which(is.na(copy_components))] #remove empty components
    copy_components <- as.numeric(table(factor(copy_components, levels = 1:nrow(components)))) #generate frequency list, where missing components are given zero
    
    dist_components <- c(brands[which(brands[, 9] %in% all_zips$dist[[which(all_zips$zip == zip)]]), 1:4]) #get all components present within the dist radius
    dist_components <- dist_components[-which(is.na(dist_components))] #remove empty components
    dist_components <- as.numeric(table(factor(dist_components, levels = 1:nrow(components)))) #generate frequency list, where missing components are given zero
    
    sim_brand <- sample(1:nrow(components), sample(1:4, 1, prob = dpois(c(1, 2, 3, 4), complexity)), replace = TRUE, prob = ((copy_components+1)^copy_strength)*((1/(dist_components+1))^dist_strength)) #sample up to four components, weighted by the copy and dist probability function, where the number of components is drawn from a poisson distribution with a lambda equal to "complexity"
    
    #if angles are to be considered
    if(angles){
      copy_angles <- c(brands[which(brands[, 9] %in% all_zips$copy[[which(all_zips$zip == zip)]]), 5:8]) #get all angles present within the copy radius
      copy_angles <- copy_angles[-which(is.na(copy_angles))] #remove empty angles
      copy_angles <- as.numeric(table(factor(copy_angles, levels = 1:9))) #generate frequency list, where missing angles are given zero
      
      dist_angles <- c(brands[which(brands[, 9] %in% all_zips$dist[[which(all_zips$zip == zip)]]), 5:8]) #get all angles present within the dist radius
      dist_angles <- dist_angles[-which(is.na(dist_angles))] #remove empty angles
      dist_angles <- as.numeric(table(factor(dist_angles, levels = 1:9))) #generate frequency list, where missing angles are given zero
      
      sim_angles <- rep(NA, 4) #generate empty vector to fill with angles
      
      #go through each component in the simulated brand
      for(x in 1:length(sim_brand)){
        #if the component is rotatable, and if a sampled boolent (weighted by "rot_prob") is TRUE
        if(components$rotatable[sim_brand[x]] & sample(c(TRUE, FALSE), 1, prob = c(rot_prob, 1-rot_prob))){
          sim_angles[x] <- sample(1:9, 1, prob = ((copy_angles+1)^copy_strength)*((1/(dist_angles+1))^dist_strength)) #then sample an angle, weighted by the copy and dist probability function
        } else{
          sim_angles[x] <- 0 #else put a zero into the vector
        }
      }
    }
    
    sim_brand <- c(sim_brand, rep(NA, 4-length(sim_brand))) #combine with NAs to bring up to four
  }
  
  #if angles were not considered
  if(!angles){
    return(c(sim_brand)) #return the brands
  } else{
    return(c(sim_brand, sim_angles)) #if else, return the brands and angles
  }
}

brand_generator(components = components, brands = brands, rot_prob = rot_prob, complexity = 3, zip = sample(all_zips$zip, 1), all_zips = all_zips, copy_strength = copy_strength, dist_strength = dist_strength, spatial = TRUE, angles = FALSE)

brands <- brands[which(brands[, 9] %in% all_zips$zip), ] #only brands from kansas

brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#get average number of brands that appear in subsequent years, vs. disappear from previous years - fsetdiff gets rows of first that are not in second
n_new <- mean(c((nrow(data.table::fsetdiff(brands_2014, brands_2008))/6),
                nrow(data.table::fsetdiff(brands_2015, brands_2014)),
                nrow(data.table::fsetdiff(brands_2016, brands_2015))))
n_old <- mean(c((nrow(data.table::fsetdiff(brands_2008, brands_2014))/6),
                nrow(data.table::fsetdiff(brands_2014, brands_2015)),
                nrow(data.table::fsetdiff(brands_2015, brands_2016))))

#get frequencies of brands in all kansas zips
zip_probs <- as.numeric(table(factor(brands[, 9], levels = all_zips$zip)))

#initialize new brand matrix from the 2008 data
running_brands <- as.matrix(brands_2008)

for(i in 1:10){
  #generate zip codes prior to generating brands (to iterate through), this should eventually be nonrandom...
  new_zips <- sample(all_zips$zip, n_new, replace = TRUE, prob = zip_probs)
  
  #generate new brands
  new_brands <- t(sapply(1:n_new, function(x){brand_generator(components = components, brands = running_brands, rot_prob = rot_prob, complexity = 6, zip = new_zips[x], all_zips = all_zips, copy_strength = 3, dist_strength = 3, spatial = TRUE, angles = TRUE)}))
  new_brands <- cbind(new_brands, new_zips)
  
  running_brands <- rbind(running_brands, new_brands)
  
  #randomly remove N overall brands
  running_brands <- running_brands[-sample(1:nrow(running_brands), n_old), ]
}



string_dists <- stringdist::seq_distmatrix(lapply(1:nrow(running_brands), function(x){as.numeric(na.omit(running_brands[x, 1:4]))}))

library(zipcodeR)
geo_dists <- proxy::dist(running_brands[, 9], method = zipcodeR::zip_distance)

plot(string_dists, geo_dists)

#get frequency distributions
comp_freqs <- as.numeric(sort(table(c(running_brands[, 1:4])[-which(is.na(c(running_brands[, 1:4])))]), decreasing = TRUE)) #component frequencies, use if angles = FALSE
comp_angle_combos <- c(sapply(1:nrow(running_brands), function(x){paste(c(running_brands[x, 1:4]), c(running_brands[x, 5:8]))}))
comp_angle_combos <- comp_angle_combos[-which(comp_angle_combos == "NA NA")]
comp_angle_freqs <- as.numeric(sort(table(comp_angle_combos), decreasing = TRUE)) #component frequencies where rotated components are treated as unique, use if angles = TRUE
brand_freqs <- as.numeric(sort(table(sapply(1:nrow(running_brands), function(x){paste(running_brands[x, 1:8], collapse = " ")})), decreasing = TRUE)) #always calculate, and just change the columns if angles = TRUE or FALSE

beta_zip_mat <- matrix(0, nrow = length(all_zips$zip), ncol = nrow(components))

for(i in 1:length(all_zips$zip)){
  as.numeric(table(factor(c(running_brands[which(running_brands[, 9] == all_zips$zip[1]), 1:4]), levels = 1:nrow(components))))
}

beta_zip_mat <- t(sapply(1:length(all_zips$zip), function(x){as.numeric(table(factor(c(running_brands[which(running_brands[, 9] == all_zips$zip[x]), 1:4]), levels = 1:nrow(components))))}))
beta_zip_mat <- beta_zip_mat[, -which(colSums(beta_zip_mat) == 0)] #remove empty columns
beta_zip_mat <- beta_zip_mat[-which(rowSums(beta_zip_mat) == 0), ] #remove empty rows

beta_county_mat <- t(sapply(1:length(unique(all_zips$county)), function(x){as.numeric(table(factor(c(running_brands[which(running_brands[, 9] %in% all_zips$zip[which(all_zips$county == unique(all_zips$county)[x])]), 1:4]), levels = 1:nrow(components))))}))
beta_county_mat <- beta_county_mat[, -which(colSums(beta_county_mat) == 0)] #remove empty columns
beta_county_mat <- beta_county_mat[-which(rowSums(beta_county_mat) == 0), ] #remove empty rows

#beta diversity measures
  #morisita-horn is abundance-based, whereas jaccard is incidence-based (chao et al., 2006)
#table 2 of chao et al. (2014): 10.1146/annurev-ecolsys-120213-091540
#chao et al. (2006): 10.1111/j.1541-0420.2005.00489.x
  #morisita-horn index systematically underestimates similarity
  #jaccard index appears to do the opposite, although it is less clear
#beck et al. (2013): 10.1111/2041-210x.12023
  #directions of systematic bias in morisita-horn and jaccard indices are less clear
#schroeder and jenkins (2018): 10.1002/ecs2.2100
  #jaccard is most robust incidence-based measure to sampling error

sum_stats <- c(max(comp_freqs)/sum(comp_freqs), #proportion of the most common component
               min(comp_freqs)/sum(comp_freqs), #proportion of the least common component
               hillR::hill_taxa(comp_freqs, q = 1), #hill number (shannon's diversity index)
               hillR::hill_taxa(comp_freqs, q = 2), #hill number (simpson's diversity index)
               exp(summary(nls(y ~ SSasymp(x, yf, y0, log_alpha), data = data.frame(y = comp_freqs, x = 1:length(comp_freqs))))$coefficients[3]), #rate constant of exponential decay curve
               hillR::hill_taxa_parti(beta_zip_mat, q = 0)$region_similarity, #jaccard index (zip codes)
               hillR::hill_taxa_parti(beta_zip_mat, q = 2)$local_similarity, #morisita-horn index (zip codes)
               hillR::hill_taxa_parti(beta_county_mat, q = 0)$region_similarity, #jaccard index (counties)
               hillR::hill_taxa_parti(beta_county_mat, q = 2)$local_similarity) #morisita-horn index (counties)

#TRY OUT SOME DISTRIBUTIONS TO SEE WHAT MIGHT WORK FOR SUM STATS

#https://cran.r-project.org/web/packages/poweRlaw/vignettes/b_powerlaw_examples.pdf
#https://rpubs.com/lgadar/power-law
library(poweRlaw)

#powerlaw
m_pl <- displ$new(brand_freqs)
est_pl <- estimate_xmin(m_pl)
m_pl$setXmin(est_pl)

#lognormal
m_ln = dislnorm$new(brand_freqs)
est_ln <- estimate_xmin(m_ln)
m_ln$setXmin(est_ln)

#exponential
m_exp <- disexp$new(brand_freqs)
est_exp <- estimate_xmin(m_exp)
m_exp$setXmin(est_exp)

#poisson
m_poi <- dispois$new(brand_freqs)
est_poi <- estimate_xmin(m_poi)
m_poi$setXmin(est_poi)

plot(m_pl)
lines(m_pl, col="red")
lines(m_ln, col="green")
lines(m_poi, col="blue")
lines(m_exp, col="magenta")

#exponential looks like the best... so let's fit it directly

library(tidyverse)
library(broom)

fit <- nls(y ~ SSasymp(x, yf, y0, log_alpha), data = data.frame(y = brand_freqs, x = 1:length(brand_freqs)))
qplot(t, y, data = augment(fit)) + geom_line(aes(y = .fitted))

#USE THIS AS A SUM STAT!!!
exp(summary(fit)$coefficients[3]) #rate constant of the exponential decay curve (AKA alpha)

exp(summary(nls(y ~ SSasymp(x, yf, y0, log_alpha), data = data.frame(y = comp_freqs, x = 1:length(comp_freqs))))$coefficients[3])


#modeling number of components as a poisson distribution might be the move! 0.1 to 15 (3 as roughly even probabilities)
poisson <- rpois(100000, lambda = 15)
poisson <- ppois(1, lambda = 15)
poisson <- poisson[which(poisson > 0 & poisson < 5)]
hist(poisson)
plot(dpois(c(1, 2, 3, 4), 3))


plot(BBmisc::normalize(dpois(c(1, 2, 3, 4), 0.1), "range"), type = "l", main = "Lambda equals 3", xlab = "Number of Components", ylab = "Probability", xaxt = "n")
axis(side = 1,at = c(1, 2, 3, 4), labels=c("1", "2", "3", "4"))

plot(BBmisc::normalize(dpois(c(1, 2, 3, 4), 0.1), "range"), type = "l", main = "Lambda equals 3", xlab = "Number of Components", ylab = "Probability", xaxt = "n")
axis(side = 1,at = c(1, 2, 3, 4), labels=c("1", "2", "3", "4"))


