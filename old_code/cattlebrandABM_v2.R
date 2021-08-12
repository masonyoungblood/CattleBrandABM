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

#unique number of brands, ignoring location but accounting for duplicates (position six)
length(unique(sapply(1:nrow(brands), function(x){paste(brands$brand[[x]][c(1:4, 6)], collapse = "")})))

# #get distribution of components
# unlisted_components <- unlist(lapply(1:nrow(brands), function(x){brands$brand[[x]][1:4]})) #unlist components
# unlisted_components <- unlisted_components[-which(unlisted_components == ",,,")] #remove empty components
# component_freqs <- as.data.frame(sort(table(all_poss_components$index[match(unlisted_components, all_poss_components$rot)]), decreasing = TRUE)) #get frequency table of components, ignoring rotation
# colnames(component_freqs) <- c("component", "freq")
# 
# #get distribution of angles
# angle_freqs <- sapply(1:length(unlisted_components), function(x){
#   setdiff(strsplit(all_poss_components[match(unlisted_components[x], all_poss_components$rot), 2], split = "")[[1]],
#           strsplit(all_poss_components[match(unlisted_components[x], all_poss_components$rot), 1], split = "")[[1]]) #get all characters from second that do not appear in first (empty for three digit components)
# })
# angle_freqs[which(lengths(angle_freqs) == 0)] <- 0
# angle_freqs <- as.data.frame(sort(table(unlist(angle_freqs)), decreasing = TRUE))
# colnames(angle_freqs) <- c("angle", "freq")

#set radii (in miles) for copying and distinctiveness
copy_radius <- 80
dist_radius <- 20

#set strength for copying and distinctiveness (for both components and angles, for now)
copy_strength <- 1
dist_strength <- 1

#probability of a 
rot_prob <- 0.2

#get all kansas zip codes
data("zip.regions", package = "choroplethrZip")
all_zips <- unique(zip.regions[which(zip.regions$state.name == "kansas"),]$region)

#pre-collect all zip codes within the two radii for each each zip code
all_zips <- data.table::data.table(zip = all_zips,
                                   copy = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], copy_radius); temp <- temp[which(temp %in% all_zips)]; temp}),
                                   dist = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], dist_radius); temp <- temp[which(temp %in% all_zips)]; temp}))

#concatenate first four componenets of brands ahead of time to save processing time during brand generation
concat_brands <- sapply(1:nrow(brands), function(x){paste(brands$brand[[x]][c(1:4)], collapse = "")})

#note: duplicates may not have to be handled... if we allow duplicates without even having codes they will influence future generations of brands without having to account for them
brand_generator <- function(components, brands, rot_prob, concat_brands, zip, all_zips, copy_strength, dist_strength, duplicates = FALSE, spatial = FALSE, angles = FALSE){
  if(!spatial){
    sim_brand <- sample(components, sample(4, 1)) #sample a random number of up to 4 components
    
    if(angles){
      to_rot <- which(grepl(",", sim_brand) & sample(c(TRUE, FALSE), length(sim_brand), replace = TRUE, prob = c(rot_prob, 1-rot_prob))) #get vector of components to rotate
      for(i in to_rot){substring(sim_brand[to_rot[x]], nchar(sim_brand[to_rot[x]])) <- sample(as.character(1:9), 1)} #replace final digit of rotated components with rotation angle
    }
    
    sim_brand <- c(sim_brand, rep(",,,", 4-length(sim_brand)), sample(as.character(1:7), 1)) #combine with commas and location
  }
  
  if(spatial){
    #get frequency distribution of brands within the copy radius
    copy_components <- brands$brand[brands$location %in% all_zips$copy[[which(all_zips$zip == zip)]]]
    copy_components <- unlist(lapply(1:length(copy_components), function(x){copy_components[[x]][1:4]})) #unlist components
    copy_components <- copy_components[-which(copy_components == ",,,")] #remove empty components
    if(angles){copy_unlisted <- copy_components} #if angles, save unlisted components for later
    copy_components <- as.data.frame(table(factor(all_poss_components$index[match(copy_components, all_poss_components$rot)], levels = components))) #get frequencies
    colnames(copy_components) <- c("component", "freq")
    
    #get frequency distribution of brands within the dist radius
    dist_components <- brands$brand[brands$location %in% all_zips$dist[[which(all_zips$zip == zip)]]]
    dist_components <- unlist(lapply(1:length(dist_components), function(x){dist_components[[x]][1:4]})) #unlist components
    dist_components <- dist_components[-which(dist_components == ",,,")] #remove empty components
    if(angles){dist_unlisted <- dist_components} #if angles, save unlisted components for later
    dist_components <- as.data.frame(table(factor(all_poss_components$index[match(dist_components, all_poss_components$rot)], levels = components))) #get frequencies
    colnames(dist_components) <- c("component", "freq")
    
    sim_brand <- sample(components, sample(4, 1), prob = ((copy_components$freq+1)^copy_strength)*((1/(dist_components$freq+1))^dist_strength)) #sample a random number of up to 4 components, weighted by the copy and dist probability function
    
    if(angles){
      #get frequency distribution of angles within the copy radius
      copy_angles <- sapply(1:length(copy_unlisted), function(x){
        setdiff(strsplit(all_poss_components[match(copy_unlisted[x], all_poss_components$rot), 2], split = "")[[1]],
                strsplit(all_poss_components[match(copy_unlisted[x], all_poss_components$rot), 1], split = "")[[1]]) #get all characters from second that do not appear in first (empty for three digit components)
      })
      copy_angles <- as.data.frame(table(factor(unlist(copy_angles), levels = as.character(1:9))))
      colnames(copy_angles) <- c("angle", "freq")
      
      #get frequency distribution of angles within the dist radius
      dist_angles <- sapply(1:length(dist_unlisted), function(x){
        setdiff(strsplit(all_poss_components[match(dist_unlisted[x], all_poss_components$rot), 2], split = "")[[1]],
                strsplit(all_poss_components[match(dist_unlisted[x], all_poss_components$rot), 1], split = "")[[1]]) #get all characters from second that do not appear in first (empty for three digit components)
      })
      dist_angles <- as.data.frame(table(factor(unlist(dist_angles), levels = as.character(1:9))))
      colnames(dist_angles) <- c("angle", "freq")
      
      to_rot <- which(grepl(",", sim_brand) & sample(c(TRUE, FALSE), length(sim_brand), replace = TRUE, prob = c(rot_prob, 1-rot_prob))) #get vector of components to rotate
      for(x in 1:length(to_rot)){substring(sim_brand[to_rot[x]], nchar(sim_brand[to_rot[x]])) <- sample(as.character(1:9), 1, prob = ((copy_angles$freq+1)^copy_strength)*((1/(dist_angles$freq+1))^dist_strength))} #replace final digit of rotated components with rotation angle
    }
    
    sim_brand <- c(sim_brand, rep(",,,", 4-length(sim_brand)), sample(as.character(1:7), 1)) #combine with commas and location
  }
  
  if(duplicates){
    #find extant brands that match, and assign a duplicate value of one higher than the previous maximum (ignoring location entirely)
    matches <- which(concat_brands == paste(sim_brand[1:4], collapse = ""))
    if(length(matches) > 0){
      prev <- as.numeric(sapply(1:length(matches), function(x){brands$brand[[matches[x]]][6]}))
      if(length(which(prev > 0)) > 0){
        sim_brand[6] <- max(prev, na.rm = TRUE)+1
      }
    }
  }
  
  return(brand = sim_brand)
}

brand_generator(components = components, brands = brands, rot_prob = rot_prob, zip = sample(all_zips$zip, 1), all_zips = all_zips, copy_strength = copy_strength, dist_strength = dist_strength, duplicates = FALSE, spatial = TRUE, angles = TRUE)

#NOW let's try the brand generator in a time loop... shall we simulate a set of starting brands
#HOW do we simulate a set of starting brands that also contain the correct level of geographic dependency?
#I think it will definitely be best to just start from the observed pattern and run from that!
brands <- brands[which(brands$year == 2008), ] #only brands from 2008
brands <- sim_from[which(sim_from$location %in% all_zips$zip), ] #only brands from kansas
brands <- brands[, 1:2]

#for now, just going to add N new brands and then delete N overall brands (so small probability of new ones being immediately deleted)
n_new <- 500
n_old <- 500

#generate zip codes prior to generating brands (to iterate through), this should eventually be nonrandom...
new_zips <- sample(all_zips$zip, n_new, replace = TRUE)

#generate new brands
new_brands <- lapply(1:n_new, function(x){brand_generator(components = components, brands = brands, rot_prob = rot_prob, zip = new_zips[x], all_zips = all_zips, copy_strength = copy_strength, dist_strength = dist_strength, duplicates = FALSE, spatial = TRUE, angles = TRUE)})

#combine with existing brands
brands <- data.table::rbindlist(list(brands, data.table::data.table(brand = new_brands, location = new_zips)))

#randomly remove N overall brands
brands <- brands[-sample(1:nrow(brands), n_old), ]



