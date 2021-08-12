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

rotations <- as.character(1:9)
locations <- as.character(1:7)

#generate all possible components, accounting for rotation, alongside index components so rotated version can be easily converted to non-rotated ones
all_poss_components <- c()
index_components <- c()
for(x in 1:length(components)){
  if(nchar(gsub(",", "", components[x])) < 3){
    all_poss_components <- c(all_poss_components, components[x], paste0(substr(components[x], 1, 2), rotations))
    index_components <- c(index_components, components[x], rep(components[x], length(paste0(substr(components[x], 1, 2), rotations))))
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

#get distribution of components
unlisted_components <- unlist(lapply(1:nrow(brands), function(x){brands$brand[[x]][1:4]})) #unlist components
unlisted_components <- unlisted_components[-which(unlisted_components == ",,,")] #remove empty components
component_freqs <- as.data.frame(sort(table(all_poss_components$index[match(unlisted_components, all_poss_components$rot)]), decreasing = TRUE)) #get frequency table of components, ignoring rotation
colnames(component_freqs) <- c("component", "freq")

#set radii (in miles) for copying and distinctiveness
copy_radius <- 100
dist_radius <- 25

#probability of a 
rot_prob <- 0.5

#get all kansas zip codes
data("zip.regions", package = "choroplethrZip")
all_zips <- unique(zip.regions[which(zip.regions$state.name == "kansas"),]$region)

#pre-collect all zip codes within the two radii for each each zip code
all_zips <- data.table::data.table(zip = all_zips,
                                   copy = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], copy_radius); temp <- temp[which(temp %in% all_zips)]; temp}),
                                   dist = sapply(1:length(all_zips), function(x){temp <- ZipRadius::getZips(all_zips[x], dist_radius); temp <- temp[which(temp %in% all_zips)]; temp}))

#concatenate first four componenets of brands ahead of time to save processing time during brand generation
concat_brands <- sapply(1:nrow(brands), function(x){paste(brands$brand[[x]][c(1:4)], collapse = "")})

brand_generator <- function(components, rotations, locations, brands, concat_brands, rot_prob){
  sim_brand <- sample(components, sample(4, 1)) #sample a random number of up to 4 components
  to_rot <- which(grepl(",", sim_brand) & sample(c(TRUE, FALSE), length(sim_brand), replace = TRUE, prob = c(rot_prob, 1-rot_prob))) #get vector of components to rotate
  for(i in to_rot){substring(sim_brand[to_rot[x]], nchar(sim_brand[to_rot[x]])) <- sample(rotations, 1)} #replace final digit of rotated components with rotation angle
  sim_brand <- c(sim_brand, rep(",,,", 4-length(sim_brand)), sample(locations, 1)) #combine with commas and location
  
  #find extant brands that match, and assign a duplicate value of one higher than the previous maximum (ignoring location entirely)
  matches <- which(concat_brands == paste(sim_brand[1:4], collapse = ""))
  if(length(matches) > 0){
    prev <- as.numeric(sapply(1:length(matches), function(x){brands$brand[[matches[x]]][6]}))
    if(length(which(prev > 0)) > 0){
      sim_brand[6] <- max(prev, na.rm = TRUE)+1
    }
  }
  
  return(sim_brand)
}

sapply(1:200, function(x){brand_generator(components, rotations, locations, brands, concat_brands, rot_prob)})

