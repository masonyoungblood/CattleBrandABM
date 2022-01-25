Agent-Based Model of Cattle Brands
================
Mason Youngblood

A detailed description of the structure of the cattle brand data can be
found on the [corresponding
GitHub](https://github.com/masonyoungblood/cattle_brand_data).

<img src="https://images.unsplash.com/photo-1590249351139-c6effec78d2a?ixid=MnwxMjA3fDB8MHxzZWFyY2h8MjV8fGJhYnklMjBjb3dzfGVufDB8fDB8fA%3D%3D&ixlib=rb-1.2.1&w=1000&q=80" width="50%" style="display: block; margin: auto;" />

## Load and prepare brands data

First let’s read in and clean up our brand data.

``` r
#read in brand data
brands <- read.csv("brand_data.csv")[, -1]

#remove brands with length of 12 (incorrectly specified according to manual inspection)
brands <- brands[-which(nchar(brands$brand) == 12), ]

#substring brand codes into vector of four components and one location
brands$brand <- lapply(1:nrow(brands), function(x){substring(brands$brand[[x]], first = c(1, 4, 7, 10, 13), last = c(3, 6, 9, 12, 13))})

#print a sample
brands[1:10, ]
```

    ##                    brand location page year
    ## 1  #,,, ,,,, ,,,, ,,,, 1    67563    1 1990
    ## 2  #,,, ,,,, ,,,, ,,1, 1    67835    1 1990
    ## 3  #,,, ,,,, ,,,, ,,1, 2    66092    1 1990
    ## 4  $,,, $,,, ,,,, ,,,, 1    03060    1 1990
    ## 5  $,,, ,,,, ,,,, ,,,, 1    66617    1 1990
    ## 6  $,,, ,,,, ,,,, ,,,, 2    67576    1 1990
    ## 7  $,,, ,,,, ,,,, ,,,, 3    67333    1 1990
    ## 8  $,,, ,,,, ,,,, ,,1, 1    67104    1 1990
    ## 9  $,,, ,,,, ,,,, ,,1, 2    67347    1 1990
    ## 10 $,,, ,,,, ,,,, ,,1, 3    66428    1 1990

Each three digit code corresponds to a component in the brand, and the
final single digit corresponds to the location of the brand on the
animal. Note that the extra commas at the end of each brand component
are just part of the data frame output. Some brands have a single digit
in the 12th digit before the location digit to separate duplicates,
where the same components are arranged in different ways. Let’s extract
those and put them into the sixth position of the brand vectors.

``` r
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

#save
save(brands, file = "raw_brands.RData")
```

Before we move any further we should determine whether letters in
components tend to be initials or whether they should be treated as
distinct. Here are the frequencies of letters in the brands.

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

If we compare this with the distribution of first letters of surnames
that appeared more than 100 times in the 2010 US Census we can see that
the distributions are drastically different.

<img src="README_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
#load in the checked brands with letters and get the proportion correct
prop_correct <- mean(read.csv("letters_checked.csv")$initials)*100
```

Weighting the US surname distribution so that it matches the ethnic
composition of Kansas does not solve the issue, so we manually checked
200 random brands with letters in the 2016 brand book. In 78% of the
random brands at least one of the letters corresponded to one or both of
the first or last names of the owners or the ranch (with some letters
modifying others, like A and L both appearing in a brand where the L is
a “leg” on the right side of the A in a brand owned by the family
Asmussen, and some characters combined to make a letter, like L and 7
combined into an S for Schneider).

Since a significant number of letters in brands are not initials, some
letters (e.g. I and O) are also used to code shapes, and different
letters have different possible rotation angles, we chose to keep them
as separate categories. Numbers were also included as separate
categories. Some of the symbols in the brand book have variants that are
distinguished using the third digit that is usually used for rotation
(e.g. AR, BB, MI, TK, and TR). We collapsed each set of these symbols
into single categories that can be rotated just like any other
component.

We made some further adjustments to Kansas’ coding scheme by removing
redundant components and restricting the possible angles of rotations
for different symbols. All of these adjustments are stored in
`components.xlsx`: the `collapse` sheet has which components should be
converted to what along with their new angles, and the `rotation` sheet
has which components can be rotated in which directions. For
clarification, the possible rotation angles for each component are the
first unique rotations for that shape. For example, the 1, 3, and 4
rotations for the letter A are all identical (upside down) so 1 is kept
as the first unique rotation. The `rotation` sheet also has a column for
each angle (1 to 9), where the value for each component is whether that
angle is correct if it appears (NA), needs to be corrected to no
rotation (0), or needs to be corrected to a different rotation (1 to 9).

All letter variations were converted to single categories. For example,
A1 and A2, two stylistic variations of A with square and rounded tops,
were both converted to A. The letter I is used primarily for vertical
lines, so the other single line symbols (i.e. /, \\, and -) were
converted to I (with their associated angles). 1, which could be easily
mistaken for I, does not appear in the coding scheme. Double and triple
lines are stored as separate symbols (i.e. = and -3). We chose to keep
these as separate categories rather than convert them to multiple
repetitions of I, as reducing components to their constituent parts
introduces many other issues and could inflate the measured complexity
(or number of components) of some brands.

), QC, and QC1 were all converted to ( (with their associated angles).
\], BX1, and BX2 were all converted to \[ (with their associated
angles). DI (diamond) was converted to BX (box) with the associated
angle. All of the \~ components (i.e. \~1 through \~6), which correspond
to pairs of “squiggles” or “wings” that modify other components, do not
seem to have systematic differences or angles and were combined into a
single category without rotation.

MU, an unused code for music notes, was converted to NT, a code for
notation that appears in the brand books. ^ was converted to A3, a code
used for the same symbol in the brand books.

Several other simplifications were considered (with angles), such as
converting + to X, 9 to 6, W to M, etc. For now we chose to keep these
components separate.

``` r
#load in adjustments from components.xlsx
collapse <- data.table::as.data.table(readxl::read_excel(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "components.xlsx"), sheet = "collapse"))
rotation <- data.table::as.data.table(readxl::read_excel(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "components.xlsx"), sheet = "rotation"))

#reformat rotation
rotation$rot <- strsplit(rotation$rot, ", ")
```

``` r
#for each brand
for(i in 1:nrow(brands)){
  #check if there are characters that need replacing (either three digit characters like BX1 and QC1 or substringed characters without commas)
  temp <- which(brands$brand[[i]][1:4] %in% collapse$from | gsub(",", "", substr(brands$brand[[i]][1:4], 1, 2)) %in% collapse$from)
  
  #and if there are
  if(length(temp) > 0){
    #go through them
    for(j in 1:length(temp)){
      #if the full three-digit component is in the data table (so something like BX1, QC1, etc.)
      if(brands$brand[[i]][temp[j]] %in% collapse$from){
        #store the index of the component
        temp_2 <- which(collapse$from == brands$brand[[i]][temp[j]])
        
        #and either replace it without or with rotation
        if(collapse$rot[temp_2] == "NA"){
          brands$brand[[i]][temp[j]] <- stringr::str_pad(collapse$to[temp_2], width = 3, side = "right", pad = ",")
        } else{
          brands$brand[[i]][temp[j]] <- paste0(stringr::str_pad(collapse$to[temp_2], width = 2, side = "right", pad = ","), collapse$rot[temp_2])
        }
      } else{ #if it's less than three digits
        #store the index of the component
        temp_2 <- which(collapse$from == gsub(",", "", substr(brands$brand[[i]][temp[j]], 1, 2)))
        
        #and either replace it without or with rotation
        if(collapse$rot[temp_2] == "NA"){
          brands$brand[[i]][temp[j]] <- stringr::str_pad(collapse$to[temp_2], width = 3, side = "right", pad = ",")
        } else{
          brands$brand[[i]][temp[j]] <- paste0(stringr::str_pad(collapse$to[temp_2], width = 2, side = "right", pad = ","), collapse$rot[temp_2])
        }
      }
      
      rm(temp_2)
    }
  }
  
  rm(temp)
}
```

To determine which angles are possible for each symbol we went through
and assigned each one a five-digit binary code, where each digit
corresponds to whether (0 or 1) each symbol matches itself when flipped
horizontally, flipped vertically, rotated 90 degrees, rotated 180
degrees, and rotated 45 degrees. Then, for each five-digit code, we
determined which rotation angles were possible. Some extra rotation
angles were manually removed for character pairs like + and X, 9 and 6,
and W and M, and some rotations angles had to be corrected when the
binary code didn’t quite capture the symbol, such as for %.

Now let’s run through and correct any rotated components where the angle
does not match the vector of first unique rotation angles for that
component.

``` r
#for each brand
for(i in 1:nrow(brands)){
  #check if there are rotated components
  temp <- which(substr(brands$brand[[i]][1:4], 3, 3) %in% c(1:9))
  
  #if there are
  if(length(temp) > 0){
    #go through them
    for(j in 1:length(temp)){
      #store the index of the component
      temp_2 <- which(rotation$component == gsub(",", "", substr(brands$brand[[i]][temp[j]], 1, 2)))
      
      #store the current angle of rotation
      angle <- substr(brands$brand[[i]][temp[j]], 3, 3)
      
      #if the rotated component matches a real component (checking since we haven't removed misspecified components yet)
      if(length(temp_2) > 0){
        #and the rotation angle is not in the first unique rotation angles for that component
        if(!(angle %in% rotation$rot[[temp_2]])){
          #and the new rotation is not zero
          if(as.numeric(rotation[temp_2, ..angle]) > 0){
            #replace the current rotated component with a corrected version
            brands$brand[[i]][temp[j]] <- paste0(substr(brands$brand[[i]][temp[j]], 1, 2), as.numeric(rotation[temp_2, ..angle]))
          } else{ #if the new rotation is zero
            #remove the rotation entirely
            brands$brand[[i]][temp[j]] <- paste0(substr(brands$brand[[i]][temp[j]], 1, 2), ",")
          }
          
        }
      }
      
      rm(list = c("temp_2", "angle"))
    }
  }
  
  rm(temp)
}
```

Now we need to store the brand components. These include the components
listed in the brand book indices, as well as some additions that appear
in the real data (i.e. “TX,”). The “UN,” component was excluded as it
corresponds to unidentifiable symbols.

``` r
#store components with commas
components <- stringr::str_pad(rotation$component, width = 3, side = "right", pad = ",")

#print them
components
```

    ##   [1] "A,," "B,," "C,," "D,," "E,," "F,," "G,," "H,," "I,," "J,," "K,," "L,,"
    ##  [13] "M,," "N,," "O,," "P,," "Q,," "R,," "S,," "T,," "U,," "V,," "W,," "X,,"
    ##  [25] "Y,," "Z,," "#,," "$,," "%,," "(,," "[,," "*,," "?,," "+,," "=,," "-3,"
    ##  [37] "~,," "2,," "3,," "4,," "5,," "6,," "7,," "8,," "9,," "A3," "AH," "AN,"
    ##  [49] "AP," "AR," "BA," "BB," "BE," "BF," "BI," "BO," "BT," "BU," "BX," "CA,"
    ##  [61] "CC," "CM," "CO," "CP," "CR," "FB," "FH," "FI," "FL," "FO," "GU," "HA,"
    ##  [73] "HC," "HK," "HR," "HT," "IN," "KS," "LA," "LI," "MA," "MI," "MO," "NT,"
    ##  [85] "O1," "OY," "PI," "PR," "RE," "SA," "SC," "SD," "SH," "SN," "SP," "SU,"
    ##  [97] "SK," "SR," "T1," "TA," "TK," "TP," "TR," "TU," "TX," "UM," "WA,"

We also need to generate all possible components, accounting for
rotation, alongside index components in a data frame so rotated versions
can be easily converted to non-rotated versions later on. Since we have
already corrected incorrect angles we will only allow for angles that
appear in the vector of first unique rotation angles for that component.

``` r
#create empty vectors to fill
all_poss_components <- c()
index_components <- c()

#iterate through components
for(x in 1:length(components)){
  #if the component is not rotatable
  if(length(which(rotation$rot[[x]] == "NA")) > 0){
    #add it without rotation
    all_poss_components <- c(all_poss_components, components[x])
    index_components <- c(index_components, components[x])
  } else{
    #add it with rotation
    all_poss_components <- c(all_poss_components, components[x], paste0(substr(components[x], 1, 2), rotation$rot[[x]]))
    index_components <- c(index_components, components[x], rep(components[x], length(paste0(substr(components[x], 1, 2), rotation$rot[[x]]))))
  }
}

#combine new vectors into a data frame and remove old variable
all_poss_components <- data.frame(index = index_components, rot = all_poss_components)
rm(index_components)
```

Now let’s remove any misspecified brands with components that don’t
appear in `all_poss_components`.

``` r
#remove all misspecified brands (with components that aren't letters or don't appear in all possible components)
misspecified <- which(sapply(1:nrow(brands), function(x){
  length(which(brands$brand[[x]][1:4] %in% c(all_poss_components$rot, ",,,")))
}) != 4)
brands <- brands[-misspecified,]
rm(misspecified)
```

Before we move any further, we should go ahead and retrieve our
geographic data and subset our brands to only include those registered
in the state of Kansas.

``` r
#get all kansas zip codes
data("zip_code_db", package = "zipcodeR")
zip_code_db <- zip_code_db[which(zip_code_db$state == "KS"), ]

#identify zip codes with missing location data
missing_locations <- which(is.na(zip_code_db$lat))

#save zip codes with missing locations, to manually fill as CSV file outside of R using Google Maps
#sink("location_data/missing_zips.txt")
#cat(zip_code_db$zipcode[missing_locations], sep = "\n")
#sink()

#add in found locations
found <- read.csv("location_data/missing_zips_found.txt", header = FALSE)
zip_code_db$lat[missing_locations] <- found$V2
zip_code_db$lng[missing_locations] <- found$V3

#construct all_zips, a data table with zip codes, counties, latitudes, and longitudes
all_zips <- data.table::data.table(zip = as.numeric(zip_code_db$zipcode), county = as.factor(zip_code_db$county), lat = zip_code_db$lat, lon = zip_code_db$lng)

#save
save(all_zips, file = "location_data/all_zips.RData")

#subset brands to only include those from kansas
brands <- brands[which(brands$location %in% all_zips$zip), ]

#remove temporary objects
rm(list = c("zip_code_db", "missing_locations", "found"))
```

For our agent-based model (ABM) we will also need the pairwise geodesic
distances between all of the zip codes in Kansas, so let’s go ahead and
collect that (and save the object after, because it takes a while).

``` r
#calculate pairwise geodesic distances (in kilometers) between zip codes
zip_dists <- geodist::geodist(all_zips[, 3:4], measure = "geodesic")/1000

#add row and column names
colnames(zip_dists) <- all_zips$zip
rownames(zip_dists) <- all_zips$zip

#save
save(zip_dists, file = "location_data/zip_dists.RData")
```

Now we need to remove brands that are duplicated within the same zip
code and year. When this occurs it’s usually one family or ranch that
has registered a single brand multiple times for different locations on
the animal. In our model, duplicated brand codes will correspond to
variations of the same combination of symbols independently of the
location on the animal.

``` r
#get concatenated brands with duplicate codes
concat_brands <- sapply(1:nrow(brands), function(x){
  #get four components and the duplicate code
  temp <- brands$brand[[x]][c(1:4, 6)]
  
  #if duplicate code is missing them remove the NA
  if(length(which(is.na(temp))) > 0){
    temp <- temp[-which(is.na(temp))]
  }
  
  #return a concatenated version
  paste0(temp, collapse = "")
})

#build data table to determine which rows are duplicated (accounting for zip code and year)
concat_brands <- data.table::data.table(brand = concat_brands, location = brands$location, year = brands$year)

#remove duplicated rows from the main data table
brands <- brands[-which(duplicated(concat_brands)),]

#remove temporary object
rm(concat_brands)
```

Now we need to turn our brand data into a numeric matrix so it is
compatible with our ABM. In this matrix, brands will be stored as a
vector of eight numbers, where the first four correspond to the
components (with zeroes for empty component codes) and the last four
correspond to the angles of rotation (with zeroes for unrotated
components). For this, we will just replace each component code with a
node denoting it’s position in the `components` vector, and assign a
zero to empty positions. We will also append zip codes and years.

Once brands are converted let’s go ahead and save the object so we don’t
have to execute all of this code every time.

``` r
#create empty matrix for converted brands
converted_brands <- matrix(0, nrow = nrow(brands), ncol = 10)

#append zip codes and years
converted_brands[, 9] <- as.numeric(brands$location)
converted_brands[, 10] <- as.numeric(brands$year)

#iterate through the brands
for(i in 1:nrow(brands)){
  #extract the index numbers of components (ignoring rotation), and replace empty values with 0
  brand_nums <- match(all_poss_components$index[match(brands$brand[[i]][1:4], all_poss_components$rot)], components)
  brand_nums[is.na(brand_nums)] <- 0
  
  #create empty vector of angles
  angle_nums <- rep(0, 4)
  
  #iterate through the components
  for(j in 1:length(brand_nums[!is.na(brand_nums)])){
    #if the component is not a letter
    #check if there are any characters that are different between the actual component and it's index (indicates rotation)
    temp <- as.numeric(setdiff(strsplit(all_poss_components$rot[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]],
                               strsplit(all_poss_components$index[match(brands$brand[[i]][j], all_poss_components$rot)], split = "")[[1]]))
    
    #if there are, replace the corresponding 0 in the vector of angles
    if(length(temp) > 0){angle_nums[j] <- temp}
  }
  
  #store numeric brands and angles in the matrix
  converted_brands[i, 1:4] <- brand_nums
  converted_brands[i, 5:8] <- angle_nums
  
  #remove temporary objects
  rm(list = c("brand_nums", "angle_nums", "temp"))
}

#rewrite brands and remove original brands
brands <- converted_brands
rm(converted_brands)

#save
save(brands, file = "converted_brands.RData")
```

Here’s a sample of what this matrix looks like.

``` r
brands[1:10,]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]  [,9] [,10]
    ##  [1,]   27    0    0    0    0    0    0    0 67563  1990
    ##  [2,]   27    0    0    0    0    0    0    0 67835  1990
    ##  [3,]   27    0    0    0    0    0    0    0 66092  1990
    ##  [4,]   28    0    0    0    0    0    0    0 66617  1990
    ##  [5,]   28    0    0    0    0    0    0    0 67576  1990
    ##  [6,]   28    0    0    0    0    0    0    0 67333  1990
    ##  [7,]   28    0    0    0    0    0    0    0 67104  1990
    ##  [8,]   28    0    0    0    0    0    0    0 67347  1990
    ##  [9,]   28    0    0    0    0    0    0    0 66428  1990
    ## [10,]   28    0    0    0    0    0    0    0 67869  1990

The total number of brands in the dataset, after removing duplicates
within zip codes, is 81,063. For brands with two components, 2,092 of
them only appear in 1990 (“old”) and 3,265 of them only appear in
2008-2016 (“young”). For brands with three components, 1,408 of them are
old and 4,925 of them are young. Only 2.06% of brands have four
components, and only 4.9% of component types are singletons (i.e. only
appear once). Here is a further breakdown of the brands in terms of the
four geographic quadrants described in the preregistration document, for
each combination of age and complexity:

    ##                NE  NW   SE   SW
    ## 2-comp old    502 379  579  632
    ## 3-comp old    279 220  448  461
    ## 2-comp young  917 605 1060  683
    ## 3-comp young 1239 806 1843 1037

In short, we have ample sample size to conduct all of the analyses
proposed in the preregistration document.

Finally, if a large number of brands are registered in cities, where
ranches are unlikely to be located, then it could be indicative of
deeper data quality issues. The four [most populous counties in
Kansas](http://www.usa.com/rank/kansas-state--population-density--county-rank.htm)
are Johnson, Wyandotte, Sedgwick, and Shawnee. Collectively, these
counties include the three biggest metropolitan areas: Kansas City,
Topeka, and Wichita.

``` r
#collect zip codes for top four counties
johnson <- zipcodeR::search_county("Johnson", "KS")
wyandotte <- zipcodeR::search_county("Wyandotte", "KS")
sedgwick <- zipcodeR::search_county("Sedgwick", "KS")
shawnee <- zipcodeR::search_county("Shawnee", "KS")

#compile p. o. box and normal zips
po_box_zips <- c(johnson$zipcode[which(johnson$zipcode_type == "PO Box")],
                 wyandotte$zipcode[which(wyandotte$zipcode_type == "PO Box")],
                 sedgwick$zipcode[which(sedgwick$zipcode_type == "PO Box")],
                 shawnee$zipcode[which(shawnee$zipcode_type == "PO Box")])
normal_zips <- c(johnson$zipcode[which(johnson$zipcode_type == "Standard")],
                 wyandotte$zipcode[which(wyandotte$zipcode_type == "Standard")],
                 sedgwick$zipcode[which(sedgwick$zipcode_type == "Standard")],
                 shawnee$zipcode[which(shawnee$zipcode_type == "Standard")])

#get proportion of brands that are in metropolitan areas, and in p. o. boxes
prop_metro <- length(which(brands[, 9] %in% c(po_box_zips, normal_zips)))/nrow(brands)
prop_po_box <- length(which(brands[, 9] %in% po_box_zips))/nrow(brands)
```

Luckily, it appears that only 3.49% of brands are registered in
metropolitan areas, and only 0.11% are registered to metropolitan P. O.
Boxes. This suggests that brand registration in cities is not a
significant issue.

## Description of the ABM

The agent-based model simulates ranchers creating new brands every year
based on the components (and optionally angles) present in the brands
used by ranchers around them, as well as dual constraints of simplicity
and complexity. For example, with the parameters in the ABM we can run a
model where ranchers copy components at a large geographic scale while
being as different as possible from their closest neighbors, and try to
create brands that are complex enough that they aren’t easily faked but
simple enough that are easily legible.

When a new brand is created, the components and angles around it are
sampled within two radii: one in which components and angles are more
likely to be used (henceforth copying), and one in which components and
angles are less likely to be used (henceforth distinctiveness). All of
the components present in each radii are compiled into frequency tables
that are used for weighted random sampling. Once components have been
sampled, all of the rotations of each component that appear in each
radii are compiled as well. The probability that a rancher uses a
particular component (or angle) is based on the frequency of it within
the copying radius, raised to an exponent *C*, and the inverse frequency
of it within the distinctiveness radius, raised to an exponent *D*.

<!-- $$P(x) = F_x^C \times \left(\frac{1}{F_x}\right)^D$$ -->
<center>
<img src="https://render.githubusercontent.com/render/math?math=P(x)%20=%20F_x^C%20%5Ctimes%20%5Cleft(%5Cfrac{1}{F_x}%5Cright)^D">
</center>

*C* and *D* control the strength of copying and distinctiveness in the
probability of adopting components and angles, where zero is neutrality,
one is proportional to their observed frequencies, and values greater
than one increase their influence beyond their observed frequencies.
When only a subset of angles are possible for a particular component
then only the frequencies of those particular angles are considered.

The map below shows an example of copying and distinctiveness radii (200
km and 100 km, respectively) around a target zip code, shown by the
green triangle. In this example, components and angles within the larger
copying radius (blue) would be more likely to be appear in the target
zip code, whereas brands within the smaller distinctiveness radius (red)
would be less likely to be appear. Here we plot the geographic centroids
of each zip code rather than their boundaries, as the only shapefiles
available at the zip code level are for the US Census’ “zip code
tabulation areas”, which do not include all locations in the dataset.

<img src="README_files/figure-gfm/unnamed-chunk-25-1.png" width="75%" style="display: block; margin: auto;" />

To simulate simplicity and complexity, the number of components in each
new brand is drawn from a Poisson distribution where *λ* is the
parameter of interest (henceforth complexity). Below is an example of
the normalized probabilities of creating a brand with between one and
four components when *λ* is 0.5, 3, and 15. Note that the normalization
here is only for plotting purposes - We will using the raw output of
`dpois` as the `prob` argument of base R’s `sample` function.

<img src="README_files/figure-gfm/unnamed-chunk-26-1.png" width="75%" style="display: block; margin: auto;" />

At the beginning of each year in the ABM a set of `n_new` brands is
created, where `n_new` is the average number of new brands that appear
each year in the observed data. Each new brand is assigned a zip code,
where the probability of each zip code is proportional to its frequency
in all years of the observed data. After new brands are created a set of
`n_old` random brands is removed, where `n_old` is the average number of
brands that disappear each year in the observed data.

The ABM is initialized with the earliest year of observed data and runs
through every year until the final year of observed data. In order to
fit the parameters of the ABM to the observed data we also need to
calculate a set of summary statistics that capture both the overall
diversity and spatial diversity of components and brands. Right now the
ABM collects the following summary statistics at the end of each year:

-   For components:
    -   Overall diversity:
        1.  Proportion of components that are the most common type
        2.  Proportion of components that are the most rare type
        3.  Shannon’s diversity index
        4.  Simpson’s diversity index
    -   Spatial diversity:
        1.  Jaccard index of beta diversity (zip codes)
        2.  Morisita-Horn index of beta diversity (zip codes)
        3.  Jaccard index of beta diversity (counties)
        4.  Morisita-Horn index of beta diversity (counties)
-   For brands:
    -   Overall diversity:
        1.  Mean Levenshtein distance (from random 10%)

For all diversity metrics calculate their Hill number counterparts,
because they are [measured on the same
scale](https://onlinelibrary.wiley.com/doi/10.1111/oik.07202) and
[better account for relative
abundance](https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/13-0133.1).
Shannon’s diversity index emphasizes more rare types whereas Simpson’s
diversity index emphasizes more common types. The Jaccard and
Morisita-Horn indices were similarly chosen for their complementarity.
The Morisita-Horn index is a commonly used
[abundance-based](https://onlinelibrary.wiley.com/doi/10.1111/j.1541-0420.2005.00489.x)
beta diversity index, whereas the Jaccard index is the most robust of
the
[incidence-based](https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecs2.2100)
beta diversity indices to sampling error. We calculate beta diversity at
both the zip code and county-level to assess spatial diversity at two
different resolutions. Diversity indices are not calculated at the level
of brands, since duplicated brands in the model are not actually the
same type (i.e. duplicates in the model and data correspond to slight
variations of the same set of components that are only coded as the
same). The mean Levenshtein distance (a.k.a. edit distance), or the
[minimum number of insertions, deletions, and substitions required to
convert one sequence to
another](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/adist.html),
is calculated from a random 10% of brands (specified by the
`edit_dist_prop` argument of the ABM function).

The ABM parameters will be fit to the observed data using the random
forest version of approximate Bayesian computation (ABC). Random forest
ABC is [robust to the number of summary
statistics](https://academic.oup.com/bioinformatics/article/32/6/859/1744513)
and even [ranks them according to their
importance](https://academic.oup.com/bioinformatics/article/35/10/1720/5132692),
which means that we will not have to [reduce the
dimensionality](https://projecteuclid.org/journals/statistical-science/volume-28/issue-2/A-Comparative-Review-of-Dimension-Reduction-Methods-in-Approximate-Bayesian/10.1214/12-STS406.full)
of our summary statistics prior to inference.

## Test of the ABM

The ABM functions are in the `cattlebrandABM.R` file. Please refer to
the (heavily commented) functions in this file for details.

``` r
source("cattlebrandABM.R")
```

Before test the ABM we need to calculate a couple of things - First the
probability of rotation, which for now is just the proportion of brands
in the full dataset that are rotated.

``` r
#probability of rotation (proportion of rotated brands in the full dataset)
rot_prob <- as.numeric(1-(table(brands[, 5:8])[1]/sum(table(brands[, 5:8]))))
```

Now we can subset the brand data to (1) calculate the average number of
new and old brands per year, and (2) have the first year of data
separated to initialize the model with.

``` r
#separate brands data by year
brands_1990 <- data.table::data.table(brands[which(brands[, 10] == 1990), 1:9])
brands_2008 <- data.table::data.table(brands[which(brands[, 10] == 2008), 1:9])
brands_2014 <- data.table::data.table(brands[which(brands[, 10] == 2014), 1:9])
brands_2015 <- data.table::data.table(brands[which(brands[, 10] == 2015), 1:9])
brands_2016 <- data.table::data.table(brands[which(brands[, 10] == 2016), 1:9])

#calculate average number of new brands that appear each year and old brands that disappear each year (fsetdiff gets rows of first that are not in second)
n_new <- mean(c((nrow(data.table::fsetdiff(brands_2008, brands_1990))/18),
                (nrow(data.table::fsetdiff(brands_2014, brands_2008))/6),
                nrow(data.table::fsetdiff(brands_2015, brands_2014)),
                nrow(data.table::fsetdiff(brands_2016, brands_2015))))
n_old <- mean(c((nrow(data.table::fsetdiff(brands_1990, brands_2008))/18),
                (nrow(data.table::fsetdiff(brands_2008, brands_2014))/6),
                nrow(data.table::fsetdiff(brands_2014, brands_2015)),
                nrow(data.table::fsetdiff(brands_2015, brands_2016))))
```

We also need to reshape components so it includes whether or not a
component is rotatable (or includes at least one comma).

``` r
#reshape components
components <- data.table::data.table(components = components, rotatable = rotation$rot)
components$rotatable[which(components$rotatable == "NA")] <- NA
components$rotatable <- sapply(1:nrow(components), function(x){as.numeric(components$rotatable[[x]])})
```

Eventually we can use the code below to set the limits of our prior for
the sizes of the two radii.

``` r
#get min and max distances between zip codes (to inform prior range)
round(min(zip_dists[which(zip_dists != 0)]))
```

    ## [1] 0

``` r
round(max(zip_dists))
```

    ## [1] 688

Okay, now we are ready to do a test run of the ABM. We’ll set the
complexity (*λ*) to 3, the copying radius to 200 km, the distinctive
radius to 100 km, and the strength of both copying and distinctiveness
to 1. The model will be initialized with the data from 1990, and summary
statistics will be collected for 2008, 2014, 2015, and 2016. We’ll also
run one model that ignores the angles of components, and one that takes
it into account

``` r
#test out the components-only ABM (and get runtime)
start <- Sys.time()
components_only <- cattlebrandABM(init_brands = as.matrix(brands_1990), components, all_zips, zip_dists,
                                  init_year = 1990, sampling_years = c(2008, 2014, 2015, 2016), n_new, n_old,
                                  rot_prob, complexity = 3, copy_radius = 200, copy_strength = 1,
                                  dist_radius = 100, dist_strength = 1, angles = FALSE, edit_dist_prop = 0.1)
Sys.time() - start
```

    ## Time difference of 23.24493 secs

``` r
#print output
components_only
```

    ##       comp_most  comp_least comp_shannon comp_simpson comp_jac_zip comp_mh_zip
    ## 2008 0.09820768 0.002827685     65.98462     40.00909    0.3063715   0.3786883
    ## 2014 0.08484781 0.003692927     74.58071     48.30082    0.3273130   0.3741008
    ## 2015 0.08208150 0.003845462     76.02231     50.01580    0.3304529   0.3724728
    ## 2016 0.07988239 0.003957933     77.27162     51.53872    0.3325422   0.3716157
    ##      comp_jac_county comp_mh_county brand_edit
    ## 2008       0.7943925      0.8274049   2.608831
    ## 2014       0.8278594      0.8054026   2.730226
    ## 2015       0.8339119      0.8005737   2.721898
    ## 2016       0.8356030      0.7966296   2.716801

``` r
#test out the components and angles ABM (and get runtime)
start <- Sys.time()
components_angles <- cattlebrandABM(init_brands = as.matrix(brands_2008), components, all_zips, zip_dists,
                                    init_year = 1990, sampling_years = c(2008, 2014, 2015, 2016), n_new, n_old, 
                                    rot_prob, complexity = 3, copy_radius = 200, copy_strength = 1, 
                                    dist_radius = 100, dist_strength = 1, angles = TRUE, edit_dist_prop = 0.1)
Sys.time() - start
```

    ## Time difference of 26.64531 secs

``` r
#print output
components_angles
```

    ##       comp_most   comp_least comp_shannon comp_simpson comp_jac_zip comp_mh_zip
    ## 2008 0.04440739 3.088135e-05     179.9646     100.9164   0.05513437   0.1848249
    ## 2014 0.03560417 3.245594e-05     201.6766     121.8063   0.05538766   0.1763187
    ## 2015 0.03425375 3.271609e-05     204.3047     125.1062   0.05531279   0.1745370
    ## 2016 0.03323752 3.303928e-05     206.5426     127.7181   0.05514262   0.1738468
    ##      comp_jac_county comp_mh_county brand_edit
    ## 2008       0.2188345      0.6458063   2.813861
    ## 2014       0.2200931      0.5769859   2.854269
    ## 2015       0.2201095      0.5643916   2.857064
    ## 2016       0.2192110      0.5778471   2.878541

The output of each model is a matrix with a row for each of the four
sampling years, and a column for each of the nine summary statistics
collected in that year. After some profiling and optimization (using
`profvis`) the runtime for the agent-based model is just barely fast
enough for generative inference. I tried to further optimize the way
that unique brands and component/angle combinations are handled by
leaving them as matrices of integers instead of converting them into
concatenated strings, but surprisingly the string method is
significantly faster.

Now let’s ensure that variation in our parameter values actually leads
to variation in our summary statistics. To do this, we’ll run 1,000
simulations of the model assuming the same radii as above but with
varying parameter values for `complexity`, `copy_strength`, and
`dist_strength`. By plotting each dynamic parameter value against the
summary statistics that they generate, we can insure that variation in
one corresponds to variation in the other.

``` r
#set number of simulations and cores
n_sims <- 1000
n_cores <- parallel::detectCores()-1

#get uniform priors for three main dynamic parameters
priors <- data.frame(complexity = runif(n_sims, min = 0.5, max = 15), 
                     copy_strength = runif(n_sims, min = 0, max = 5),
                     dist_strength = runif(n_sims, min = 0, max = 5))

#run simulations
sum_stats <- parallel::mclapply(1:n_sims, function(x){cattlebrandABM(init_brands = as.matrix(brands_2008), components, all_zips, zip_dists,
                                                                     init_year = 1990, sampling_years = c(2016), n_new, n_old,
                                                                     rot_prob, complexity = priors$complexity[x], copy_radius = 200,
                                                                     copy_strength = priors$copy_strength[x], dist_radius = 100,
                                                                     dist_strength = priors$dist_strength[x], angles = FALSE, edit_dist_prop = 0.1)}, mc.cores = n_cores)

#simplify and save output
sum_stats <- do.call("rbind", sum_stats)
sim_test <- cbind(priors, sum_stats)
save(sim_test, file = "sim_test.RData")
```

In this plot, each row corresponds to the nine summary statistics and
each column corresponds to the three dynamic parameters in the ABM (A:
complexity, B: copying strength, C: distinctiveness strength).

<img src="README_files/figure-gfm/unnamed-chunk-34-1.png" style="display: block; margin: auto;" />
