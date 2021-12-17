
# *preps the Vital Population Statistics
# Updates: 10.29.2020 	- Fixed spelling of Multnomah
# - Created a separate SJC only file
# - Created another population grouping for 0-17 for EBR 
# - Collapsed 5 NYC Counties into one NYC Site using the sum function.
# - Changed the FIPS for NYC Site to equal that of NY County (36061)
# Updates: 3.2.2021	-Fixed spelling of East Baton Rouge
# -Changed NYC to New York City
# -Added Ada (16001) and Shelby County (01117)
# Update(MK): 10/20/21 - Updated Shelby County FIPS code (should've be 047157)

# This code was written initially in Stata, but we are going to...
# accomplish the same tasks in R


# Import necessary packages
library("dplyr") # Data Manipulation 
library("stringr") # R String Manipulation Package
library("tidyr") # for pivot longer function
library("rio") # input output library
cdc_pop <- import("C:/Users/Reagan/Documents/ISLG/CDC Vital Population Statisitics/pcen_v2020_y1020.sas7bdat")



colnames(cdc_pop) <- tolower(colnames(cdc_pop))
# black_male <- filter(cdc_pop, age >= 18, racesex == 3)



# black_male$st_fips <- as.character(black_male$st_fips)

# length(which(str_length(black_male$st_fips) < 2))

# length(black_male$st_fips)

# which(str_length(black_male$st_fips) < 2)
# look at first five rows
#head(cdc_pop)

# Turn column names from uppercase to lowercase
colnames(cdc_pop) <- tolower(colnames(cdc_pop))

convert_st_fips <- function(df) {
  
  # convert state fips codes to a string
  st_fips <- as.character(df$st_fips)
  
  # Find out which state fips have a length less than two
  less_than_two <- which(str_length(st_fips) < 2)
  
  # loop through our index where fips codes are less than two...
  # and convert them to two character strings
  for (index in less_than_two) {
    
    if(nchar(st_fips[index]) < 2) {
      st_fips[index] <- paste("0", st_fips[index], sep = "")
    }
  }
  df$st_fips <- st_fips
  return(df)
}

convert_co_fips <- function(df) {
  
  # convert county fips codes to a string
  co_fips <- as.character(df$co_fips)
  
  # Find out which county fips codes have a length less than three
  less_than_three <- which(str_count(co_fips) < 3)
  
  # loop through our index where fips codes are less than two...
  # and convert them to three character strings
  for (index in less_than_three) {
    
    if (nchar(co_fips[index]) == 2) {
      co_fips[index] <- paste("0", co_fips[index], sep = "")
    }
  
    if (nchar(co_fips[index]) == 1) {
      co_fips[index] <- paste("00", co_fips[index], sep = "")
    }
  }
  
  df$co_fips <- co_fips
  return(df)
}



filter_and_fips_conversion <- function(df, age_filter, hisp_filter, racesex_filter) {
  
  if (length(racesex_filter > 1) & length(hisp_filter) > 1) {
    
      df <-filter(df, age >= age_filter, hisp %in% hisp_filter, racesex %in% racesex_filter)
    }
  
  else if (length(racesex_filter < 2) & length(hisp_filter) > 1) {
    
    df <-filter(df, age >= age_filter, hisp %in% hisp_filter, racesex == racesex_filter )
  }
  else if (length(racesex_filter > 1) & length(hisp_filter) < 2 ) {
    
    df <- filter(df, age >= age_filter, hisp == hisp_filter, racesex %in% racesex_filter)
  }
  
  else {
    df <- filter(df, age >= age_filter, hisp = hisp_filter, 
                 racesex == racesex_filter)
  }
  
  # convert fips codes
  # state fips
  df <- convert_st_fips(df)
  # county fips
  df <- convert_co_fips(df)
}
  
######################## Black Population ########################### 
  
##### Black any ethnicity #####

adult_male_black <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 3)
adult_female_black <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 4)
adult_black <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), c(3,4))
pop_black <- filter_and_fips_conversion(cdc_pop, 0, c(1,2), c(3,4))

#### Black non hispanic ####

adult_male_black_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 3)
adult_female_black_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 4)
adult_black_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, c(3,4))
pop_black_nh <- filter_and_fips_conversion(cdc_pop, 0, 1, c(3,4))

#### Black +17 population ####

adult_black_17 <- filter_and_fips_conversion(cdc_pop, 17, c(1,2), c(3,4))
adult_black_17_nh <- filter_and_fips_conversion(cdc_pop, 17, 1, c(3,4))


##################### White population ##############################

#### white any ethnicity #####

adult_male_white <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 1)
adult_female_white <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 2)
adult_white <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), c(1,2))
pop_white <- filter_and_fips_conversion(cdc_pop, 0, c(1,2), c(1,2))

#### white non hispanic ####

adult_male_white_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 1)
adult_female_white_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 1)
adult_white_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, c(1,2))
pop_white_nh <- filter_and_fips_conversion(cdc_pop, 0, 1, c(1,2))

#### White +17 population ####

adult_white_17 <- filter_and_fips_conversion(cdc_pop, 17, c(1,2), c(1,2))
adult_white_17_nh <- filter_and_fips_conversion(cdc_pop, 17, 1, c(1,2))


########### Native American Alaska Native ###################

#### Native American Alaska Native any ethnicity #####

adult_male_native_american <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 5)
adult_female_native_american <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 6)
adult_native_american <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), c(5,6))
pop_native_american <- filter_and_fips_conversion(cdc_pop, 0, c(1,2), c(5,6))

#### Native American Alaska Native non hispanic ####

adult_male_native_american_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 5)
adult_female_native_american_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 6)
adult_native_american_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, c(5,6))
pop_native_american_nh <- filter_and_fips_conversion(cdc_pop, 0, 1, c(5,6))

#### Native American Alaska Native +17 population ####

adult_native_american_17 <- filter_and_fips_conversion(cdc_pop, 17, c(1,2), c(5,6))
adult_native_american_17_nh <- filter_and_fips_conversion(cdc_pop, 17, 1, c(5,6))


########### Asian Pacific Islander  ###################

#### asian pacific islander any ethnicity #####

adult_male_asian <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 7)
adult_female_asian <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), 8)
adult_asian <- filter_and_fips_conversion(cdc_pop, 18, c(1,2), c(7,8))
pop_asian <- filter_and_fips_conversion(cdc_pop, 0, c(1,2), c(7,8))

#### asian pacific islander non hispanic ####

adult_male_asian_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 7)
adult_female_asian_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, 8)
adult_asian_nh <- filter_and_fips_conversion(cdc_pop, 18, 1, c(7,8))
pop_asian_nh <- filter_and_fips_conversion(cdc_pop, 0, 1, c(7,8))

#### asian pacific islander +17 population ####

adult_asian_17 <- filter_and_fips_conversion(cdc_pop, 17, c(1,2), c(7,8))
adult_asian_17_nh <- filter_and_fips_conversion(cdc_pop, 17, 1, c(7,8))


############# Hispanic population ###############

adult_hispanic <- filter(cdc_pop, age >= 18, hisp == 2)
pop_hisp <-filter(cdc_pop, hisp == 2)
adult_hispanic_17 <- filter(cdc_pop, age >= 17, hisp == 2)

# Convert st fips codes
adult_hispanic <- convert_st_fips(adult_hispanic)
pop_hisp <- convert_st_fips(pop_hisp)
adult_hispanic_17 <- convert_st_fips(adult_asian_17)

# Convert co fips codes
adult_hispanic <- convert_co_fips(adult_hispanic)
pop_hisp <- convert_co_fips(pop_hisp)
adult_hispanic_17 <- convert_co_fips(adult_asian_17)


########## Total Population ##################
adult_total_pop <- filter(cdc_pop, age >= 18)
adult_total_pop_17 <- filter(cdc_pop, age >= 17)

# Convert st fips codes
adult_total_pop <- convert_st_fips(adult_total_pop)
adult_total_pop_17 <- convert_st_fips(adult_total_pop_17)

# Convert co fips codes 
adult_total_pop <- convert_co_fips(adult_total_pop)
adult_total_pop_17 <- convert_co_fips(adult_total_pop_17)




############ 

.rs.restartR()