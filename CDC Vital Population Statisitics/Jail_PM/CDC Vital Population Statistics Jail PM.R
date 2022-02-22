# Brian Holliday
# Date: 2/14/2022


# Objective: 
# - This script takes CDC Population SAS dataset and totals these these populations...
# - by year, race, sex, age, and hispanic origin
# - This is used to get the populations for our sjc sites.
# This code was written initially in Stata, but we are going to...
# accomplish the same tasks in R
# Make populations to join to the Jail PM Quarterly Data

########## Change Log ############

# - Make sure to only have st and co fips code...
# ...and pop total for final join
# install tibble package to insert row at a set place
# install.packages("tibble")
# add global variables for filtering function calls
# export two sets of files ones with sites and without (csv and excel format)
# change wd for upload to public or research drives
# take male and female populations out of the final output
# Add male and female populations back to the output 

######### To Do #################

# Add young, middle age, and older populations to the data



# Import necessary packages
library("dplyr") # Data Manipulation 
library("stringr") # R String Manipulation Package
library("tidyr") # for pivot longer function
library("rio") # input output library
library("tibble") # insert columns at set places

# import data using rio package
cdc_pop <- import("C:/Users/Reagan/Documents/CDC Vital Population Statisitics/pcen_v2020_y1020.sas7bdat")

colnames(cdc_pop) <- tolower(colnames(cdc_pop))

convert_st_fips <- function(df) {
  
  st_fips <- as.character(df$st_fips) # convert state fips codes to a string
  
  # Find out which state fips have a length less than two
  less_than_two <- which(str_length(st_fips) < 2)
  
  # loop through our index where fips codes are less than two...
  # and convert them into two character strings
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


cdc_pop <- convert_st_fips(cdc_pop)
cdc_pop <- convert_co_fips(cdc_pop)

# Create column to combine st fips and county fips codes 
cdc_pop$fips_state_county_code <- paste(cdc_pop$st_fips, cdc_pop$co_fips, sep = "")

# Create column to combine fips codes and year
cdc_pop$fips_state_county_code_year <- paste(cdc_pop$fips_state_county_code, cdc_pop$year, sep = " ")

# Create site_name column
cdc_pop$site_name <- NA

cdc_pop <- select(cdc_pop, fips_state_county_code, site_name,age, 
                  hisp, racesex, pop2010_jul, pop2011, pop2012, pop2013, pop2014, pop2015, 
                  pop2016, pop2017, pop2018, pop2019, pop2020)

colnames(cdc_pop) <- c("fips_state_county_code", "site_name", "age", "hisp", "racesex", 
                       "pop2010", "pop2011", "pop2012", "pop2013", "pop2014", 
                       "pop2015", "pop2016", "pop2017", "pop2018", "pop2019", 
                       "pop2020")

######## filtering variables ########

# age
adult_age_filter <- 18
adult_age_17_filter <- 17
any_age_filter <- 0
# young population
young_low <- 0
young_high <- 24
# middle age population
middle_low <- 25
middle_high <- 44
# older population
older_low <- 45
older_high <- 150

# hispanic
non_and_hispanic_filter <- c(1,2)
non_hispanic_filter <- 1
hispanic_filter <- 2

# black
black_male_filter <- 3
black_female_filter <- 4
black_any_gender_filter <- c(3,4)

# white
white_male_filter <- 1
white_female_filter <- 2
white_any_gender_filter <- c(1,2)

# native american Alaskan native
native_american_male_filter <- 5
native_american_female_filter <- 6
native_american_any_gender_filter <- c(5,6)

# asian pacific islander
asian_male_filter <- 7
asian_female_filter <- 8
asian_any_gender_filter <- c(7,8)

# Combinations
all_males <- c(1, 3, 5, 7)
all_female <- c(2, 4, 6, 8)
poc <- c(3, 4, 5, 6, 7, 8)
poc_males <- c(1, 3, 5, 7)
poc_females <- c(2, 4, 6, 8)

filter_aggregate_and_pivot <- function(df, age_filter, hisp_filter, racesex_filter, values_name, age_groups) {
  
  
  # if filtering for more than one racesex and with hispanic and non_hispanic
  if (length(racesex_filter > 1) & length(hisp_filter) > 1) {
    
      df <-filter(df, age >= age_filter, hisp %in% hisp_filter, racesex %in% racesex_filter)
  }
  
  # if filtering for a single racesex with hispanics and non-hispanic
  else if (length(racesex_filter < 2) & length(hisp_filter) > 1) {
    
    df <-filter(df, age >= age_filter, hisp %in% hisp_filter, racesex == racesex_filter )
  }
  
  # if filtering for more than one racesex and non-hispanic
  else if (length(racesex_filter > 1) & length(hisp_filter) < 2 ) {
    
    df <- filter(df, age >= age_filter, hisp == hisp_filter, racesex %in% racesex_filter)
  }
  
  # if filtering for more than more than one racesex with hispanics and non-hispanics
  else {
    df <- filter(df, age >= age_filter, hisp = hisp_filter, 
                 racesex == racesex_filter)
  }
  
  # sum totals for each year and fips code
  df <- aggregate(df[, 2:16], by = list(fips_state_county_code = df$fips_state_county_code), FUN = sum)
  # pivot years for wide to long
  df <- pivot_longer(df, cols = starts_with("pop"), names_to = "year", 
                     names_prefix = "pop", values_to = values_name)
  df <- select(df, -age, -hisp, -racesex)
  
  df$year <- as.character(df$year)
  
  df$fips_state_county_code_year <- paste(df$fips_state_county_code, df$year, sep = "_") 
  
  # take fips_state_country_code and year out of columns only keep combined column for left join
  df <- select(df, fips_state_county_code_year, values_name)  
  
  return(df)
}

filter_aggregate_and_pivot_age_groups <- function(df, hisp_filter, racesex_filter, age_group, values_name) {
  
  # determine the age group being filtered
  if(age_group == "young") { # young age
    low <- young_low
    high <- young_high
  }
  
  if (age_group == "middle") { # middle age
    low <- middle_low
    high <- middle_high
  }
  
  if (age_group == "older") { # old age
    low <- older_low
    high <- older_high
  }
  
  ############## Dataset filters ###########################
  # if filtering for more than one racesex and with hispanic and non_hispanic
  if (length(racesex_filter > 1) & length(hisp_filter) > 1) {
    
    df <-filter(df, age >= low & age <= high, hisp %in% hisp_filter, racesex %in% racesex_filter)
  }
  
  # if filtering for a single racesex with hispanics and non-hispanic
  else if (length(racesex_filter < 2) & length(hisp_filter) > 1) {
    
    df <-filter(df, age >= low & age <= high, hisp %in% hisp_filter, racesex == racesex_filter )
  }
  
  # if filtering for more than one racesex and non-hispanic
  else if (length(racesex_filter > 1) & length(hisp_filter) < 2 ) {
    
    df <- filter(df, age >= low & age <= high, hisp == hisp_filter, racesex %in% racesex_filter)
  }
  
  # if filtering for more than more than one racesex with hispanics and non-hispanics
  else {
    df <- filter(df, age >= low & age <= high, hisp = hisp_filter, racesex == racesex_filter)
  }
  
  # sum totals for each year and fips code
  df <- aggregate(df[, 2:16], by = list(fips_state_county_code = df$fips_state_county_code), FUN = sum)
  # pivot years for wide to long
  df <- pivot_longer(df, cols = starts_with("pop"), names_to = "year", 
                     names_prefix = "pop", values_to = values_name)
  df <- select(df, -age, -hisp, -racesex)
  
  df$year <- as.character(df$year)
  
  df$fips_state_county_code_year <- paste(df$fips_state_county_code, df$year, sep = "_") 
  
  # take fips_state_country_code and year out of columns only keep combined column for left join
  df <- select(df, fips_state_county_code_year, values_name)  
  
  return(df)
}

aggregate_and_pivot <- function(df, values_name) {
  
  # sum totals for each year and fips code
  df <- aggregate(df[, 2:16], by = list(fips_state_county_code = df$fips_state_county_code), FUN = sum)
  
  # pivot years for wide to long
  df <- pivot_longer(df, cols = starts_with("pop"), names_to = "year", 
                     names_prefix = "pop", values_to = values_name)
  df <- select(df, -age, -hisp, -racesex)
  df$year <- as.character(df$year)
  
  df$fips_state_county_code_year <- paste(df$fips_state_county_code, df$year, sep = "_")
  
  # take fips_state_country_code and year out of columns only keep combined column for left join
  df <- select(df, fips_state_county_code_year, values_name)
}



######################## Black Population ########################### 
  
##### Black any ethnicity #####
adult_male_black <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                               black_male_filter, "pop_adult_male_black_any_ethnicity")
adult_female_black <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                 black_female_filter, "pop_adult_female_black_any_ethnicity")
adult_black <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                          black_any_gender_filter, "pop_adult_black_any_ethnicity")
pop_black <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_and_hispanic_filter, 
                                        black_any_gender_filter, "pop_total_black_any_ethnicity")
black_young <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                            black_any_gender_filter, "young", "pop_young_black_any_ethnicity")
black_middle <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                black_any_gender_filter, "middle", "pop_middle_black_any_ethnicity")
black_older <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                 black_any_gender_filter, "older", "pop_older_black_any_ethnicity")

#### Black non hispanic ####
adult_male_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  black_male_filter, "pop_adult_male_black_non_hispanic")
adult_female_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    black_female_filter, "pop_adult_female_black_non_hispanic")
adult_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             black_any_gender_filter, "pop_adult_black_non_hispanic")
pop_black_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           black_any_gender_filter, "pop_total_black_non_hispanic")
black_young_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           black_any_gender_filter, "young", "pop_young_black_non_hispanic")
black_middle_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                            black_any_gender_filter, "middle", "pop_middle_black_non_hispanic")
black_older_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           black_any_gender_filter, "older", "pop_older_black_non_hispanic")



#### Black +17 population ####
adult_black_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             black_any_gender_filter, "pop_adult_black_17_any_ethnicity")
adult_male_black_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                               black_male_filter, "pop_adult_male_black_17_any_ethnicity")
adult_female_black_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                                 black_female_filter, "pop_adult_female_black_17_any_ethnicity")
adult_black_17_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                black_any_gender_filter, "pop_adult_black_17_non_hispanic")
adult_male_black_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                  black_male_filter, "pop_adult_male_black_17_non_hispanic")
adult_female_black_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                    black_female_filter, "pop_adult_female_17_black_non_hispanic")


##################### White population ##############################

#### white any ethnicity #####
adult_male_white <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                               white_male_filter, "pop_adult_male_white_any_ethnicity")
adult_female_white <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                 white_female_filter, "pop_adult_female_white_any_ethnicity")
adult_white <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                          white_any_gender_filter, "pop_adult_white_any_ethnicity")
pop_white <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_and_hispanic_filter, 
                                        white_any_gender_filter, "pop_total_white_any_ethnicity")
white_young <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                white_any_gender_filter, "young", "pop_young_white_any_ethnicity")
white_middle <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                 white_any_gender_filter, "middle", "pop_middle_white_any_ethnicity")
white_older <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                white_any_gender_filter, "older", "pop_older_white_any_ethnicity")

#### white non hispanic ####
adult_male_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  white_male_filter, "pop_adult_male_white_non_hispanic")
adult_female_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    white_female_filter, "pop_adult_female_white_non_hispanic")
adult_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             white_any_gender_filter, "pop_adult_white_non_hispanic")
pop_white_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           white_any_gender_filter, "pop_total_white_non_hispanic")
white_young_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           white_any_gender_filter, "young", "pop_young_white_non_hispanic")
white_middle_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                            white_any_gender_filter, "middle", "pop_middle_white_non_hispanic")
white_older_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           white_any_gender_filter, "older", "pop_older_white_non_hispanic")

#### White +17 population ####
adult_white_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             white_any_gender_filter, "pop_adult_white_17_any_ethnicity")
adult_white_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                white_any_gender_filter, "pop_adult_white_17_non_hispanic")
adult_male_white_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                               white_male_filter, "pop_adult_male_white_17_any_ethnicity")
adult_female_white_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                 white_female_filter, "pop_adult_female_white_17_any_ethnicity")
adult_male_white_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                  white_male_filter, "pop_adult_male_white_17_non_hispanic")
adult_female_white_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                    white_female_filter, "pop_adult_female_white_17_non_hispanic")


########### Native American Alaska Native ###################

#### Native American Alaska Native any ethnicity #####
adult_male_native_american <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter,
                                                         native_american_male_filter, "pop_adult_male_AIAN_any_ethnicity")
adult_female_native_american <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                           native_american_female_filter, "pop_adult_female_AIAN_any_ethnicity")
adult_native_american <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                    native_american_any_gender_filter, "pop_adult_AIAN_any_ethnicity")
pop_native_american <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_and_hispanic_filter, 
                                                  native_american_any_gender_filter, "pop_total_AIAN_any_ethnicity")
native_american_young <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                native_american_any_gender_filter, "young", "pop_young_AIAN_any_ethnicity")
native_american_middle <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                 native_american_any_gender_filter, "middle", "pop_middle_AIAN_any_ethnicity")
native_american_older <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                native_american_any_gender_filter, "older", "pop_older_AIAN_any_ethnicity")

#### Native American Alaska Native non hispanic ####
adult_male_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                            native_american_male_filter, "pop_adult_male_AIAN_non_hispanic")
adult_female_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                              native_american_female_filter, "pop_adult_female_AIAN_non_hispanic")
adult_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                       native_american_any_gender_filter, "pop_adult_AIAN_non_hispanic")
pop_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                                     native_american_any_gender_filter, "pop_total_AIAN_non_hispanic")
native_american_young_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                                     native_american_any_gender_filter, "young", "pop_young_AIAN_non_hispanic")
native_american_middle_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                                      native_american_any_gender_filter, "middle", "pop_middle_AIAN_non_hispanic")
native_american_older_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                                     native_american_any_gender_filter, "older", "pop_older_AIAN_non_hispanic")

#### Native American Alaska Native +17 population ####
adult_native_american_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                                       native_american_any_gender_filter, "pop_adult_AIAN_17_any_ethnicity")
adult_native_american_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                          native_american_any_gender_filter, "pop_adult_AIAN_17_non_hispanic")
adult_male_native_american_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter,
                                                         native_american_male_filter, "pop_adult_male_AIAN_17_any_ethnicity")
adult_female_native_american_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                                           native_american_female_filter, "pop_adult_female_AIAN_17_any_ethnicity")
adult_male_native_american_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                            native_american_male_filter, "pop_adult_male_AIAN_17_non_hispanic")
adult_female_native_american_nh_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                              native_american_female_filter, "pop_adult_female_AIAN_17_non_hispanic")

########### Asian Pacific Islander  ###################

#### asian pacific islander any ethnicity #####
adult_male_asian <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                               asian_male_filter, "pop_adult_male_API_any_ethnicity")
adult_female_asian <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                                 asian_female_filter, "pop_adult_female_API_any_ethnicity")
adult_asian <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                          asian_any_gender_filter, "pop_adult_API_any_ethnicity")
pop_asian <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_and_hispanic_filter, 
                                        asian_any_gender_filter, "pop_total_API_any_ethnicity")
asian_young <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                     asian_any_gender_filter, "young", "pop_young_API_any_ethnicity")
asian_middle <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                      asian_any_gender_filter, "middle", "pop_middle_API_any_ethnicity")
asian_older <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_and_hispanic_filter,
                                                                     asian_any_gender_filter, "older", "pop_older_API_any_ethnicity")

#### asian pacific islander non hispanic ####
adult_male_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  asian_male_filter, "pop_adult_male_API_non_hispanic")
adult_female_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    asian_female_filter, "pop_adult_female_API_non_hispanic")
adult_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             asian_any_gender_filter, "pop_adult_API_non_hispanic")
pop_asian_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           asian_any_gender_filter, "pop_total_API_non_hispanic")
asian_young_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           asian_any_gender_filter, "young", "pop_young_API_non_hispanic")
asian_middle_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                            asian_any_gender_filter, "middle", "pop_middle_API_non_hispanic")
asian_older_nh <- filter_aggregate_and_pivot_age_groups(cdc_pop, non_hispanic_filter,
                                                           asian_any_gender_filter, "older", "pop_older_API_non_hispanic")

#### asian pacific islander +17 population ####
adult_asian_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             asian_any_gender_filter, "pop_adult_API_17_any_ethnicity")
adult_asian_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                asian_any_gender_filter, "pop_adult_API_17_non_hispanic")
adult_male_asian_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_and_hispanic_filter, 
                                               asian_male_filter, "pop_adult_male_API_17_any_ethnicity")
adult_female_asian_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                                 asian_female_filter, "pop_adult_female_API_17_any_ethnicity")
adult_male_asian_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                  asian_male_filter, "pop_adult_male_API_non_hispanic")
adult_female_asian_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                    asian_female_filter, "pop_adult_female_API_non_hispanic")

############# Hispanic population ###############
pop_hisp <-filter(cdc_pop, hisp == hispanic_filter)
adult_hispanic <- filter(cdc_pop, age >= adult_age_filter, hisp == hispanic_filter)
adult_male_hispanic <- filter(cdc_pop, age >= adult_age_filter, hisp == hispanic_filter, racesex %in% all_males)
adult_female_hispanic <- filter(cdc_pop, age >= adult_age_filter, hisp == hispanic_filter, racesex %in% all_female)
############ Hispanic 17+ population ###############
adult_hispanic_17 <- filter(cdc_pop, age >= adult_age_17_filter, hisp == hispanic_filter)
adult_male_hispanic_17 <- filter(cdc_pop, age >= adult_age_17_filter, hisp == hispanic_filter, racesex %in% all_males)
adult_female_hispanic_17 <- filter(cdc_pop, age >= adult_age_17_filter, hisp == hispanic_filter, racesex %in% all_female)
########### Hispanic Age Groups ##################
hispanic_young <- filter(cdc_pop, age >= young_low & age <= young_high, hisp == hispanic_filter)
hispanic_middle <- filter(cdc_pop, age >= middle_low & age <= middle_high, hisp == hispanic_filter)
hispanic_older <- filter(cdc_pop, age >= older_low & age <= older_high, hisp == hispanic_filter)

# pivot and aggregate hispanic
adult_hispanic <- aggregate_and_pivot(adult_hispanic, "pop_adult_hispanic_any_race")
pop_hisp <- aggregate_and_pivot(pop_hisp, "pop_total_hispanic_any_race")
adult_male_hispanic <- aggregate_and_pivot(adult_male_hispanic, "pop_adult_male_hispanic_any_race")
adult_female_hispanic <- aggregate_and_pivot(adult_female_hispanic, "pop_adult_female_hispanic_any_race")
adult_hispanic_17 <- aggregate_and_pivot(adult_hispanic_17, "pop_adult_hispanic_17_any_race")
adult_male_hispanic_17 <- aggregate_and_pivot(adult_male_hispanic_17, "pop_adult_male_hispanic_17_any_race")
adult_female_hispanic_17 <- aggregate_and_pivot(adult_female_hispanic_17, "pop_adult_female_hispanic_17_any_race")
hispanic_young <- aggregate_and_pivot(hispanic_young, "pop_young_hispanic_any_race")
hispanic_middle <- aggregate_and_pivot(hispanic_middle, "pop_middle_hispanic_any_race")
hispanic_older <- aggregate_and_pivot(hispanic_older, "pop_older_hispanic_any_race")

########## POC Population ##################
total_poc <- filter(cdc_pop, racesex %in% poc)
# We need to add white Hispanics to people of color
white_hispanics <- filter(cdc_pop, racesex %in% white_any_gender_filter, hisp == hispanic_filter)
total_poc <- rbind(total_poc, white_hispanics)
adult_total_poc <- filter(total_poc, age >= adult_age_filter)
adult_male_poc <- filter(total_poc, age >= adult_age_filter, racesex %in% poc_males)
adult_female_poc <- filter(total_poc, age >= adult_age_filter, racesex %in% poc_females)
########## POC Population 17+ ###############
adult_total_poc_17 <- filter(total_poc, age >= adult_age_17_filter)
adult_male_poc_17 <- filter(total_poc, age >= adult_age_17_filter, racesex %in% poc_males)
adult_female_poc_17 <- filter(total_poc, age >= adult_age_17_filter, racesex %in% poc_females)
######### POC Population Age Groups ##########
total_poc_young <- filter(total_poc, age >= young_low & age <= young_high)
total_poc_middle <- filter(total_poc, age >= middle_low & age <= middle_high)
total_poc_older <- filter(total_poc, age >= older_low & age <= older_high)

# pivot and aggregate POC
total_poc <- aggregate_and_pivot(total_poc, "pop_total_poc")
adult_total_poc <- aggregate_and_pivot(adult_total_poc, "pop_adult_poc")
adult_male_poc <- aggregate_and_pivot(adult_male_poc, "pop_adult_male_poc")
adult_female_poc <- aggregate_and_pivot(adult_female_poc, "pop_adult_female_poc")
adult_total_poc_17 <- aggregate_and_pivot(adult_total_poc_17, "pop_adult_poc_17")
adult_male_poc_17 <- aggregate_and_pivot(adult_male_poc_17, "pop_adult_male_poc_17")
adult_female_poc_17 <- aggregate_and_pivot(adult_female_poc_17, "pop_adult_female_poc_17")
total_poc_young <- aggregate_and_pivot(total_poc_young, "pop_young_poc")
total_poc_middle <- aggregate_and_pivot(total_poc_middle, "pop_middle_poc")
total_poc_older <- aggregate_and_pivot(total_poc_older, "pop_older_poc")

########## Total Population ##################
adult_total_pop <- filter(cdc_pop, age >= adult_age_filter)
adult_male_total <- filter(cdc_pop, age >= adult_age_filter, racesex %in% all_males)
adult_female_total <- filter(cdc_pop, age >= adult_age_filter, racesex %in% all_female)
########## Total Population 17+ ###############
adult_total_pop_17 <- filter(cdc_pop, age >= adult_age_17_filter)
adult_male_total_pop_17 <- filter(cdc_pop, age >= adult_age_17_filter, racesex %in% all_males)
adult_female_total_pop_17 <- filter(cdc_pop, age >= adult_age_17_filter, racesex %in% all_female)
######### Total Population Age Groups ##########
total_pop_young <- filter(cdc_pop, age >= young_low & age <= young_high)
total_pop_middle <- filter(cdc_pop, age >= middle_low & age <= middle_high)
total_pop_older <- filter(cdc_pop, age >= older_low & age <= older_high)

# pivot and aggregate total population
total_pop <- aggregate_and_pivot(cdc_pop, "pop_total_any_ethnicity")
adult_total_pop <- aggregate_and_pivot(adult_total_pop, "pop_adult_any_ethnicity")
adult_male_total <- aggregate_and_pivot(adult_male_total, "pop_adult_male_any_ethnicity")
adult_female_total <- aggregate_and_pivot(adult_female_total, "pop_adult_female_any_ethnicity")
adult_total_pop_17 <- aggregate_and_pivot(adult_total_pop_17, "pop_adult_any_ethnicity_17")
adult_male_total_pop_17 <- aggregate_and_pivot(adult_male_total_pop_17, "pop_adult_male_any_ethnicity_17")
adult_female_total_pop_17 <- aggregate_and_pivot(adult_female_total_pop_17, "pop_adult_female_any_ethnicity_17")
total_pop_young <- aggregate_and_pivot(total_pop_young, "pop_young_any_ethnicity")
total_pop_middle <- aggregate_and_pivot(total_pop_middle, "pop_middle_any_ethnicity")
total_pop_older <- aggregate_and_pivot(total_pop_older, "pop_older_any_ethnicity")

# Make a list of all populations
jail_pm_pop_list <- list(total_pop, adult_total_pop, adult_male_total, adult_female_total, # Total populations
                 total_pop_young, total_pop_middle, total_pop_older, # Total populations age groups
                 total_poc, adult_total_poc, adult_male_poc, adult_female_poc, # Total POC
                 total_poc_young, total_poc_middle, total_poc_older, # Total POC age groups
                 pop_hisp, adult_hispanic, adult_male_hispanic, adult_female_hispanic, # Hispanic populations
                 hispanic_young, hispanic_middle, hispanic_older, # Hispanic Age Groups
                 pop_black_nh, adult_black_nh, adult_male_black_nh, adult_female_black_nh, # Black non Hispanic totals and adults
                 black_young_nh, black_middle_nh, black_older_nh, # Black non Hispanic age groups
                 pop_white_nh, adult_white_nh, adult_male_white_nh, adult_female_white_nh, # White non Hispanic
                 white_young_nh, white_middle_nh, white_older_nh, # White non Hispanic Age Groups
                 pop_native_american_nh, adult_native_american_nh, adult_male_native_american_nh, adult_female_native_american_nh, # Native American non Hispanic
                 native_american_young_nh, native_american_middle_nh, native_american_older_nh, # Native American Non Hispanic Age Groups
                 pop_asian_nh, adult_asian_nh, adult_male_asian_nh, adult_female_asian_nh, # Asian non Hispanic and adults
                 asian_young_nh, asian_middle_nh, asian_older_nh) # Asian non Hispanic Age Groups

# Join all datasets into one dataframe using merge on fips_county_code_year and for loop

df_pop <- total_pop # initialize total_pop as first dataset. 
for (i in 2:length(jail_pm_pop_list)) {
  df_pop <- merge(df_pop, jail_pm_pop_list[i], by = "fips_state_county_code_year")
}

# add site_name column in second place using tibble library
df_pop <- add_column(df_pop, site_name = NA, .after = "fips_state_county_code_year")
# add fips_state_county_code column after fips_state_county_code_year column using tibble library
df_pop <- add_column(df_pop, fips_state_county_code = substr(df_pop$fips_state_county_code_year, 1, 5),
                     .after = "fips_state_county_code_year")
# add year column after fips_state_country_code using tibble library
df_pop <- add_column(df_pop, year = substr(df_pop$fips_state_county_code_year, 7, 10),
                     .after = "fips_state_county_code")
# Change year to an integer
df_pop$year <- as.integer(df_pop$year)
# drop fips_state_county_code_year column
df_pop <- select(df_pop, -fips_state_county_code_year)

# Create site names dictionary to marry site names and with state and county codes
site_names_dict <- c("Ada"="16001", "Shelby"="47157", "Pennington"="46103", "Missoula"="30063",
               "Minnehaha"="46099", "Pima"="04019", "Charleston"="45019", 
               "New York City"="36047", # Kings County
               "New York City"="36061", # New York County
               "New York City"="36005", # Bronx County
               "New York City"="36085", # Richmond County
               "New York City"="36081", # Queens County
               "Mecklenburg"="37119", "Los Angeles"="06037",
               "Allegheny"="42003", "Harris"="48201", "Clark"="32003",
               "Multnomah"="41051", "Spokane"="53063", "New Orleans"="22071",
               "Buncombe"="37021", "Cook"="17031", "East Baton Rouge"="22033",
               "Lake"="17097", "Lucas"="39095", "Milwaukee"="55079",
               "Missoula"="30063", "Palm Beach"="12099", "Philadelphia"="42101",
               "San Francisco"="06075", "St. Louis"="29189")



# Use a for loop to join the individual populations
for (i in seq(1, length(site_names_dict))) {
  insert_rows <- df_pop$fips_state_county_code == site_names_dict[i]
  df_pop[insert_rows, "site_name"] <- names(site_names_dict[i])
}


# Create a dataframe for SJC Sites
missing_site_rows <- which(is.na(df_pop$site_name))
sjc_site_populations <- df_pop[-missing_site_rows, ]

# export datasets as csv and excel using export from rio package

# csv
export(df_pop, "C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/cdc_populations_jail_pm_2010_2020.csv")
export(sjc_site_populations, "C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/sjc_site_populations_jail_pm_2010_2020.csv")

# excel
export(df_pop, file = "C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/cdc_populations_jail_pm_2010_2020.xlsx",
       overwrite = TRUE)
export(sjc_site_populations, file = "C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/sjc_site_populations_jail_pm_2010_2020.xlsx",
       overwrite = TRUE)



.rs.restartR()