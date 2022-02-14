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


filter_aggregate_and_pivot <- function(df, age_filter, hisp_filter, racesex_filter, values_name) {
  
  # if filtering for a single racesex and non-hispanic
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

#### Black non hispanic ####
adult_male_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  black_male_filter, "pop_adult_male_black_non_hispanic")
adult_female_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    black_female_filter, "pop_adult_female_black_non_hispanic")
adult_black_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             black_any_gender_filter, "pop_adult_black_non_hispanic")
pop_black_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           black_any_gender_filter, "pop_total_black_non_hispanic")

#### Black +17 population ####
adult_black_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             black_any_gender_filter, "pop_adult_black_17_any_ethnicity")
adult_black_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                black_any_gender_filter, "pop_adult_black_17_non_hispanic")


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

#### white non hispanic ####
adult_male_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  white_male_filter, "pop_adult_male_white_non_hispanic")
adult_female_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    white_female_filter, "pop_adult_female_white_non_hispanic")
adult_white_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             white_any_gender_filter, "pop_adult_white_non_hispanic")
pop_white_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           white_any_gender_filter, "pop_total_white_non_hispanic")

#### White +17 population ####
adult_white_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             white_any_gender_filter, "pop_adult_white_17_any_ethnicity")
adult_white_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                white_any_gender_filter, "pop_adult_white_17_non_hispanic")


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

#### Native American Alaska Native non hispanic ####
adult_male_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                            native_american_male_filter, "pop_adult_male_AIAN_non_hispanic")
adult_female_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                              native_american_female_filter, "pop_adult_female_AIAN_non_hispanic")
adult_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                       native_american_any_gender_filter, "pop_adult_AIAN_non_hispanic")
pop_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                                     native_american_any_gender_filter, "pop_total_AIAN_non_hispanic")

#### Native American Alaska Native +17 population ####
adult_native_american_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                                       native_american_any_gender_filter, "pop_adult_AIAN_17_any_ethnicity")
adult_native_american_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                          native_american_any_gender_filter, "pop_adult_AIAN_17_non_hispanic")


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

#### asian pacific islander non hispanic ####
adult_male_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                  asian_male_filter, "pop_adult_male_API_non_hispanic")
adult_female_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                                    asian_female_filter, "pop_adult_female_API_non_hispanic")
adult_asian_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_filter, non_hispanic_filter, 
                                             asian_any_gender_filter, "pop_adult_API_non_hispanic")
pop_asian_nh <- filter_aggregate_and_pivot(cdc_pop, any_age_filter, non_hispanic_filter, 
                                           asian_any_gender_filter, "pop_total_API_non_hispanic")

#### asian pacific islander +17 population ####
adult_asian_17 <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_and_hispanic_filter, 
                                             asian_any_gender_filter, "pop_adult_API_17_any_ethnicity")
adult_asian_17_nh <- filter_aggregate_and_pivot(cdc_pop, adult_age_17_filter, non_hispanic_filter, 
                                                asian_any_gender_filter, "pop_adult_API_17_non_hispanic")


############# Hispanic population ###############
adult_hispanic <- filter(cdc_pop, age >= adult_age_filter, hisp == hispanic_filter)
pop_hisp <-filter(cdc_pop, hisp == hispanic_filter)
adult_hispanic_17 <- filter(cdc_pop, age >= adult_age_17_filter, hisp == hispanic_filter)

# pivot and aggregate hispanic
adult_hispanic <- aggregate_and_pivot(adult_hispanic, "pop_adult_hispanic_any_race")
pop_hisp <- aggregate_and_pivot(pop_hisp, "pop_total_hispanic_any_race")
adult_hispanic_17 <- aggregate_and_pivot(adult_hispanic_17, "pop_adult_hispanic_17_any_race")

########## Total Population ##################
adult_total_pop <- filter(cdc_pop, age >= adult_age_filter)
adult_total_pop_17 <- filter(cdc_pop, age >= adult_age_17_filter)

# pivot and aggregate total population
adult_total_pop <- aggregate_and_pivot(adult_total_pop, "pop_adult_any_ethnicity")
adult_total_pop_17 <- aggregate_and_pivot(adult_total_pop_17, "pop_total_any_ethnicity_17")
total_pop <- aggregate_and_pivot(cdc_pop, "pop_total_any_ethnicity")

# Make a list of all populations
pop_list <- list(total_pop, adult_total_pop, adult_total_pop_17, # Total populations
                 pop_hisp, adult_hispanic, adult_hispanic_17, # Hispanic populations
                 pop_black, adult_black, adult_male_black, adult_female_black, # Black total and adults
                 pop_black_nh, adult_black_nh, adult_male_black_nh, adult_female_black_nh, # Black non Hispanic totals and adults
                 adult_black_17, adult_black_17_nh, # Black 17
                 pop_white, adult_white, adult_male_white, adult_female_white, # White totals and adults
                 pop_white_nh, adult_white_nh, adult_male_white_nh, adult_female_white_nh, # White non Hispanic and adults
                 adult_white_17, adult_white_17_nh, # adult white 17
                 pop_native_american, adult_native_american, adult_male_native_american, adult_female_native_american, # Native American totals and adults
                 pop_native_american_nh, adult_native_american_nh, adult_male_native_american_nh, adult_female_native_american_nh, # Native American non Hispanic and adults
                 adult_native_american_17, adult_native_american_17_nh, # Native American 17
                 pop_asian, adult_asian, adult_male_asian, adult_female_asian, # Asian totals and adults
                 pop_asian_nh, adult_asian_nh, adult_male_asian_nh, adult_female_asian_nh, # Asian non Hispanic and adults
                 adult_asian_17, adult_asian_17_nh) # Asian 17

# Join all datasets into one dataframe using merge on fips_county_code_year and for loop

df_pop <- total_pop # initialize total_pop as first dataset. 
for (i in 2:length(pop_list)) {
  df_pop <- merge(df_pop, pop_list[i], by = "fips_state_county_code_year")
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
export(df_pop, "C:/Users/Reagan/Documents/ISLG/CDC Population Statistics/cdc_population/CDC Vital Population Statisitics/Datasets/cdc_populations_jail_pm_2010_2020.csv")
export(sjc_site_populations, "C:/Users/Reagan/Documents/ISLG/CDC Population Statistics/cdc_population/CDC Vital Population Statisitics/Datasets/sjc_site_populations_jail_pm_2010_2020.csv")

# excel
export(df_pop, "C:/Users/Reagan/Documents/ISLG/CDC Population Statistics/cdc_population/CDC Vital Population Statisitics/Datasets/cdc_populations_jail_pm_2010_2020.csv")
export(sjc_site_populations, "C:/Users/Reagan/Documents/ISLG/CDC Population Statistics/cdc_population/CDC Vital Population Statisitics/Datasets/cdc_populations_jail_pm_2010_2020.csv")



.rs.restartR()