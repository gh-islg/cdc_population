
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

########## Change Log ############

# - Make sure to only have st and co fips code...
# ...and pop total for final join



########## Change Log ###########
# install tibble package to insert row at a set place
# install.packages("tibble")


# Import necessary packages
library("dplyr") # Data Manipulation 
library("stringr") # R String Manipulation Package
library("tidyr") # for pivot longer function
library("rio") # input output library
library("tibble") # insert columns at set places
cdc_pop <- import("C:/Users/Reagan/Documents/ISLG/CDC Vital Population Statisitics/pcen_v2020_y1020.sas7bdat")



colnames(cdc_pop) <- tolower(colnames(cdc_pop))
# black_male <- filter(cdc_pop, age >= 18, racesex == 3)



# black_male$st_fips <- as.character(black_male$st_fips)

# length(which(str_length(black_male$st_fips) < 2))

# length(black_male$st_fips)

# which(str_length(black_male$st_fips) < 2)
# look at first five rows
#head(cdc_pop)

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


cdc_pop <- convert_st_fips(cdc_pop)
cdc_pop <- convert_co_fips(cdc_pop)


cdc_pop$fips_state_county_code <- paste(cdc_pop$st_fips, cdc_pop$co_fips, sep = "")

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


# $age <- as.character(cdc_pop$age)
# cdc_pop$hisp <- as.character(cdc_pop$hisp)
# cdc_pop$racesex <- as.character(cdc_pop$racesex)



filter_aggregate_and_pivot <- function(df, age_filter, hisp_filter, racesex_filter, values_name) {
  
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
  
  df <- aggregate(df[, 2:16], by = list(fips_state_county_code = df$fips_state_county_code), FUN = sum)
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
  
  df <- aggregate(df[, 2:16], by = list(fips_state_county_code = df$fips_state_county_code), FUN = sum)
  df <- pivot_longer(df, cols = starts_with("pop"), names_to = "year", 
                     names_prefix = "pop", values_to = values_name) 
  df <- select(df, -age, -hisp, -racesex)
  
  df$year <- as.character(df$year)
  
  df$fips_state_county_code_year <- paste(df$fips_state_county_code, df$year, sep = "_")
  
  # take fips_state_country_code and year out of columns only keep combined column for left join
  df <- select(df, fips_state_county_code_year, values_name)
}



# adult_black_sum <- aggregate(adult_black[, 2:12], by = list(fips_state_county_code = adult_black$fips_state_county_code), FUN = sum)


######################## Black Population ########################### 
  
##### Black any ethnicity #####

adult_male_black <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 3, "pop_adult_male_black_any_ethnicity")
adult_female_black <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 4, "pop_adult_female_black_any_ethnicity")
adult_black <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), c(3,4), "pop_adult_black_any_ethnicity")
pop_black <- filter_aggregate_and_pivot(cdc_pop, 0, c(1,2), c(3,4), "pop_total_black_any_ethnicity")

#### Black non hispanic ####

adult_male_black_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 3, "pop_adult_male_black_non_hispanic")
adult_female_black_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 4, "pop_adult_female_black_non_hispanic")
adult_black_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, c(3,4), "pop_adult_black_non_hispanic")
pop_black_nh <- filter_aggregate_and_pivot(cdc_pop, 0, 1, c(3,4), "pop_total_black_non_hispanic")

#### Black +17 population ####

adult_black_17 <- filter_aggregate_and_pivot(cdc_pop, 17, c(1,2), c(3,4), "pop_adult_black_17_any_ethnicity")
adult_black_17_nh <- filter_aggregate_and_pivot(cdc_pop, 17, 1, c(3,4), "pop_adult_black_17_non_hispanic")


##################### White population ##############################

#### white any ethnicity #####

adult_male_white <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 1, "pop_adult_male_white_any_ethnicity")
adult_female_white <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 2, "pop_adult_female_white_any_ethnicity")
adult_white <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), c(1,2), "pop_adult_white_any_ethnicity")
pop_white <- filter_aggregate_and_pivot(cdc_pop, 0, c(1,2), c(1,2), "pop_total_white_any_ethnicity")

#### white non hispanic ####

adult_male_white_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 1, "pop_adult_male_white_non_hispanic")
adult_female_white_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 1, "pop_adult_female_white_non_hispanic")
adult_white_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, c(1,2), "pop_adult_white_non_hispanic")
pop_white_nh <- filter_aggregate_and_pivot(cdc_pop, 0, 1, c(1,2), "pop_total_white_non_hispanic")

#### White +17 population ####

adult_white_17 <- filter_aggregate_and_pivot(cdc_pop, 17, c(1,2), c(1,2), "pop_adult_white_17_any_ethnicity")
adult_white_17_nh <- filter_aggregate_and_pivot(cdc_pop, 17, 1, c(1,2), "pop_adult_white_17_non_hispanic")


########### Native American Alaska Native ###################

#### Native American Alaska Native any ethnicity #####

adult_male_native_american <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 5, "pop_adult_male_AIAN_any_ethnicity")
adult_female_native_american <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 6, "pop_adult_female_AIAN_any_ethnicity")
adult_native_american <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), c(5,6), "pop_adult_AIAN_any_ethnicity")
pop_native_american <- filter_aggregate_and_pivot(cdc_pop, 0, c(1,2), c(5,6), "pop_total_AIAN_any_ethnicity")

#### Native American Alaska Native non hispanic ####

adult_male_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 5, "pop_adult_male_AIAN_non_hispanic")
adult_female_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 6, "pop_adult_female_AIAN_non_hispanic")
adult_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, c(5,6), "pop_adult_AIAN_non_hispanic")
pop_native_american_nh <- filter_aggregate_and_pivot(cdc_pop, 0, 1, c(5,6), "pop_total_AIAN_non_hispanic")

#### Native American Alaska Native +17 population ####

adult_native_american_17 <- filter_aggregate_and_pivot(cdc_pop, 17, c(1,2), c(5,6), "pop_adult_AIAN_17_any_ethnicity")
adult_native_american_17_nh <- filter_aggregate_and_pivot(cdc_pop, 17, 1, c(5,6), "pop_adult_AIAN_17_non_hispanic")


########### Asian Pacific Islander  ###################

#### asian pacific islander any ethnicity #####

adult_male_asian <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 7, "pop_adult_male_API_any_ethnicity")
adult_female_asian <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), 8, "pop_adult_female_API_any_ethnicity")
adult_asian <- filter_aggregate_and_pivot(cdc_pop, 18, c(1,2), c(7,8), "pop_adult_API_any_ethnicity")
pop_asian <- filter_aggregate_and_pivot(cdc_pop, 0, c(1,2), c(7,8), "pop_total_API_any_ethnicity")

#### asian pacific islander non hispanic ####

adult_male_asian_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 7, "pop_adult_male_API_non_hispanic")
adult_female_asian_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, 8, "pop_adult_female_API_non_hispanic")
adult_asian_nh <- filter_aggregate_and_pivot(cdc_pop, 18, 1, c(7,8), "pop_adult_API_non_hispanic")
pop_asian_nh <- filter_aggregate_and_pivot(cdc_pop, 0, 1, c(7,8), "pop_total_API_non_hispanic")

#### asian pacific islander +17 population ####

adult_asian_17 <- filter_aggregate_and_pivot(cdc_pop, 17, c(1,2), c(7,8), "pop_adult_API_17_any_ethnicity")
adult_asian_17_nh <- filter_aggregate_and_pivot(cdc_pop, 17, 1, c(7,8), "pop_adult_API_17_non_hispanic")


############# Hispanic population ###############

adult_hispanic <- filter(cdc_pop, age >= 18, hisp == 2)
pop_hisp <-filter(cdc_pop, hisp == 2)
adult_hispanic_17 <- filter(cdc_pop, age >= 17, hisp == 2)

# pivot and aggregate hispanic
adult_hispanic <- aggregate_and_pivot(adult_hispanic, "pop_adult_hispanic_any_race")
pop_hisp <- aggregate_and_pivot(pop_hisp, "pop_total_hispanic_any_race")
adult_hispanic_17 <- aggregate_and_pivot(adult_hispanic_17, "pop_adult_hispanic_17_any_race")

########## Total Population ##################

adult_total_pop <- filter(cdc_pop, age >= 18)
adult_total_pop_17 <- filter(cdc_pop, age >= 17)

# pivot and aggregate total population

adult_total_pop <- aggregate_and_pivot(adult_total_pop, "pop_adult_any_ethnicity")
adult_total_pop_17 <- aggregate_and_pivot(adult_total_pop_17, "pop_total_any_ethnicity_17")
total_pop <- aggregate_and_pivot(cdc_pop, "pop_total_any_ethnicity")

# Make a list of all populations
pop_list <- list(
                # Total populations
                 total_pop, adult_total_pop, adult_total_pop_17, 
                # Hispanic populations
                 pop_hisp, adult_hispanic, adult_hispanic_17, 
                # Black total and adults
                 pop_black, adult_black, adult_male_black, adult_female_black,
                # Black non hispanic totals and adults
                 pop_black_nh, adult_black_nh, adult_male_black_nh, adult_female_black_nh,
                # Black 17
                 adult_black_17, adult_black_17_nh, 
                # White totals and adults
                 pop_white, adult_white, adult_male_white, adult_female_white, 
                # White non hispanic and adults
                 pop_white_nh, adult_white_nh, adult_male_white_nh, adult_female_white_nh, 
                # adult white 17
                 adult_white_17, adult_white_17_nh, 
                # Native American totals and adults 
                 pop_native_american, adult_native_american, adult_male_native_american, adult_female_native_american,
                # Native American non hispanic and adults
                 pop_native_american_nh, adult_native_american_nh, adult_male_native_american_nh, adult_female_native_american_nh, 
                # Native American 17
                adult_native_american_17, adult_native_american_17_nh,
                # Asian totals and adults
                 pop_asian, adult_asian, adult_male_asian, adult_female_asian, 
                # Asian non hispanic and adults
                 pop_asian_nh, adult_asian_nh, adult_male_asian_nh, adult_female_asian_nh,
                # Asian 17
                adult_asian_17, adult_asian_17_nh)

# Join all datasets into one dataframe using merge on fips_county_code_year and for loop
# initalize total_pop as first dataset. 
df_pop <- total_pop
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
               # Kings County
               "New York City"="36047",
               # New York County
               "New York City"="36061",
               # Bronx County
               "New York City"="36005",
               # Richmond County
               "New York City"="36085",
               # Queens County
               "New York City"="36081", "Mecklenburg"="37119", "Los Angeles"="06037",
               "Allegheny"="42003", "Harris"="48201", "Clark"="32003",
               "Multnomah"="41051", "Spokane"="53063", "New Orleans"="22071",
               "Buncombe"="37021", "Cook"="17031", "East Baton Rouge"="22033",
               "Lake"="17097", "Lucas"="39095", "Milwaukee"="55079",
               "Missoula"="30063", "Palm Beach"="12099", "Philadelphia"="42101",
               "San Francisco"="06075", "St. Louis"="29189")



for (i in seq(1, length(site_names_dict))) {
  insert_rows <- df_pop$fips_state_county_code == site_names_dict[i]
  df_pop[insert_rows, "site_name"] <- names(site_names_dict[i])
}


View(df_pop)


.rs.restartR()