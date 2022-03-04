# Brian Holliday
# Jail PM Measurements Script
# 2/9/22

# Goals of this Script

# 1. Join Jail PM Data with most recent CDC Population Data from SJC Sites
# 2. Produce Quarterly Measurements for Jail PM Statistics for every race, ethnicity, and sub population and for each site

######### Change log #############
# 1. Add aggregated calc_n in all dataframes. 
# 2. Take all population and white population out of loop in ALOS Disparity Measure calculations
# 3. Keep booking rate for total population
# 4. Take out booking rate RRI for total population and white
# 5. Calculate measures at the monthly level
# 6. Import latest

# import libraries
library("plyr")
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

jail_pm_full <- read.csv("C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/monthly_jail_measures_all_sites_BLtoY5 3.3.22.csv") # import Jail PM Dataset

jail_pm_full <- jail_pm_full[!is.na(jail_pm_full$sjc_quarter), ] # Drop rows where sjc_quarter is null
jail_pm_full <- jail_pm_full[!is.na(jail_pm_full$year), ] # Drop rows where year is null
jail_pm_full <- jail_pm_full[!is.na(jail_pm_full$month), ] # Drop rows where month is null

###### Take out quarters after April 2021 ######
jail_pm_full_cohorts1and2 <- filter(jail_pm_full, cohort %in% c(1, 2), sjc_quarter <= 20)
jail_pm_full_cohort3 <- filter(jail_pm_full, cohort == 3, sjc_quarter <= 12)

jail_pm_full <- rbind(jail_pm_full_cohorts1and2, jail_pm_full_cohort3) # rind cohorts back together

# delete any quarters before April 2021 
jail_pm_full[jail_pm_full$pop_cat == "all_pop" & is.na(jail_pm_full$sub_pop), "sub_pop"] <- "all_pop_sub"

############# Join populations to jail_pm sheet ##################

#### Join Jail PM Data with most recent CDC Population Data ####
# import CDC Data

cdc_pop <- read.csv("C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/sjc_site_populations_jail_pm_2010_2020.csv")

# Create site names dictionary to marry site names and with state and county codes
jail_pm_names_dict <- c("Pennington"="PEN", "Pima"="PIM", 
                        "Charleston"="CHA", "Allegheny"="ALL", "Harris"="HAR",
                        "Multnomah"="MUL", "Spokane"="SPO", "New Orleans"="NOR",
                        "Buncombe"="BUN", "Cook"="COO", "Milwaukee"="MIL",
                        "Palm Beach"="PBC", "Philadelphia"="PHI",
                        "San Francisco"="SAN", "St. Louis"="STL")

# Use a for loop to join the individual populations
for (i in seq(1, length(jail_pm_names_dict))) {
  insert_rows <- cdc_pop$site_name == names(jail_pm_names_dict[i])
  cdc_pop[insert_rows, "site_name"] <- jail_pm_names_dict[i] 
}


# Put cdc_pop_long dataframe in quarterly format to row bind with quarterly dataset
cdc_pop_long <- pivot_longer(cdc_pop, cols = starts_with("pop"), values_to = "value")
cdc_pop_site <- cdc_pop_long$site_name
cdc_pop_long <- select(cdc_pop_long, -fips_state_county_code, -site_name)
cdc_pop_long <- add_column(cdc_pop_long, site = cdc_pop_site, .before = "year")
cdc_pop_long$calc_n <- NA

colnames(cdc_pop_long) <- c("site", "year", "measure", "value", "calc_n") # rename columns

# Filter out for jail_pm sites
cdc_pop_long <- filter(cdc_pop_long, site %in% unique(jail_pm_full$site))

cdc_pop_long <- add_column(cdc_pop_long, month = NA, .after = "year")
cdc_pop_long <- add_column(cdc_pop_long, cohort = NA, .after = "month")
cdc_pop_long <- add_column(cdc_pop_long, sjc_year = NA, .after = "cohort")
cdc_pop_long <- add_column(cdc_pop_long, sjc_quarter = NA, .after = "sjc_year")
cdc_pop_long <- add_column(cdc_pop_long, race_ethn = NA, .after = "sjc_quarter")
cdc_pop_long <- add_column(cdc_pop_long, pop_cat = NA, .after = "race_ethn")
cdc_pop_long <- add_column(cdc_pop_long, sub_pop = NA, .after = "pop_cat")

cohort_dict <- c("ALL"=3, "BUN"=3, "COO"=2,
                 "HAR"=1, "MIL"=1, "MUL"=2,
                 "NOR"=2, "PBC"=2, "PEN"=2,
                 "PHI"=1, "PIM"=1)

for (i in seq(length(cohort_dict))) {
  insert_rows <- cdc_pop_long$site == names(cohort_dict[i])
  cdc_pop_long[insert_rows, "cohort"] <- cohort_dict[i]
}

# limit to data that just have total population and total for each race
# I need to add more comming up soon
pop_adults <- c("pop_adult_any_ethnicity", "pop_adult_poc",                      
                "pop_adult_hispanic_any_race", "pop_adult_black_non_hispanic",       
                "pop_adult_white_non_hispanic", "pop_adult_AIAN_non_hispanic",
                "pop_adult_API_non_hispanic")
sjc_years <- c(2015, 2016, 2017, 2018, 2019, 2020) # filter sjc years
jail_pm_sites <- unique(jail_pm_full$site) # limit by sites that are in the jail_pm file 

# Limit cdc_pop_long to adult measures including any gender for now
cdc_pop_long <- filter(cdc_pop_long, site %in% jail_pm_sites, measure %in% pop_adults, year %in% sjc_years)

# populate Race/Ethnicity
pop_race_dict <- c("pop_adult_any_ethnicity"="all_race_ethn", "pop_adult_poc"="POC",                      
                   "pop_adult_hispanic_any_race"="L", "pop_adult_black_non_hispanic"="B",       
                   "pop_adult_white_non_hispanic"="W", "pop_adult_AIAN_non_hispanic"="AIAN",
                   "pop_adult_API_non_hispanic"="API")

# Use a for loop to join the race values to population dataset
for (i in seq(1, length(pop_race_dict))) {
  insert_rows <- cdc_pop_long$measure == names(pop_race_dict[i])
  cdc_pop_long[insert_rows, "race_ethn"] <- pop_race_dict[i]
}

cdc_pop_long$measure <- "gen_adult_pop" # This should be the variable name of all adult populations
cdc_pop_long$pop_cat <- "all_pop"
cdc_pop_long$calc_n <- NA



populate_prop_gen <- function(df) {
  final_df <- df # initialize final df
  # populate prop_gen_adult_pop
  for (current_site in unique(df$site)) {
    for (current_year in unique(df$year)) {
      for (race in unique(df$race_ethn)) {
        current_df <- filter(df, site == current_site, year == current_year)
        
        if (dim(current_df)[1] == 0) {
          next # if data.frame is empty move to the next iteration
        }
        
        else {
          total_pop <- current_df[current_df$race_ethn == "all_race_ethn", "value"] # get the total population value
          race_pop <- current_df[current_df$race_ethn == race, "value"] # get the population for the specific race
          current_cohort <- unique(current_df$cohort) # get current cohort
          # Calculate the proportion of general adult population for the specific race
          prop_gen_adult_pop <- round(race_pop / total_pop, 3)
          new_df <- data.frame(current_site, current_year, NA, current_cohort, NA,
                               NA, race, "all_pop", NA, "prop_gen_adult_pop", 
                               prop_gen_adult_pop, NA)
          
          colnames(new_df) <- colnames(df) # get column names from filtered df
          
          final_df <- rbind(final_df, new_df) # add new row to dataframe
        }
      }
    }
  }
  return(final_df)
}


cdc_pop_long <- populate_prop_gen(cdc_pop_long) # call populate_prop_gen function to get population rates

jail_pm_full <- rbind(jail_pm_full, cdc_pop_long) # rbind quarter_df and cdc_pop data.frames

jail_pm_full$year_month <- paste(as.character(jail_pm_full$year), as.character(jail_pm_full$month), sep = "_")


jail_pm_full$site <- as.character(jail_pm_full$site)
jail_pm_full$year <- as.integer(jail_pm_full$year)
jail_pm_full$month <- as.integer(jail_pm_full$month)
jail_pm_full$cohort <- as.integer(jail_pm_full$cohort)
jail_pm_full$sjc_year <- as.character(jail_pm_full$sjc_year)
jail_pm_full$sjc_quarter <- as.character(jail_pm_full$sjc_quarter)
jail_pm_full$race_ethn <- as.character(jail_pm_full$race_ethn)
jail_pm_full$pop_cat <- as.character(jail_pm_full$pop_cat)
jail_pm_full$sub_pop <- as.character(jail_pm_full$sub_pop)
jail_pm_full$measure <- as.character(jail_pm_full$measure)
jail_pm_full$value <- as.numeric(jail_pm_full$value)
jail_pm_full$calc_n <- as.integer(jail_pm_full$calc_n)
jail_pm_full$year_month <- as.character(jail_pm_full$year_month)


missing_calculations <- c()

######## Create loop to population measures at the monthly level ########

add_bookings_rates <- function(df) { # booking_rates function
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W")
  pop_cat_list <-  c("all_pop", "leg_stat_booking", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  bookings <- filter(df, measure == "bookings")
  bookings_year_month <- unique(bookings$year_month)
  sites <- unique(bookings$site)
  
  for (current_site in sites) {
    for (current_year_month in bookings_year_month) {
      for(race in race_ethn_list) {
        for (pop in pop_cat_list) { 
          if (pop == "all_pop") {
            sub_pop_list <- "all_pop_sub"
          }
          else if (pop == "leg_stat_booking") {
            sub_pop_list <- leg_stat_sub
          }
        
          else { # This means that pop_cat == severity
            sub_pop_list <- severity_sub
          }
          for (sub in sub_pop_list) {
          # We are just going to calculate for all_pop in this loop
            current_df <- filter(df, site == current_site, 
                                 year_month == current_year_month, 
                                 race_ethn == race, 
                                 pop_cat == pop, sub_pop == sub, measure == "bookings")
            current_df_white <- filter(df, site == current_site, year_month == current_year_month, 
                                     race_ethn == "W", pop_cat == pop, sub_pop == sub, measure == "bookings")
          
            if ((dim(current_df)[1] == 0)) {
              next # if filtered data.frame is empty move to the next iteration
            }
          
            else { # if the data frame has rows proceed to the calculation
            # get population for site and year
              current_year <- unique(current_df$year) # get current year
              cat("Booking", current_site, current_year_month, race, pop, sub, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
            
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
            
              pop_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == race,
                              pop_cat == "all_pop", measure == "gen_adult_pop") # filter for adult population
              pop_df_white <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == "W",
                                   pop_cat == "all_pop", measure == "gen_adult_pop") # filter for white adult population
            
              race_pop <- pop_df[, "value"] # get population
              race_pop_white <- pop_df_white[, "value"]
              bookings <- current_df[, "value"] # get bookings for that population
              bookings_white <- current_df_white[, "value"]
            
              booking_rate <- round((bookings / race_pop) * 100000, 3) # normalize per 100,000 people
            
              booking_rate_RRI <- round(((((bookings / race_pop)) * 100000) / 
                                         ((bookings_white / race_pop_white) * 100000)), 3)
              
              
            
              current_cohort <- unique(current_df$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df$sjc_year) # get sjc_year from filtered df
              current_year <- unique(current_df$year) # get current quarter
              current_month <- unique(current_df$month)
              current_sjc_quarter <- unique(current_df$sjc_quarter)
              
              if (dim(current_df_white)[1] == 0) {
                new_df <- data.frame(current_site, current_year, current_month, current_cohort,
                                   current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                   "booking_rate", booking_rate, NA, current_year_month)
                
                colnames(new_df) <- colnames(df) # get column names from current_df
                final_df <- rbind(final_df, new_df) # add new row before going to the next iteration
              }
              
              else {
                new_df <- data.frame(current_site, current_year, current_month, current_cohort,
                                     current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                     "booking_rate", booking_rate, NA, current_year_month)
                
                new_df_RRI <- data.frame(current_site, current_year, current_month, current_cohort,
                                       current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                       "booking_rate_RRI", booking_rate_RRI, NA, current_year_month)
                
                colnames(new_df) <- colnames(df) # get column names from current_df
                colnames(new_df_RRI) <- colnames(df)
                final_df <- rbind(final_df, new_df, new_df_RRI) # add new row before going to the next iteration
              }
            }
          }
        }
      }
    }
  }
  return(final_df)
}  
        
jail_pm_full <- add_bookings_rates(jail_pm_full) # add booking rates

add_prop_inc_rate_ADP_admrel <- function(df) { # add ADP admrel rates
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W") 
  pop_cat_list <-  c("all_pop", "leg_stat_booking", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  ADP_df <- filter(df, measure == "ADP_admrel")
  sites <- unique(ADP_df$site)
  ADP_month_year <- unique(ADP_df$year_month)
  
  for (current_site in sites) {
    for(current_year_month in ADP_month_year) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            sub_pop_list <- "all_pop_sub"
          }
          else if (pop == "leg_stat_booking") {
            sub_pop_list <- leg_stat_sub
          }
          else { # This means that pop_cat == severity
            sub_pop_list <- severity_sub
          }
          for (sub in sub_pop_list) {
            # We are just going to calculate for all_pop in this loop
            # ADP admrel 
            current_df_ADP_admrel <- filter(df, site == current_site, 
                                            year_month == current_year_month, 
                                            race_ethn == race, pop_cat == pop, 
                                            sub_pop == sub, measure == "ADP_admrel")
            total_df_ADP_admrel <- filter(df, site == current_site, 
                                          year_month == current_year_month, 
                                          race_ethn == "all_race_ethn", pop_cat == pop, 
                                          sub_pop == sub, measure == "ADP_admrel")
            
            if ((dim(current_df_ADP_admrel)[1] == 0) | dim(total_df_ADP_admrel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ADP_admrel$year)
              cat("ADP admrel", current_site, current_year_month, race, pop, sub, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              pop_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == race, measure == "gen_adult_pop") # filter for adult population
              pop_total_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == "all_race_ethn", measure == "gen_adult_pop") # filter for white adult population  `
              
              race_pop <- pop_df[, "value"]
              total_pop <- pop_total_df[, "value"]
              
              race_ADP_admrel <- current_df_ADP_admrel[, "value"] # get population
              total_ADP_admrel <- total_df_ADP_admrel[, "value"] # get total ADP_admrel
              
              prop_of_subpop_ADP_admrel <- round(race_ADP_admrel / total_ADP_admrel, 3)
              
              inc_rate_ADP_admrel <- round((race_ADP_admrel / race_pop) * 100000, 3) # normalize per 100,000
              
              ADP_admrel_disprop_ratio <- round(((race_ADP_admrel / total_ADP_admrel) / (race_pop / total_pop)), 3)
              
              current_cohort <- unique(current_df_ADP_admrel$cohort) # get cohort from filtered df
              current_month <- unique(current_df_ADP_admrel$month)
              current_sjc_year <- unique(current_df_ADP_admrel$sjc_year) # get sjc_year from filtered df
              current_sjc_quarter <- unique(current_df_ADP_admrel$sjc_quarter) # get current quarter
              
              
              
              new_df_prop_of_sub_pop_ADP_admrel <- data.frame(current_site, current_year, current_month, current_cohort,
                                                              current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                              "prop_of_subpop_ADP_admrel", prop_of_subpop_ADP_admrel, 
                                                              NA, current_year_month)
             
              
              new_df_inc_rate_ADP_admrel <- data.frame(current_site, current_year, current_month, current_cohort,
                                                       current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                       "Inc_rate_ADP_admrel" , inc_rate_ADP_admrel, 
                                                        NA, current_year_month)
              
              new_df_ADP_admrel_disprop_ratio <- data.frame(current_site, current_year, current_month, current_cohort,
                                                            current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                            "ADP_admrel_disprop_ratio", ADP_admrel_disprop_ratio, 
                                                             NA, current_year_month)
              
              
              colnames(new_df_prop_of_sub_pop_ADP_admrel) <- colnames(df) # get column names from current_df
              colnames(new_df_inc_rate_ADP_admrel) <- colnames(df)
              colnames(new_df_ADP_admrel_disprop_ratio) <- colnames(df)
              
              final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_admrel,
                                new_df_inc_rate_ADP_admrel,
                                new_df_ADP_admrel_disprop_ratio) # add new row before going to the next iteration  
            }
          }
        }
      }
    }
  
  } 
  return (final_df)
}  

jail_pm_full <- add_prop_inc_rate_ADP_admrel(jail_pm_full)


add_prop_and_inc_rate_ADP_snapshots <- function(df) {
  final_df <- df # initialize final_df 
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W")
  pop_cat_list <-  c("all_pop", "leg_stat_snap", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  ADP_df <- filter(df, measure == "ADP_snapshot")
  sites <- unique(ADP_df$site)
  ADP_year_month <- unique(ADP_df$year_month)
  
  
  for (current_site in sites) {
    for (current_year_month in ADP_year_month) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            sub_pop_list <- "all_pop_sub"
          }
          else if (pop == "leg_stat_snap") {
            sub_pop_list <- leg_stat_sub
          }
          else { # This means that pop_cat == severity
            sub_pop_list <- severity_sub
          }
          for (sub in sub_pop_list) {
            # We are just going to calculate for all_pop in this loop
            
            # ADP_snapshot
            current_df_ADP_snapshot <- filter(df, site == current_site, year_month == current_year_month,
                                              race_ethn == race, pop_cat == pop, sub_pop == sub,
                                              measure == "ADP_snapshot")
            total_df_ADP_snapshot <- filter(df, site == current_site, year_month == current_year_month, 
                                            race_ethn == "all_race_ethn", pop_cat == pop, sub_pop == sub,
                                            measure == "ADP_snapshot")
            
            if (dim(current_df_ADP_snapshot)[1] == 0 | dim(total_df_ADP_snapshot)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ADP_snapshot$year)
              cat("ADP Snapshots", current_site, current_year_month, race, pop, sub, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              pop_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == race,
                               pop_cat == "all_pop", measure == "gen_adult_pop") # filter for adult population
              pop_total_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == "all_race_ethn",
                                     pop_cat == "all_pop", measure == "gen_adult_pop") # filter for white adult population  `
              
              race_pop <- pop_df[, "value"]
              total_pop <- pop_total_df[, "value"]
              
              race_ADP_snapshot <- current_df_ADP_snapshot[, "value"]
              total_ADP_snapshot <- total_df_ADP_snapshot[, "value"] # get total ADP_snapshot
              
              prop_of_subpop_ADP_snapshots <- round(race_ADP_snapshot / total_ADP_snapshot, 3) 
              
              inc_rate_ADP_snapshots <- round((race_ADP_snapshot / race_pop) * 100000, 3) # normalize per 100,000
              
              ADP_snapshot_disprop_ratio <- round(((race_ADP_snapshot / total_ADP_snapshot) / (race_pop / total_pop)), 3)
              
              
              current_cohort <- unique(current_df_ADP_snapshot$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ADP_snapshot$sjc_year) # get sjc_year from filtered df
              current_sjc_quarter <- unique(current_df_ADP_snapshot$sjc_quarter) # get current quarter
              current_month <- unique(current_df_ADP_snapshot$month) # get current_month
              
              new_df_prop_of_sub_pop_ADP_snapshots <- data.frame(current_site, current_year, current_month, current_cohort,
                                                                 current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                                 "prop_of_subpop_ADP_snapshot", prop_of_subpop_ADP_snapshots, 
                                                                 NA, current_year_month)
              
              
              new_df_inc_rate_ADP_snapshots <- data.frame(current_site, current_year, current_month, current_cohort,
                                                          current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                          "Inc_rate_ADP_snapshot", inc_rate_ADP_snapshots, 
                                                          NA, current_year_month)
              
              
              new_df_ADP_snapshot_disprop_ratio <- data.frame(current_site, current_year, current_month, current_cohort,
                                                              current_sjc_year, current_sjc_quarter, race, pop, sub, 
                                                              "ADP_snapshot_disprop_ratio", ADP_snapshot_disprop_ratio, 
                                                              NA, current_year_month)
              
    
              colnames(new_df_prop_of_sub_pop_ADP_snapshots) <- colnames(df)
              colnames(new_df_inc_rate_ADP_snapshots) <- colnames(df)
              colnames(new_df_ADP_snapshot_disprop_ratio) <- colnames(df)
              
              final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_snapshots,
                                new_df_inc_rate_ADP_snapshots,
                                new_df_ADP_snapshot_disprop_ratio) # add new row before going to the next iteration            
            }
          }
        }
      }
    }
  }
  return(final_df)
}  

jail_pm_full <- add_prop_and_inc_rate_ADP_snapshots(jail_pm_full)


add_ALOS_rel <- function(df) {
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "API", "B", "L", "POC")
  pop_cat_list <-  c("all_pop", "leg_stat_booking", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  ALOS_df <- filter(df, measure == "ALOS_rel")
  sites <- unique(ALOS_df$sites)
  ADP_year_month <- unique(ALOS_df$year_month)
  
  for (current_site in sites) {
    for (current_year_month in ADP_year_month) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            sub_pop_list <- "all_pop_sub"
          }
          else if (pop == "leg_stat_booking") {
            sub_pop_list <- leg_stat_sub
          }
          else { # This means that pop_cat == severity
            sub_pop_list <- severity_sub
          }
          for (sub in sub_pop_list) {
            # We are just going to calculate for all_pop in this loop
            # get current year  
            
            # ALOS_rel
            current_df_ALOS_rel <- filter(df, site == current_site, year_month == current_year_month,
                                          race_ethn == race, pop_cat == pop, sub_pop == sub, measure == "ALOS_rel")
            white_df_ALOS_rel <- filter(df, site == current_site, year_month == current_year_month,
                                        race_ethn == "W", pop_cat == pop, sub_pop == sub, measure == "ALOS_rel")
            
            if (dim(current_df_ALOS_rel)[1] == 0 | dim(white_df_ALOS_rel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ALOS_rel$year)
              cat("ALOS rel", current_site, current_year_month, race, current_year, pop, sub, "\n", " ")
              
              race_ALOS_rel <- current_df_ALOS_rel[, "value"] # get population
              white_ALOS_rel <- white_df_ALOS_rel[, "value"] # get total ALOS_rel
              
              ALOS_rel_disparity_ratio <- round(race_ALOS_rel / white_ALOS_rel, 3)
              
              current_cohort <- unique(current_df_ALOS_rel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ALOS_rel$sjc_year) # get sjc_year from filtered df
              current_sjc_quarter <- unique(current_df_ALOS_rel$sjc_quarter) # get current quarter
              current_month <- unique(current_df_ALOS_rel$month) # get current month
        
              
              new_df_ALOS_rel_disparity_ratio <- data.frame(current_site, current_year, current_month, current_cohort,
                                                            current_sjc_year, current_sjc_quarter, race, pop, sub,  
                                                            "ALOS_rel_disparity_ratio", ALOS_rel_disparity_ratio, NA,
                                                            current_year_month) # Make new dataframe with values in proper place

            
              colnames(new_df_ALOS_rel_disparity_ratio) <- colnames(df) # get column names from current_df
              
              final_df <- rbind(final_df, new_df_ALOS_rel_disparity_ratio) # add new row before going to the next iteration            
            }
          }
        }
      }
    }
  }
  return(final_df) # return dataframe with newly added rows
}

jail_pm_full <- add_ALOS_rel(jail_pm_full)


add_ALOS_conf <- function(df) {
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "API", "B", "L", "POC")
  pop_cat_list <-  c("all_pop", "leg_stat_snap", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  ALOS_df <- filter(df, measure == "ALOS_conf")
  sites <- unique(ALOS_df$site)
  ADP_year_month <- unique(ALOS_df$year_month)
  
  for (current_site in sites) {
    for (current_year_month in ADP_year_month) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            sub_pop_list <- "all_pop_sub"
          }
          else if (pop == "leg_stat_snap") {
            sub_pop_list <- leg_stat_sub
          }
          else { # This means that pop_cat == severity
            sub_pop_list <- severity_sub
          }
          for (sub in sub_pop_list) {
            # We are just going to calculate for all_pop in this loop
            # get current year  
            
            # ALOS_rel
            current_df_ALOS_conf <- filter(df, site == current_site, year_month == current_year_month,
                                          race_ethn == race, pop_cat == pop, sub_pop == sub)
            white_df_ALOS_conf <- filter(ALOS_df, site == current_site, year_month == current_year_month,
                                        race_ethn == "W", pop_cat == pop, sub_pop == sub)
            
            if (dim(current_df_ALOS_conf)[1] == 0 | dim(white_df_ALOS_conf)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              
              cat("ALOS conf", current_site, current_year_month, race, pop, sub, "\n", " ")
              
              race_ALOS_conf <- current_df_ALOS_conf[, "value"] # get population
              white_ALOS_conf <- white_df_ALOS_conf[, "value"] # get total ALOS_rel
              
              ALOS_conf_disparity_ratio <- round(race_ALOS_conf / white_ALOS_conf, 3)
              
              current_year <- unique(current_df_ALOS_conf$year)
              current_cohort <- unique(current_df_ALOS_conf$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ALOS_conf$sjc_year) # get sjc_year from filtered df
              current_year <- unique(current_df_ALOS_conf$year) # get current year
              current_sjc_quarter <- unique(current_df_ALOS_conf$sjc_quarter) # get current quarter
              current_month <- unique(current_df_ALOS_conf$month) # get current month
              
              
              new_df_ALOS_conf_disparity_ratio <- data.frame(current_site, current_year, current_month, current_cohort,
                                                            current_sjc_year, current_sjc_quarter, race, pop, sub,
                                                            "ALOS_conf_disparity_ratio", ALOS_conf_disparity_ratio, NA,
                                                            current_year_month)
              
              colnames(new_df_ALOS_conf_disparity_ratio) <- colnames(df) # get column names from current_df
              
              final_df <- rbind(final_df, new_df_ALOS_conf_disparity_ratio) # add new row before going to the next iteration            
            }
          }
        }
      }
    }
  }
  return(final_df)
}

jail_pm_full <- add_ALOS_conf(jail_pm_full)


# split values that need to be averaged versus summed
avg_measures <- c("ADP_admrel", "ALOS_rel", "ADP_snapshot", "ALOS_conf")
sum_measures <- "bookings"

jail_pm_avg_full <- filter(jail_pm_full, measure %in% avg_measures) # filter average measures
jail_pm_sum_full <- filter(jail_pm_full, measure == sum_measures) # filter sum measures


avg_df_agg <- aggregate(jail_pm_avg_full[, "value"], by = list(jail_pm_avg_full$site, 
                                                             jail_pm_avg_full$sjc_quarter,
                                                             jail_pm_avg_full$race_ethn,
                                                             jail_pm_avg_full$pop_cat,
                                                             jail_pm_avg_full$sub_pop,
                                                             jail_pm_avg_full$measure),
                                                             FUN = mean)

sum_df_agg <- aggregate(jail_pm_sum_full[, c("value", "calc_n")], by = list(jail_pm_sum_full$site, 
                                                             jail_pm_sum_full$sjc_quarter,
                                                             jail_pm_sum_full$race_ethn,
                                                             jail_pm_sum_full$pop_cat,
                                                             jail_pm_sum_full$sub_pop,
                                                             jail_pm_sum_full$measure),
                                                             FUN = sum)

colnames(avg_df_agg) <- c("site", "quarter", "race_ethn", "pop_cat", "sub_pop", 
                          "measure", "value")

colnames(sum_df_agg) <- c("site", "quarter", "race_ethn", "pop_cat", "sub_pop", 
                          "measure", "value", "calc_n")

############# Add calc_n back to avg data frame (calc_n variable needs to be summed instead of averaged) ############
avg_df_agg_calc_n <- aggregate(jail_pm_avg_full[, "calc_n"], by = list(jail_pm_avg_full$site, 
                                                               jail_pm_avg_full$sjc_quarter,
                                                               jail_pm_avg_full$race_ethn,
                                                               jail_pm_avg_full$pop_cat,
                                                               jail_pm_avg_full$sub_pop,
                                                               jail_pm_avg_full$measure),
                                                               FUN = sum)

colnames(avg_df_agg_calc_n) <- c("site", "quarter", "race_ethn", "pop_cat", "sub_pop", 
                          "measure", "calc_n")

# make a new variables to join avg and avg_calc datasets

avg_df_agg$join_var <- paste(avg_df_agg$site, avg_df_agg$quarter, avg_df_agg$race_ethn, 
                             avg_df_agg$pop_cat, avg_df_agg$sub_pop, avg_df_agg$measure, sep = "_")
avg_df_agg_calc_n$join_var <- paste(avg_df_agg_calc_n$site, avg_df_agg_calc_n$quarter, avg_df_agg_calc_n$race_ethn, 
                                    avg_df_agg_calc_n$pop_cat, avg_df_agg_calc_n$sub_pop, avg_df_agg_calc_n$measure, sep = "_")

# use left join to join calc_n column to avg dataset
avg_df_agg <- left_join(avg_df_agg, avg_df_agg_calc_n[, c("join_var", "calc_n")], 
                        by = "join_var")

avg_df_agg <- select(avg_df_agg, -join_var) # delete extra column in avg dataset


quarter_df <- rbind(avg_df_agg, sum_df_agg) # rbind avg and sum datasets together

quarter_df <- add_column(quarter_df, year = NA, .before = "quarter") # make sjc_year variable
quarter_df <- add_column(quarter_df, quarter_range = NA, .after = "quarter") # make quarter variable
quarter_df <- add_column(quarter_df, sjc_cohort = NA, .after = "quarter_range") # make sjc_year variable
quarter_df <- add_column(quarter_df, sjc_year = NA, .after = "sjc_cohort") # make sjc_year variable
quarter_df <- add_column(quarter_df, race_ethn_drop = NA, .after = "race_ethn") # make sjc_year variable

cohort_dict <- c("ALL"="3", "BUN"="3", "COO"="2",
                 "HAR"="1", "MIL"="1", "MUL"="2",
                 "NOR"="1", "PBC"="2", "PEN"="2",
                 "PHI"="1", "PIM"="1")

for (i in seq(length(cohort_dict))) {
  insert_rows <- quarter_df$site == names(cohort_dict[i])
  quarter_df[insert_rows, "sjc_cohort"] <- cohort_dict[i]
}

# Split cohorts...values may differ based on cohort
quarter_1and2 <- filter(quarter_df, sjc_cohort %in% c("1","2"))
quarter_3 <-filter(quarter_df, sjc_cohort == "3")

# population quarter variable
month_to_quarter_dict_cohort_1_and_2 <- c("Baseline"=0, "May 16 - Jul 16"=1, "Aug 16 - Oct 16"=2,
                                          "Nov 16 - Jan 17"=3, "Feb 17 - Apr 17"=4, "May 17 - Jul 17"=5,
                                          "Aug 17 - Oct 17"=6, "Nov 17 - Jan 18"=7, "Feb 18 - Apr 18"=8,
                                          "May 18 - Jul 18"=9, "Aug 18 - Oct 18"=10, "Nov 18 - Jan 19"=11,
                                          "Feb 19 - Apr 19"=12, "May 19 - Jul 19"=13, "Aug 19 - Oct 19"=14,
                                          "Nov 19 - Jan 20"=15, "Feb 20 - Apr 20"=16, "May 20 - Jul 20"=17,
                                          "Aug 20 - Oct 20"=18, "Nov 20 - Jan 21"=19, "Feb 21 - Apr 21"=20)


month_to_quarter_dict_cohort_3 <- c("Baseline"=0, "May 18 - Jul 18"=1, "Aug 18 - Oct 18"=2,
                                    "Nov 18 - Jan 19"=3, "Feb 19 - Apr 19"=4, "May 19 - Jul 19"=5,
                                    "Aug 19 - Oct 19"=6, "Nov 19 - Jan 20"=7, "Feb 20 - Apr 20"=8,
                                    "May 20 - Jul 20"=9, "Aug 20 - Oct 20"=10, "Nov 20 - Jan 21"=11,
                                    "Feb 21 - Apr 21"=12)

# Use a for loop to populate quarter variable
for (i in seq(1, length(month_to_quarter_dict_cohort_1_and_2))) {
  insert_rows <- quarter_1and2$quarter == month_to_quarter_dict_cohort_1_and_2[i]
  quarter_1and2[insert_rows, "quarter_range"] <- names(month_to_quarter_dict_cohort_1_and_2[i])
}

# Use a for loop to join the individual populations
for (i in seq(1, length(month_to_quarter_dict_cohort_3))) {
  insert_rows <- quarter_3$quarter == month_to_quarter_dict_cohort_3[i]
  quarter_3[insert_rows, "quarter_range"] <- names(month_to_quarter_dict_cohort_3[i])
}

# populate sjc_year variable
sjc_quarter_sjc_year_dict_1and2 <- c("baseline"=0, "year 1"=1, "year 1"=2, "year 1"=3, "year 1"=4,   
                                     "year 2"=5, "year 2"=6, "year 2"=7, "year 2"=8, "year 3"=9,   
                                     "year 3"=10, "year 3"=11, "year 3"=12, "year 4"=13, "year 4"=14,  
                                     "year 4"=15, "year 4"=16, "year 5"=17, "year 5"=18, "year 5"=19,  
                                     "year 5"=20, "year 6"=21, "year 6"=22, "year 6"=23, "year 6"=24)

sjc_quarter_sjc_year_dict_3 <- c("baseline"=0, "year 1"=1, "year 1"=2, "year 1"=3, "year 1"=4,   
                                 "year 2"=5, "year 2"=6, "year 2"=7, "year 2"=8, "year 3"=9,   
                                 "year 3"=10, "year 3"=11, "year 3"=12, "year 4"=13, "year 4"=14,  
                                 "year 4"=15)

# Use a for loop to populate sjc_year variable
for (i in seq(1, length(sjc_quarter_sjc_year_dict_1and2))) {
  insert_rows <- quarter_1and2$quarter == sjc_quarter_sjc_year_dict_1and2[i]
  quarter_1and2[insert_rows, "sjc_year"] <- names(sjc_quarter_sjc_year_dict_1and2[i])
}

# Use a for loop to join the individual populations
for (i in seq(1, length(sjc_quarter_sjc_year_dict_3))) {
  insert_rows <- quarter_3$quarter == sjc_quarter_sjc_year_dict_3[i]
  quarter_3[insert_rows, "sjc_year"] <- names(sjc_quarter_sjc_year_dict_3[i])
}


quarter_df <- rbind(quarter_1and2, quarter_3)

# Population year variable
quarter_df$year <- paste("20", str_sub(quarter_df$quarter_range, start = -2, end = -1), sep = "")
# populate year where quarter is baseline
quarter_df[quarter_df$sjc_cohort %in% c("1","2") & quarter_df$sjc_year == "baseline", "year"] <- "2016" # Cohorts 1and2

quarter_df[quarter_df$sjc_cohort == "3" & quarter_df$sjc_year == "baseline" , "year"] <- "2018" # Cohort 3

# Turn year back into an integer
quarter_df$year <- as.integer(quarter_df$year)




# Take out booking_rate_RRI for all race ethn and white popluations
quarter_df <- quarter_df[!(quarter_df$race_ethn == "all_race_ethn" & quarter_df$measure == "booking_rate_RRI"), ]
quarter_df <- quarter_df[!(quarter_df$race_ethn == "W" & quarter_df$measure == "booking_rate_RRI"), ]



write.csv(quarter_df, file = "C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/jail_pm_quarter_df_test_df.csv",
          row.names = FALSE)
