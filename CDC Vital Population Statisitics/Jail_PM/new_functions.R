# Brian Holliday
# Jail PM Measurements Script
# 2/9/22

# Goals of this Script

# 1. Join Jail PM Data with most recent CDC Population Data from SJC Sites
# 2. Produce Quarterly Measurements for Jail PM Statistics for every race, ethnicity, and sub population and for each site

######### Change log #############
# 1. Have to separate ADP_snapshots and ADP_admrel for aggregation



# import libraries
library("plyr")
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

jail_pm_full <- read.csv("C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/monthly_jail_measures_all_sites_BLtoY5_full.csv") # import Jail PM Dataset

jail_pm_full <- jail_pm_full[!is.na(jail_pm_full$sjc_quarter), ] # Drop rows where sjc_quarter is null

# split values that need to be averaged versus summed
avg_measures <- c("ADP_admrel", "ALOS_rel", "ADP_snapshot", "ALOS_conf")
sum_measures <- "bookings"

jail_pm_avg_full <- filter(jail_pm_full, measure %in% avg_measures) # filter average measures
jail_pm_sum_full <- filter(jail_pm_full, measure == sum_measures) # filter sum measures

######### Sum Values by SJC_Quarter ################# 
values_by_quarter <- function(df, agg_func) {# This is the aggregation function
  
  # initialize empty dataframe with columns
  final_df <- data.frame(site = character(),
                         cohort = integer(),
                         sjc_quarter = integer(),
                         sub_pop = character(), 
                         measure = character(), 
                         value = double(),
                         calc_n = double())
  
  for (current_site in unique(df$site)) {
    for (race in unique(df$race_ethn)) {
      for (cat in unique(df$pop_cat)) {
        # Average Measures  
        if (agg_func == "average") {
          avg_df <- filter(df, site == current_site, race_ethn == race, pop_cat == cat)
          #avg_df <- summarise_at(group_by(avg_df, sjc_quarter, sub_pop, measure), vars(value, calc_n), funs(mean(., na.rm = TRUE)))
          # Use Aggregate()
          if (dim(avg_df)[1] == 0) { # Skip to next iteration if filtered df has no rows
            next
          }
          
          else { # print the calculation being added to the larger dataset
            cat("Adding", current_site, race, cat, " ")
          }
          
          if (cat == "all_pop") {
            avg_df_agg <- aggregate(avg_df[, 11:12], by = list(avg_df$sjc_quarter, avg_df$measure),
                                    FUN = mean)
            colnames(avg_df_agg) <- c("sjc_quarter", "measure", "value", "calc_n")
            avg_df_agg <- add_column(avg_df_agg, sub_pop = NA, .after = "sjc_quarter")
            
          }
          
          else {
            avg_df_agg <- aggregate(avg_df[, 11:12], by = list(avg_df$sjc_quarter, avg_df$sub_pop, avg_df$measure),
                                    FUN = mean)
            colnames(avg_df_agg) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")
          }
          
          
          # Add columns
          avg_df_agg <- add_column(avg_df_agg, site = current_site, .before = "sjc_quarter")
          current_cohort <- unique(avg_df$cohort)
          avg_df_agg <- add_column(avg_df_agg, cohort = current_cohort, .after = "site")
          avg_df_agg <- add_column(avg_df_agg, race_ethn = race, .after = "sjc_quarter")
          avg_df_agg <- add_column(avg_df_agg, pop_cat = cat, .after = "race_ethn")
          # Bind and Join
          final_df <- rbind(final_df, avg_df_agg)
          print("\n")
        }
        # else mean that sum is the aggregate function 
        else {
          # Sum Measures
          sum_df <- filter(df, site == current_site, race_ethn == race, pop_cat == cat)
          #sum_df <- summarise_at(group_by(sum_df, sjc_quarter, sub_pop, measure), vars(value, calc_n), funs(sum(., na.rm = TRUE)))
          if (dim(sum_df)[1] == 0) { # Skip to next iteration if filter df has no rows
            next
          }
          
          else { # print the calculation being added to the larger dataset
            cat("Adding", current_site, race, cat, " ") 
          }
          
          if (cat == "all_pop") {
            sum_df_agg <- aggregate(sum_df[, 11:12], by = list(sum_df$sjc_quarter, sum_df$measure),
                                    FUN = sum)
            colnames(sum_df_agg) <- c("sjc_quarter", "measure", "value", "calc_n")
            sum_df_agg <- add_column(sum_df_agg, sub_pop = NA, .after = "sjc_quarter") # Add columns
            
          }
          
          else {
            sum_df_agg <- aggregate(sum_df[, 11:12], by = list(sum_df$sjc_quarter, sum_df$sub_pop, sum_df$measure),
                                    FUN = sum)
            colnames(sum_df_agg) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")# Add columns
          }
          
          
          # Add columns
          sum_df_agg <- add_column(sum_df_agg, site = current_site, .before = "sjc_quarter")
          current_cohort <- unique(sum_df$cohort)
          sum_df_agg <- add_column(sum_df_agg, cohort = current_cohort, .after = "site")
          sum_df_agg <- add_column(sum_df_agg, race_ethn = race, .after = "sjc_quarter")
          sum_df_agg <- add_column(sum_df_agg, pop_cat = cat, .after = "race_ethn")
          # Bind and join
          final_df <- rbind(final_df, sum_df_agg)
          print("\n")
        }
        
      }
    }
  }
  
  return(final_df)
}


quarter_df_sum <- values_by_quarter(jail_pm_sum_full, "sum")
quarter_df_avg  <- values_by_quarter(jail_pm_avg_full, "average")

quarter_df <- rbind(quarter_df_avg, quarter_df_sum) # row join data frames at need to be averaged and summed

####### Add back columns that were dropped during aggeragate #######
# quarter is an extra column to add the time period regardless of the sjc_quarter since it is different between cohorts 1and2 and cohort 3

quarter_df <- add_column(quarter_df, quarter = NA, .after = "sjc_quarter") # make quarter variable
quarter_df <- add_column(quarter_df, sjc_year = NA, .before = "sjc_quarter") # make sjc_year variable
quarter_df <- add_column(quarter_df, year = NA, .before = "cohort") # make sjc_year variable

# Split cohorts...values may differ based on cohort
quarter_1and2 <- filter(quarter_df, cohort %in% c(1,2))
quarter_3 <-filter(quarter_df, cohort == 3)

# population quarter variable
month_to_quarter_dict_cohort_1_and_2 <- c("Baseline"=0, "May 16 - Jul 16"=1, "Aug 16 - Oct 16"=2,
                                          "Nov 16 - Jan 17"=3, "Feb 17 - Apr 17"=4, "May 17 - Jul 17"=5,
                                          "Aug 17 - Oct 17"=6, "Nov 17 - Jan 18"=7, "Feb 18 - Apr 18"=8,
                                          "May 18 - Jul 18"=9, "Aug 18 - Oct 18"=10, "Nov 18 - Jan 19"=11,
                                          "Feb 19 - Apr 19"=12, "May 19 - Jul 19"=13, "Aug 19 - Oct 19"=14,
                                          "Nov 19 - Jan 20"=15, "Feb 20 - Apr 20"=16, "May 20 - Jul 20"=17,
                                          "Aug 20 - Oct 20"=18, "Nov 20 - Jan 21"=19, "Feb 21 - Apr 21"=20,
                                          "May 21 - Jul 21"=21, "Aug 21 - Oct 21"=22, "Nov 21 - Jan 22"=23,
                                          "Feb 22 - Apr 22"=24)

month_to_quarter_dict_cohort_3 <- c("Baseline"=0, "May 18 - Jul 18"=1, "Aug 18 - Oct 18"=2,
                                    "Nov 18 - Jan 19"=3, "Feb 19 - Apr 19"=4, "May 19 - Jul 19"=5,
                                    "Aug 19 - Oct 19"=6, "Nov 19 - Jan 20"=7, "Feb 20 - Apr 20"=8,
                                    "May 20 - Jul 20"=9, "Aug 20 - Oct 20"=10, "Nov 20 - Jan 21"=11,
                                    "Feb 21 - Apr 21"=12, "May 21 - Jul 21"=13, "Aug 21 - Oct 21"=14,
                                    "Nov 21 - Jan 22"=15)

# Use a for loop to populate quarter variable
for (i in seq(1, length(month_to_quarter_dict_cohort_1_and_2))) {
  insert_rows <- quarter_1and2$sjc_quarter == month_to_quarter_dict_cohort_1_and_2[i]
  quarter_1and2[insert_rows, "quarter"] <- names(month_to_quarter_dict_cohort_1_and_2[i])
}

# Use a for loop to join the individual populations
for (i in seq(1, length(month_to_quarter_dict_cohort_3))) {
  insert_rows <- quarter_3$sjc_quarter == month_to_quarter_dict_cohort_3[i]
  quarter_3[insert_rows, "quarter"] <- names(month_to_quarter_dict_cohort_3[i])
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
  insert_rows <- quarter_1and2$sjc_quarter == sjc_quarter_sjc_year_dict_1and2[i]
  quarter_1and2[insert_rows, "sjc_year"] <- names(sjc_quarter_sjc_year_dict_1and2[i])
}

# Use a for loop to join the individual populations
for (i in seq(1, length(sjc_quarter_sjc_year_dict_3))) {
  insert_rows <- quarter_3$sjc_quarter == sjc_quarter_sjc_year_dict_3[i]
  quarter_3[insert_rows, "sjc_year"] <- names(sjc_quarter_sjc_year_dict_3[i])
}


quarter_df <- rbind(quarter_1and2, quarter_3) # bind quarter1and2 and quarter3 back together

# Population year variable
quarter_df$year <- paste("20", str_sub(quarter_df$quarter, start = -2, end = -1), sep = "")
# populate year where quarter is baseline
quarter_df[quarter_df$cohort %in% c(1,2) & quarter_df$sjc_year == "baseline", "year"] <- "2016" # Cohorts 1and2

quarter_df[quarter_df$cohort == 3 & quarter_df$sjc_year == "baseline" , "year"] <- "2018" # Cohort 3

# Turn year back into an integer
quarter_df$year <- as.integer(quarter_df$year)


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
cdc_pop_year <- cdc_pop_long$year
cdc_pop_long <- select(cdc_pop_long, -fips_state_county_code, -year)
cdc_pop_long <- add_column(cdc_pop_long, year = cdc_pop_year, .after = "site_name")
cdc_pop_long$calc_n <- NA

colnames(cdc_pop_long) <- c("site", "year", "measure", "value", "calc_n")

# Filter out for jail_pm sites
cdc_pop_long <- filter(cdc_pop_long, site %in% unique(quarter_df$site))

cdc_pop_long <- add_column(cdc_pop_long, sjc_quarter = NA, .after = "year")
cdc_pop_long <- add_column(cdc_pop_long, cohort = NA, .after = "year")
cdc_pop_long <- add_column(cdc_pop_long, race_ethn = NA, .after = "sjc_quarter")
cdc_pop_long <- add_column(cdc_pop_long, sjc_year = NA, .before = "sjc_quarter")
cdc_pop_long <- add_column(cdc_pop_long, pop_cat = NA, .after = "race_ethn")
cdc_pop_long <- add_column(cdc_pop_long, sub_pop = NA, .after = "pop_cat")
cdc_pop_long <- add_column(cdc_pop_long, quarter = NA, .after = "sjc_quarter")

cohort_dict <- c("ALL"=3, "BUN"=3, "COO"=2, 
                 "HAR"=1, "MIL"=1, "MUL"=2,
                 "NOR"=1, "PBC"=2, "PEN"=2,
                 "PHI"=1, "PIM"=1)

# Use a for loop to join the individual populations
for (i in seq(1, length(cohort_dict))) {
  insert_rows <- cdc_pop_long$site == names(cohort_dict[i])
  cdc_pop_long[insert_rows, "cohort"] <- cohort_dict[i]
}

# Find rows that start with pop adult
measure <- unique(cdc_pop_long$measure)
pop_adults <- grep("pop_adult", measure, value = TRUE)

# limit to data that just have total population and total for each race
# I need to add more comming up soon
pop_adults <- c("pop_adult_any_ethnicity", "pop_adult_poc",                      
                "pop_adult_hispanic_any_race", "pop_adult_black_non_hispanic",       
                "pop_adult_white_non_hispanic", "pop_adult_AIAN_non_hispanic",
                "pop_adult_API_non_hispanic")

sjc_years <- c(2015, 2016, 2017, 2018, 2019, 2020) # filter sjc years
jail_pm_sites <- unique(quarter_df$site) # limit by sites that are in the jail_pm file 

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
          # Calculate the propcolortion of general adult population for the specific race
          prop_gen_adult_pop <- round(race_pop / total_pop, 3)
          new_df <- data.frame(current_site, current_year, current_cohort, NA,
                               NA, NA, race, NA, NA, "prop_gen_adult_pop", 
                               prop_gen_adult_pop, NA)
          
          colnames(new_df) <- colnames(current_df) # get column names from filtered df
          
          final_df <- rbind(final_df, new_df) # add new row to dataframe
        }
      }
    }
  }
  return(final_df)
}


cdc_pop_long_prop_gen <- populate_prop_gen(cdc_pop_long) # call populate_prop_gen function to get population rates
cdc_pop_long_prop_gen$pop_cat <- "all_pop" # for now we are only dealing with total adult populations

quarter_df <- rbind(quarter_df, cdc_pop_long_prop_gen) # rbind quarter_df and cdc_pop data.frames 

add_bookings_rates <- function(df) { # booking_rates function
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W")
  pop_cat_list <-  c("all_pop", "leg_stat_booking", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  for (current_site in unique(df$site)) {
    for (current_quarter in unique(df$sjc_quarter)) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            # We are just going to calculate for all_pop in this loop
            current_df <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                 race_ethn == race, pop_cat == "all_pop", measure == "bookings")
            current_df_white <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                       race_ethn == "W", pop_cat == "all_pop", measure == "bookings")
            
            if ((dim(current_df)[1] == 0) | (dim(current_df_white)[1] == 0)) {
              next # if filtered data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df$year) # get current year
              cat("Adding", current_site, current_year, current_quarter, race, "\n", " ")
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
              current_quarter_long <- unique(current_df$quarter) # get current quarter
              
              new_df <- data.frame(current_site, current_year, current_cohort,
                                   current_sjc_year, current_quarter, current_quarter_long,
                                   race, "all_pop", NA, "booking_rate", booking_rate, 
                                   NA)
              new_df_RRI <- data.frame(current_site, current_year, current_cohort,
                                       current_sjc_year, current_quarter, current_quarter_long,
                                       race, "all_pop", NA, "booking_rate_RRI", booking_rate_RRI, 
                                       NA)
              
              colnames(new_df) <- colnames(current_df) # get column names from current_df
              colnames(new_df_RRI) <- colnames(current_df)
              
              final_df <- rbind(final_df, new_df, new_df_RRI) # add new row before going to the next iteration
              }
            }
          
          else if (pop == "leg_stat_booking") {
            for (leg_stat in leg_stat_sub) {
              # We are just going to calculate for all_pop in this loop
              current_df <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                   race_ethn == race, pop_cat == pop, 
                                   sub_pop == leg_stat, measure == "bookings")
              current_df_white <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                         race_ethn == "W", pop_cat == pop, 
                                         sub_pop == leg_stat, measure == "bookings")
              
              if ((dim(current_df)[1] == 0) | (dim(current_df_white)[1] == 0)) {
                next # if data.frame is empty move to the next iteration
              }
              
              else { # if the data frame has rows proceed to the calculation
                # get population for site and year
                current_year <- unique(current_df$year) # get current year
                cat("Adding", current_site, current_year, current_quarter, race, leg_stat, "\n", " ")
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
                current_quarter_long <- unique(current_df$quarter) # get current quarter
                
                new_df <- data.frame(current_site, current_year, current_cohort,
                                     current_sjc_year, current_quarter, current_quarter_long,
                                     race, pop, leg_stat, "booking_rate", booking_rate, 
                                     NA)
                new_df_RRI <- data.frame(current_site, current_year, current_cohort,
                                         current_sjc_year, current_quarter, current_quarter_long,
                                         race, pop, leg_stat, "booking_rate_RRI", booking_rate_RRI, 
                                         NA)
                
                colnames(new_df) <- colnames(current_df) # get column names from current_df
                colnames(new_df_RRI) <- colnames(current_df)
                
                final_df <- rbind(final_df, new_df, new_df_RRI) # add new row before going to the next iteration
                }
              }
            }
            
          else { # This clause means that pop == severity
            # We are just going to calculate for all_pop in this loop
            for (sev in severity_sub) {
              current_df <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                   race_ethn == race, pop_cat == pop, 
                                   sub_pop == sev, measure == "bookings")
              current_df_white <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                         race_ethn == "W", pop_cat == pop, 
                                         sub_pop == sev, measure == "bookings")
              if ((dim(current_df)[1] == 0) | (dim(current_df_white)[1] == 0)) {
                next # if data.frame is empty move to the next iteration
              }
              
              else { # if the data frame has rows proceed to the calculation
                # get population for site and year
                current_year <- unique(current_df$year) # get current year
                cat("Adding", current_site, current_year, current_quarter, race, sev, "\n", " ")
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
                current_quarter_long <- unique(current_df$quarter) # get current quarter
                
                new_df <- data.frame(current_site, current_year, current_cohort,
                                     current_sjc_year, current_quarter, current_quarter_long,
                                     race, pop, sev, "booking_rate", booking_rate, 
                                     NA)
                new_df_RRI <- data.frame(current_site, current_year, current_cohort,
                                         current_sjc_year, current_quarter, current_quarter_long,
                                         race, pop, sev, "booking_rate_RRI", booking_rate_RRI, 
                                         NA)
                
                colnames(new_df) <- colnames(current_df) # get column names from current_df
                colnames(new_df_RRI) <- colnames(current_df)
                
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

quarter_df <- add_bookings_rates(quarter_df) # add booking rates to quarter_df

add_prop_inc_rate_ADP_admrel <- function(df) {
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W") 
  pop_cat_list <-  c("all_pop", "leg_stat_booking", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  for (current_site in unique(df$site)) {
    for (current_quarter in unique(df$sjc_quarter)) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            # We are just going to calculate for all_pop in this loop
            # ADP admrel 
            current_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                            race_ethn == race, pop_cat == "all_pop", measure == "ADP_admrel")
            total_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                          race_ethn == "all_race_ethn", pop_cat == "all_pop", measure == "ADP_admrel")
            
            if ((dim(current_df_ADP_admrel)[1] == 0)) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ADP_admrel$year)
              cat("Adding", current_site, current_year, current_quarter, race, "\n", " ")
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
              
              race_ADP_admrel <- current_df_ADP_admrel[, "value"] # get population
              total_ADP_admrel <- total_df_ADP_admrel[, "value"] # get total ADP_admrel
              
              prop_of_subpop_ADP_admrel <- round(race_ADP_admrel / total_ADP_admrel, 3)
              
              inc_rate_ADP_admrel <- round((race_ADP_admrel / race_pop) * 100000, 3) # normalize per 100,000
              
              ADP_admrel_disprop_ratio <- round(((race_ADP_admrel / total_ADP_admrel) / (race_pop / total_pop)), 3)
              
              current_cohort <- unique(current_df_ADP_admrel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ADP_admrel$sjc_year) # get sjc_year from filtered df
              current_quarter_long <- unique(current_df_ADP_admrel$quarter) # get current quarter
              
              new_df_prop_of_sub_pop_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                              current_sjc_year, current_quarter, current_quarter_long,
                                                              race, "all_pop", NA, "prop_of_subpop_ADP_admrel", prop_of_subpop_ADP_admrel, 
                                                              NA)
              
              new_df_inc_rate_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                       current_sjc_year, current_quarter, current_quarter_long,
                                                       race, "all_pop", NA, "Inc_rate_ADP_admrel", inc_rate_ADP_admrel, 
                                                       NA)
              
              new_df_ADP_admrel_disprop_ratio <- data.frame(current_site, current_year, current_cohort,
                                                            current_sjc_year, current_quarter, current_quarter_long,
                                                            race, "all_pop", NA, "ADP_admrel_disprop_ratio", ADP_admrel_disprop_ratio, 
                                                            NA)
              
              
              colnames(new_df_prop_of_sub_pop_ADP_admrel) <- colnames(current_df_ADP_admrel) # get column names from current_df
              colnames(new_df_inc_rate_ADP_admrel) <- colnames(current_df_ADP_admrel)
              colnames(new_df_ADP_admrel_disprop_ratio) <- colnames(current_df_ADP_admrel)
              
              final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_admrel,
                                new_df_inc_rate_ADP_admrel,
                                new_df_ADP_admrel_disprop_ratio) # add new row before going to the next iteration            
            }
          }
          else if (pop == "leg_stat_booking") {
            for (leg_stat in leg_stat_sub) { # loop through sub pop variables when sub_pop == leg_stat_bookings
            # We are just going to calculate for all_pop in this loop
            # ADP admrel 
            current_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                            race_ethn == race, pop_cat == pop, 
                                            sub_pop == leg_stat, measure == "ADP_admrel")
            total_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                          race_ethn == "all_race_ethn", pop_cat == pop, 
                                          sub_pop == leg_stat, measure == "ADP_admrel")
            
            if (dim(current_df_ADP_admrel)[1] == 0 | dim(current_df_ADP_admrel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ADP_admrel$year)
              cat("Adding", current_site, current_year, current_quarter, race, leg_stat, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              pop_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == race,
                               measure == "gen_adult_pop") # filter for adult population
              pop_total_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == "all_race_ethn",
                                     measure == "gen_adult_pop") # filter for white adult population  `
              
              race_pop <- pop_df[, "value"]
              total_pop <- pop_total_df[, "value"]
              
              race_ADP_admrel <- current_df_ADP_admrel[, "value"] # get population
              total_ADP_admrel <- total_df_ADP_admrel[, "value"] # get total ADP_admrel
              
              prop_of_subpop_ADP_admrel <- round(race_ADP_admrel / total_ADP_admrel, 3)
              
              inc_rate_ADP_admrel <- round((race_ADP_admrel / race_pop) * 100000, 3) # normalize per 100,000
              
              ADP_admrel_disprop_ratio <- round(((race_ADP_admrel / total_ADP_admrel) / (race_pop / total_pop)), 3)
              
              current_cohort <- unique(current_df_ADP_admrel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ADP_admrel$sjc_year) # get sjc_year from filtered df
              current_quarter_long <- unique(current_df_ADP_admrel$quarter) # get current quarter
              
              new_df_prop_of_sub_pop_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                              current_sjc_year, current_quarter, current_quarter_long,
                                                              race, pop, leg_stat, "prop_of_subpop_ADP_admrel", prop_of_subpop_ADP_admrel, 
                                                              NA)
              
              new_df_inc_rate_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                       current_sjc_year, current_quarter, current_quarter_long,
                                                       race, pop, leg_stat, "Inc_rate_ADP_admrel", inc_rate_ADP_admrel, 
                                                       NA)
              
              new_df_ADP_admrel_disprop_ratio <- data.frame(current_site, current_year, current_cohort,
                                                            current_sjc_year, current_quarter, current_quarter_long,
                                                            race, pop, leg_stat, "ADP_admrel_disprop_ratio", ADP_admrel_disprop_ratio, 
                                                            NA)
              
              
              colnames(new_df_prop_of_sub_pop_ADP_admrel) <- colnames(df) # get column names from current_df
              colnames(new_df_inc_rate_ADP_admrel) <- colnames(df)
              colnames(new_df_ADP_admrel_disprop_ratio) <- colnames(df)
              
              final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_admrel,
                                new_df_inc_rate_ADP_admrel,
                                new_df_ADP_admrel_disprop_ratio) # add new row before going to the next iteration              
              }
            }
          }
          else { # This is if sub_pop == severity
            for (sev in severity_sub) {
              # We are just going to calculate for all_pop in this loop
              # ADP admrel 
              current_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                              race_ethn == race, pop_cat == pop, 
                                              sub_pop == sev, measure == "ADP_admrel")
              total_df_ADP_admrel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                            race_ethn == "all_race_ethn", pop_cat == pop, 
                                            sub_pop == sev, measure == "ADP_admrel")
              
              if ((dim(current_df_ADP_admrel)[1] == 0)) {
                next # if data.frame is empty move to the next iteration
              }
              
              else { # if the data frame has rows proceed to the calculation
                # get population for site and year
                current_year <- unique(current_df_ADP_admrel$year)
                cat("Adding", current_site, current_year, current_quarter, race, sev, "\n", " ")
                current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
                
                if (current_year >= 2020) { # We only have populations from 2015 - 2020
                  current_year_in_loop <- 2020
                }
                
                pop_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == race,
                                 pop_cat == pop, measure == "gen_adult_pop") # filter for adult population
                pop_total_df <- filter(df, site == current_site, year == current_year_in_loop, race_ethn == "all_race_ethn",
                                       measure == "gen_adult_pop") # filter for white adult population  `
                
                race_pop <- pop_df[, "value"]
                total_pop <- pop_total_df[, "value"]
                
                race_ADP_admrel <- current_df_ADP_admrel[, "value"] # get population
                total_ADP_admrel <- total_df_ADP_admrel[, "value"] # get total ADP_admrel
                
                prop_of_subpop_ADP_admrel <- round(race_ADP_admrel / total_ADP_admrel, 3)
                
                inc_rate_ADP_admrel <- round((race_ADP_admrel / race_pop) * 100000, 3) # normalize per 100,000
                
                ADP_admrel_disprop_ratio <- round(((race_ADP_admrel / total_ADP_admrel) / (race_pop / total_pop)), 3)
                
                current_cohort <- unique(current_df_ADP_admrel$cohort) # get cohort from filtered df
                current_sjc_year <- unique(current_df_ADP_admrel$sjc_year) # get sjc_year from filtered df
                current_quarter_long <- unique(current_df_ADP_admrel$quarter) # get current quarter
                
                new_df_prop_of_sub_pop_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                                current_sjc_year, current_quarter, current_quarter_long,
                                                                race, pop, sev, "prop_of_subpop_ADP_admrel", prop_of_subpop_ADP_admrel, 
                                                                NA)
                
                new_df_inc_rate_ADP_admrel <- data.frame(current_site, current_year, current_cohort,
                                                         current_sjc_year, current_quarter, current_quarter_long,
                                                         race, pop, sev, "Inc_rate_ADP_admrel", inc_rate_ADP_admrel, 
                                                         NA)
                
                new_df_ADP_admrel_disprop_ratio <- data.frame(current_site, current_year, current_cohort,
                                                              current_sjc_year, current_quarter, current_quarter_long,
                                                              race, pop, sev, "ADP_admrel_disprop_ratio", ADP_admrel_disprop_ratio, 
                                                              NA)
                
                
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
  }
  return(final_df)
}

quarter_df <- add_prop_inc_rate_ADP_admrel(quarter_df) # Call function for ADP_admrel rates

add_prop_and_inc_rate_ADP_snapshots <- function(df) {
  final_df <- df # initialize final_df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W") 
  pop_cat_list <-  c("all_pop", "leg_stat_snap", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  
  for(current_site in unique(df$site)) {
    for (current_quarter in unique(df$sjc_quarter)) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            # We are just going to calculate for all_pop in this loop
            
            # ADP_snapshot
            current_df_ADP_snapshot <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                              race_ethn == race, pop_cat == "all_pop", measure == "ADP_snapshot")
            total_df_ADP_snapshot <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                            race_ethn == "all_race_ethn", pop_cat == "all_pop", measure == "ADP_snapshot")
            
            if (dim(current_df_ADP_snapshot)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ADP_snapshot$year)
              cat("Adding", current_site, current_year, current_quarter, race, "\n", " ")
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
              current_quarter_long <- unique(current_df_ADP_snapshot$quarter) # get current quarter
              
              new_df_prop_of_sub_pop_ADP_snapshots <- data.frame(current_site, current_year, current_cohort,
                                                                 current_sjc_year, current_quarter, current_quarter_long,
                                                                 race, "all_pop", NA, "prop_of_subpop_ADP_snapshot", prop_of_subpop_ADP_snapshots, 
                                                                 NA)
              
              new_df_inc_rate_ADP_snapshots <- data.frame(current_site, current_year, current_cohort,
                                                          current_sjc_year, current_quarter, current_quarter_long,
                                                          race, "all_pop", NA, "Inc_rate_ADP_snapshot", inc_rate_ADP_snapshots, 
                                                          NA)
              
              new_df_ADP_snapshot_disprop_ratio <- data.frame(current_site, current_year, current_cohort,
                                                              current_sjc_year, current_quarter, current_quarter_long,
                                                              race, "all_pop", NA, "ADP_snapshot_disprop_ratio", ADP_snapshot_disprop_ratio, 
                                                              NA)
              
              
              
              colnames(new_df_prop_of_sub_pop_ADP_snapshots) <- colnames(current_df_ADP_snapshot)
              colnames(new_df_inc_rate_ADP_snapshots) <- colnames(current_df_ADP_snapshot)
              colnames(new_df_ADP_snapshot_disprop_ratio) <- colnames(current_df_ADP_snapshot)
              
              final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_snapshots,
                                new_df_inc_rate_ADP_snapshots,
                                new_df_ADP_snapshot_disprop_ratio) # add new row before going to the next iteration            
              }
            }
         else if (pop == "leg_stat_snap") {
           for (leg_stat in leg_stat) {
             # ADP_snapshot
             current_df_ADP_snapshot <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                               race_ethn == race, pop_cat == pop, 
                                               sub_pop == leg_stat, measure == "ADP_snapshot")
             total_df_ADP_snapshot <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                             race_ethn == "all_race_ethn", pop_cat == pop, 
                                             sub_pop == leg_stat, measure == "ADP_snapshot")
             
             if (dim(current_df_ADP_snapshot)[1] == 0) {
               next # if data.frame is empty move to the next iteration
             }
             
             else { # if the data frame has rows proceed to the calculation
               # get population for site and year
               current_year <- unique(current_df_ADP_snapshot$year)
               cat("Adding", current_site, current_year, current_quarter, race, leg_stat, "\n", " ")
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
               current_quarter_long <- unique(current_df_ADP_snapshot$quarter) # get current quarter
               
               new_df_prop_of_sub_pop_ADP_snapshots <- data.frame(current_site, current_year, current_cohort,
                                                                  current_sjc_year, current_quarter, current_quarter_long,
                                                                  race, pop, leg_stat, "prop_of_subpop_ADP_snapshot", prop_of_subpop_ADP_snapshots, 
                                                                  NA)
               
               new_df_inc_rate_ADP_snapshots <- data.frame(current_site, current_year, current_cohort,
                                                           current_sjc_year, current_quarter, current_quarter_long,
                                                           race, pop, leg_stat, "Inc_rate_ADP_snapshot", inc_rate_ADP_snapshots, 
                                                           NA)
               
               new_df_ADP_snapshot_disprop_ratio <- data.frame(current_site, current_year, current_cohort,
                                                               current_sjc_year, current_quarter, current_quarter_long,
                                                               race, pop,leg_stat, "ADP_snapshot_disprop_ratio", ADP_snapshot_disprop_ratio, 
                                                               NA)
               
               
               
               colnames(new_df_prop_of_sub_pop_ADP_snapshots) <- colnames(current_df_ADP_snapshot)
               colnames(new_df_inc_rate_ADP_snapshots) <- colnames(current_df_ADP_snapshot)
               colnames(new_df_ADP_snapshot_disprop_ratio) <- colnames(current_df_ADP_snapshot)
               
               final_df <- rbind(final_df, new_df_prop_of_sub_pop_ADP_snapshots,
                                 new_df_inc_rate_ADP_snapshots,
                                 new_df_ADP_snapshot_disprop_ratio) # add new row before going to the next iteration                        
           }
         }
        }
      }
    }
  }
}  


add_ALOS_rel <- function(df) {
  final_df <- df
  race_ethn_list <- c("AIAN", "all_race_ethn", "API", "B", "L", "POC", "W")
  pop_cat_list <-  c("all_pop", "leg_stat_snap", "severity")
  leg_stat_sub <- c("awaiting_action", "pretrial", "pretrial_awaitingaction", "sentenced", "violation")
  severity_sub <- c("fel", "misd")
  for (current_site in unique(df$site)) {
    for (current_quarter in unique(df$quarte)) {
      for (race in race_ethn_list) {
        for (pop in pop_cat_list) {
          if (pop == "all_pop") {
            # We are just going to calculate for all_pop in this loop
            # get current year
            # ALOS_rel
            current_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                          race_ethn == race, pop_cat == "all_pop", measure == "ALOS_rel")
            white_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                        race_ethn == "W", pop_cat == "all_pop", measure == "ALOS_rel")
            
            if (dim(current_df_ALOS_rel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ALOS_rel$year)
              cat("Adding", current_site, current_quarter, race, current_year, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              race_ALOS_rel <- current_df_ALOS_rel[, "value"] # get population
              white_ALOS_rel <- white_df_ALOS_rel[, "value"] # get total ALOS_rel
              
              ALOS_rel_disparity_ratio <- round(race_ALOS_rel / white_ALOS_rel, 3)
              
              current_cohort <- unique(current_df_ALOS_rel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ALOS_rel$sjc_year) # get sjc_year from filtered df
              current_quarter_long <- unique(current_df_ALOS_rel$quarter) # get current quarter
              
              
              new_df_ALOS_rel_disparity_ratio <- data.frame(current_site, current_year, current_cohort,
                                                          current_sjc_year, current_quarter, current_quarter_long,
                                                          race, "all_pop", NA, "ALOS_rel_disparity_ratio", ALOS_rel_disparity_ratio, 
                                                          NA)
              
              
              colnames(new_df_ALOS_rel_disparity_ratio) <- colnames(current_df_ALOS_rel) # get column names from current_df
              
              final_df <- rbind(final_df, new_df_ALOS_rel_disparity_ratio) # add new row before going to the next iteration 
            
            
            }
          }
        else if (pop == "leg_stat_snap") {
          for (leg_stat in leg_stat_sub) {
            # We are just going to calculate for all_pop in this loop
            # get current year
            # ALOS_rel
            current_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                          race_ethn == race, pop_cat == pop, 
                                          sub_pop == leg_stat, measure == "ALOS_rel")
            white_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                        race_ethn == "W", pop_cat == pop, 
                                        sub_pop == leg_stat, measure == "ALOS_rel")
            
            if (dim(current_df_ALOS_rel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ALOS_rel$year)
              cat("Adding", current_site, current_quarter, race, current_year, sub_pop, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              race_ALOS_rel <- current_df_ALOS_rel[, "value"] # get population
              white_ALOS_rel <- white_df_ALOS_rel[, "value"] # get total ALOS_rel
              
              ALOS_rel_disparity_ratio <- round(race_ALOS_rel / white_ALOS_rel, 3)
              
              current_cohort <- unique(current_df_ALOS_rel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ALOS_rel$sjc_year) # get sjc_year from filtered df
              current_quarter_long <- unique(current_df_ALOS_rel$quarter) # get current quarter
              
              
              new_df_ALOS_rel_disparity_ratio <- data.frame(current_site, current_year, current_cohort,
                                                          current_sjc_year, current_quarter, current_quarter_long,
                                                          race, pop, leg_stat, "ALOS_rel_disparity_ratio", ALOS_rel_disparity_ratio, 
                                                          NA)
              
              
              colnames(new_df_ALOS_rel_disparity_ratio) <- colnames(current_df_ALOS_rel) # get column names from current_df
              
              final_df <- rbind(final_df, new_df_ALOS_rel_disparity_ratio) # add new row before going to the next iteration 
              }
            }
          }
        else { # When pop == severity
          for (sev in severity_sub) {
            # We are just going to calculate for all_pop in this loop
            # get current year
            # ALOS_rel
            current_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                          race_ethn == race, pop_cat == pop, 
                                          sub_pop == sev, measure == "ALOS_rel") # change sub_pop to value in loop
            white_df_ALOS_rel <- filter(df, site == current_site, sjc_quarter == current_quarter, 
                                        race_ethn == "W", pop_cat == pop, 
                                        sub_pop == sev, measure == "ALOS_rel")
            
            if (dim(current_df_ALOS_rel)[1] == 0) {
              next # if data.frame is empty move to the next iteration
            }
            
            else { # if the data frame has rows proceed to the calculation
              # get population for site and year
              current_year <- unique(current_df_ALOS_rel$year)
              cat("Adding", current_site, current_quarter, race, current_year, sub_pop, "\n", " ")
              current_year_in_loop <- current_year # initialize to year in loop...might need to be changed within the loop
              
              if (current_year >= 2020) { # We only have populations from 2015 - 2020
                current_year_in_loop <- 2020
              }
              
              race_ALOS_rel <- current_df_ALOS_rel[, "value"] # get population
              white_ALOS_rel <- white_df_ALOS_rel[, "value"] # get total ALOS_rel
              
              ALOS_rel_disparity_ratio <- round(race_ALOS_rel / white_ALOS_rel, 3)
              
              current_cohort <- unique(current_df_ALOS_rel$cohort) # get cohort from filtered df
              current_sjc_year <- unique(current_df_ALOS_rel$sjc_year) # get sjc_year from filtered df
              current_quarter_long <- unique(current_df_ALOS_rel$quarter) # get current quarter
              
              
              new_df_ALOS_rel_disparity_ratio <- data.frame(current_site, current_year, current_cohort,
                                                            current_sjc_year, current_quarter, current_quarter_long,
                                                            race, pop, leg_stat, "ALOS_rel_disparity_ratio", ALOS_rel_disparity_ratio, 
                                                            NA)
              
              
              colnames(new_df_ALOS_rel_disparity_ratio) <- colnames(current_df_ALOS_rel) # get column names from current_df
              
              final_df <- rbind(final_df, new_df_ALOS_rel_disparity_ratio) # add new row before going to the next iteration 
          
              } 
            }
          }
        }
      }
  
    }
  }
  return(final_df)
}

quarter_df <- add_ALOS_rel(quarter_df)



