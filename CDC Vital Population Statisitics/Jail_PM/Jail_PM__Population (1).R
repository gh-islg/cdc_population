# Brian Holliday
# Jail PM Measurements Script
# 2/9/22

# Goals of this Script

# 1. Join Jail PM Data with most recent CDC Population Data from SJC Sites
# 2. Produce Quarterly Measurements for Jail PM Statistics for every race, ethnicity, and sub population and for each site

# import libraries
library("plyr")
library("dplyr")
library("tidyr")
library("tibble")
library("xlsx")
library("stringr")



# cdc_pop <- filter(cdc_pop, site_name %in% jail_pm_sites) # filter for jail_pm

jail_pm <- read.csv("C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/monthly_jail_measures_all_sites_BLtoY5 (1).csv") # import Jail PM Dataset

jail_pm_full <- read.csv("C:/Users/Reagan/Documents/GitHub/cdc_population/CDC Vital Population Statisitics/Jail_PM/Jail PM Data/monthly_jail_measures_all_sites_BLtoY5_full.csv") # import Jail PM Dataset

# make quarter variable
month_to_quarter_dict <- c("2015-11"="Nov 15 - Apr 16", "2015-12"="Nov 15 - Apr 16", 
                           "2016-1"="Nov 15 - Apr 16", "2016-2"="Nov 15 - Apr 16", 
                           "2016-3"="Nov 15 - Apr 16", "2016-4"="Nov 15 - Apr 16",  
                           "2016-5"="May 16 - Jul 16", "2016-6"="May 16 - Jul 16", 
                           "2016-7"="May 16 - Jul 16", "2016-8"="Aug 16 - Oct 16",  
                           "2016-9"="Aug 16 - Oct 16", "2016-10"="Aug 16 - Oct 16",
                           "2016-11"="Nov 16 - Jan 17", "2016-12"="Nov 16 - Jan 17",  
                           "2017-1"="Nov 16 - Jan 17", "2017-2"="Feb 17 - Apr 17", 
                           "2017-3"="Feb 17 - Apr 17", "2017-4"="Feb 17 - Apr 17",
                           "2017-5"="May 17 - Jul 17", "2017-6"="May 17 - Jul 17",
                           "2017-7"="May 17 - Jul 17", "2017-8"="Aug 17 - Oct 17", 
                           "2017-9"="Aug 17 - Oct 17", "2017-10"="Aug 17 - Oct 17", 
                           "2017-11"="Nov 17 - Jan 18", "2017-12"="Nov 17 - Jan 18", 
                           "2018-1"="Nov 17 - Jan 18", "2018-2"="Feb 18 - Apr 18", 
                           "2018-3"="Feb 18 - Apr 18", "2018-4"="Feb 18 - Apr 18", 
                           "2018-5"="May 18 - Jul 18", "2018-6"="May 18 - Jul 18", 
                           "2018-7"="May 18 - Jul 18", "2018-8"="Aug 18 - Oct 18", 
                           "2018-9"="Aug 18 - Oct 18", "2018-10"="Aug 18 - Oct 18", 
                           "2018-11"="Nov 18 - Jan 19", "2018-12"="Nov 18 - Jan 19", 
                           "2019-1"="Nov 18 - Jan 19", "2019-2"="Feb 19 - Apr 19", 
                           "2019-3"="Feb 19 - Apr 19", "2019-4"="Feb 19 - Apr 19", 
                           "2019-5"="May 19 - Jul 19", "2019-6"="May 19 - Jul 19", 
                           "2019-7"="May 19 - Jul 19", "2019-8"="Aug 19 - Oct 19", 
                           "2019-9"="Aug 19 - Oct 19", "2019-10"="Aug 19 - Oct 19", 
                           "2019-11"="Nov 19 - Jan 20", "2019-12"="Nov 19 - Jan 20", 
                           "2020-1"="Nov 19 - Jan 20", "2020-2"="Feb 20 - Apr 20", 
                           "2020-3"="Feb 20 - Apr 20", "2020-4"="Feb 20 - Apr 20", 
                           "2020-5"="May 20 - Jul 20", "2020-6"="May 20 - Jul 20", 
                           "2020-7"="May 20 - Jul 20", "2020-8"="Aug 20 - Oct 20", 
                           "2020-9"="Aug 20 - Oct 20", "2020-10"="Aug 20 - Oct 20", 
                           "2020-11"="Nov 20 - Jan 21", "2020-12"="Nov 20 - Jan 21", 
                           "2021-1"="Nov 20 - Jan 21", "2021-2"="Feb 21 - Apr 21",
                           "2021-3"="Feb 21 - Apr 21", "2021-4"="Feb 21 - Apr 21",
                           "2021-5"="May 21 - Jul 21", "2021-6"="May 21 - Jul 21", 
                           "2021-7"="May 21 - Jul 21", "2021-8"="Aug 21 - Oct 21",
                           "2021-9"="Aug 21 - Oct 21", "2021-10"="Aug 21 - Oct 21", 
                           "2021-11"="Nov 21 - Dec 21", "2021-12"="Nov 21 - Dec 21",
                           "2015-10"="Oct 15 - Apr 16")

# Make year_month column to make quarterly measures
jail_pm$year_month <- paste(jail_pm$year, jail_pm$month, sep = "-")
jail_pm_full$year_month <- paste(jail_pm_full$year, jail_pm_full$month, sep = "-")




# Use a for loop to join the individual populations
for (i in seq(1, length(month_to_quarter_dict))) {
  insert_rows <- jail_pm$year_month == names(month_to_quarter_dict[i])
  jail_pm[insert_rows, "sjc_quarter"] <- month_to_quarter_dict[i]
}

# Use a for loop to join the individual populations
for (i in seq(1, length(month_to_quarter_dict))) {
  insert_rows <- jail_pm_full$year_month == names(month_to_quarter_dict[i])
  jail_pm_full[insert_rows, "sjc_quarter"] <- month_to_quarter_dict[i]
}

# Drop year month_year
jail_pm <- select(jail_pm, -year_month)

jail_pm_full <- select(jail_pm_full, -year_month)

# Find rows where quarter sjc quarter is null
null_quarter <- which(is.na(jail_pm$sjc_quarter))
jail_pm_not_null <- jail_pm[-null_quarter, ]

# Find rows where quarter sjc quarter is null
null_quarter_full <- which(is.na(jail_pm_full$sjc_quarter))
jail_pm_not_null_full <- jail_pm_full[-null_quarter_full, ]

# split values that need to be averaged versus summed
avg_measures <- c("ADP_admrel", "ALOS_rel", "ADP_snapshot", "ALOS_conf")
sum_measures <- "bookings"
# Define Average dataframe vs summed  
jail_pm_avg <- filter(jail_pm_not_null, measure %in% avg_measures) # filter average measures
jail_pm_sum <- filter(jail_pm_not_null, measure == sum_measures) # filter sum measures

jail_pm_avg_full <- filter(jail_pm_not_null_full, measure %in% avg_measures) # filter average measures
jail_pm_sum_full <- filter(jail_pm_not_null_full, measure == sum_measures) # filter sum measures

######### Sum Values by SJC_Quarter #################
values_by_quarter <- function(df, agg_func) {

  # initialize empty dataframe with columns
  final_df <- data.frame(site = character(),
                         year = integer(),
                         sjc_quarter = character(),
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
          avg_df <- aggregate(avg_df[, 11:12], by = list(avg_df$sjc_quarter, avg_df$sub_pop, avg_df$measure),
                              FUN = mean)
          colnames(avg_df) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")
          # Add columns
          avg_df <- add_column(avg_df, site = current_site, .before = "sjc_quarter")
          avg_df <- add_column(avg_df, race_ethn = race, .after = "sjc_quarter")
          current_year <- as.integer(paste("20", str_sub(avg_df$sjc_quarter, 5, 6), sep = ""))
          avg_df <- add_column(avg_df, year = current_year, .after = "site")
          avg_df <- add_column(avg_df, pop_cat = cat, .after = "race_ethn")
          avg_df <- add_column(avg_df, sjc_year = )
          # Bind and Join
          final_df <- rbind(final_df, avg_df)
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

          sum_df <- aggregate(sum_df[, 11:12], by = list(sum_df$sjc_quarter, sum_df$sub_pop, sum_df$measure),
                              FUN = sum)
          colnames(sum_df) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")
          # Add columns
          sum_df <- add_column(sum_df, site = current_site, .before = "sjc_quarter")
          sum_df <- add_column(sum_df, race_ethn = race, .after = "sjc_quarter")
          current_year <- as.integer(paste("20", str_sub(sum_df$sjc_quarter, 5, 6), sep = ""))
          sum_df <- add_column(sum_df, year = current_year, .after = "site")
          sum_df <- add_column(sum_df, pop_cat = cat, .after = "race_ethn")
          # Bind and join
          final_df <- rbind(final_df, sum_df)
          print("\n")
        }
        
      }
    }
  }
  
  # Add Cohort
  cohort_dict <- c("ALL"=3, "HAR"=1,
                   "MUL"=2, "PEN"=2)
  # Add Cohort Column
  final_df <- add_column(final_df, cohort = NA, .after = "year")
  
  # Use a for loop to join the individual populations
  for (i in seq(1, length(cohort_dict))) {
    insert_rows <- final_df$site == names(cohort_dict[i])
    final_df[insert_rows, "cohort"] <- cohort_dict[i]
  }
  
  # Add SJC Year (Add tomorrow)
#  cohort_dict <- c(2015="baseline", "HAR"=1,
#                   "MUL"=2, "PEN"=2)
  # Add Cohort Column
#  final_df <- add_column(final_df, cohort = NA, .after = "year")
  
  # Use a for loop to join the individual populations
#  for (i in seq(1, length(cohort_dict))) {
#    insert_rows <- final_df$site_name == names(cohort_dict[i])
#    final_df[insert_rows, "cohort"] <- cohort_dict[i]  

  return(final_df)
}

######### Sum Values by SJC_Quarter #################
values_by_quarter <- function(df, agg_func) {
  
  # initialize empty dataframe with columns
  final_df <- data.frame(site = character(),
                         year = integer(),
                         sjc_quarter = character(),
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
          avg_df <- aggregate(avg_df[, 11:12], by = list(avg_df$sjc_quarter, avg_df$sub_pop, avg_df$measure),
                              FUN = mean)
          colnames(avg_df) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")
          # Add columns
          avg_df <- add_column(avg_df, site = current_site, .before = "sjc_quarter")
          avg_df <- add_column(avg_df, race_ethn = race, .after = "sjc_quarter")
          current_year <- as.integer(paste("20", str_sub(avg_df$sjc_quarter, 5, 6), sep = ""))
          avg_df <- add_column(avg_df, year = current_year, .after = "site")
          avg_df <- add_column(avg_df, pop_cat = cat, .after = "race_ethn")
          avg_df <- add_column(avg_df, sjc_year = )
          # Bind and Join
          final_df <- rbind(final_df, avg_df)
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
          
          sum_df <- aggregate(sum_df[, 11:12], by = list(sum_df$sjc_quarter, sum_df$sub_pop, sum_df$measure),
                              FUN = sum)
          colnames(sum_df) <- c("sjc_quarter", "sub_pop", "measure", "value", "calc_n")
          # Add columns
          sum_df <- add_column(sum_df, site = current_site, .before = "sjc_quarter")
          sum_df <- add_column(sum_df, race_ethn = race, .after = "sjc_quarter")
          current_year <- as.integer(paste("20", str_sub(sum_df$sjc_quarter, 5, 6), sep = ""))
          sum_df <- add_column(sum_df, year = current_year, .after = "site")
          sum_df <- add_column(sum_df, pop_cat = cat, .after = "race_ethn")
          # Bind and join
          final_df <- rbind(final_df, sum_df)
          print("\n")
        }
        
      }
    }
  }
  
  # Add Cohort
  cohort_dict <- c("ALL"=3, "BUN"=3, "HAR"=1,
                   "MUL"=2, "PEN"=2, "COO"=2,
                   "MIL"=1, "NOR"=1, "PBC"=2,
                   "PHI"=1, "PIM"=1)
  # Add Cohort Column
  final_df <- add_column(final_df, cohort = NA, .after = "year")
  
  # Use a for loop to join the individual populations
  for (i in seq(1, length(cohort_dict))) {
    insert_rows <- final_df$site == names(cohort_dict[i])
    final_df[insert_rows, "cohort"] <- cohort_dict[i]
  }
  
  # Add SJC Year (Add tomorrow)
  #  cohort_dict <- c(2015="baseline", "HAR"=1,
  #                   "MUL"=2, "PEN"=2)
  # Add Cohort Column
  #  final_df <- add_column(final_df, cohort = NA, .after = "year")
  
  # Use a for loop to join the individual populations
  #  for (i in seq(1, length(cohort_dict))) {
  #    insert_rows <- final_df$site_name == names(cohort_dict[i])
  #    final_df[insert_rows, "cohort"] <- cohort_dict[i]  
  
  return(final_df)
}

quarter_df_sum <- values_by_quarter(jail_pm_sum, "sum")
quarter_df_avg  <- values_by_quarter(jail_pm_avg, "average")

quarter_df <- rbind(quarter_df_avg, quarter_df_sum)


# Join sjc_year to quarter_df dataset (revisit with full_data_set)
quarter_df <- add_column(quarter_df, sjc_year = NA, .before = "sjc_quarter")



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
cdc_pop_long$calc_n <- cdc_pop_long$value
 
colnames(cdc_pop_long) <- c("site", "year", "measure", "value", "calc_n")

cdc_pop_long <- add_column(cdc_pop_long, sjc_quarter = NA, .after = "year")
cdc_pop_long <- add_column(cdc_pop_long, cohort = NA, .after = "year")
cdc_pop_long <- add_column(cdc_pop_long, race_ethn = NA, .after = "sjc_quarter")
cdc_pop_long <- add_column(cdc_pop_long, pop_cat = NA, .after = "race_ethn")
cdc_pop_long <- add_column(cdc_pop_long, sub_pop = NA, .after = "pop_cat")

cohort_dict <- c("ALL"=3, "HAR"=1,
                 "MUL"=2, "PEN"=2)

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

cdc_pop_long$pop_cat <- "all_pop" # Add population category
cdc_pop_long$sub_pop <- "adults" # Add population category


# join jail_pm data with population data
# jail_pm <- left_join(jail_pm, cdc_pop[, 1:5], by = c("jail_pm_site_name"="site_name", "year"="year"))
# jail_pm$population <- NA

# join cdc_long to 

