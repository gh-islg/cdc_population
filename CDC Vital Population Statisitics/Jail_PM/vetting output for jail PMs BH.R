
################################################################################
# Authors: Kailey Spencer and Brian Holliday
# Date: 3/1/2022
# Last updated: 3/1/2022
# Task: Create vetting template output for each site with jail PMs
################################################################################

#INITIALIZATIONS
setwd("P:/SJC/03. Data and PM Management/Jail Data Performance Measures")
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library("xlsx")

################################################


#IMPORTS

jailpm <- read.csv("P4. Generated Output (Final Output)/jail_pm_BLtoApr2021_allsites.csv",
                   stringsAsFactors=F) %>% 
  filter(!(sub_pop %in% c("awaiting_action", "not_freq_utl"))) %>% 
  mutate(quar_num=paste(quarter, quarter_range),
         value = round(value, digits=0),
         sub_pop = recode(sub_pop, all_pop_sub = "n/a", freq_utl = "3+ admissions during SJC year",
                          pretrial = "pretrial only", pretrial_awaitingaction="pretrial/awaiting action",
                          sentenced = "sentenced only", violation = "violation only", misd = "misdemeanor",
                          fel = "felony", "under 25" = "24 and under"),
         race_ethn=recode(race_ethn, W = "White", POC = "People of color", B = "Black", L = "Latino", AIAN = "American Indian/Alaska Native",
                          API = "Asian/Pacific Islander", O = "Other", all_race_ethn = "All race/ethnicities combined"),
         pop_cat = recode(pop_cat, leg_stat_booking = "Legal status at admission",
                          age = "Age", sex = "Sex", leg_stat_snap = "Legal status at snapshot",
                          severity = "Severity of top charge", freq_utl = "Frequent utilizer")) %>%
  group_by(site, measure, race_ethn, pop_cat, sub_pop) %>% 
  mutate(small_n_baseline = ifelse((calc_n< 200 | is.na(calc_n)) & quarter==0, 1, NA)) %>% 
  tidyr::fill(small_n_baseline, .direction="updown") %>% 
  ungroup() %>% 
  arrange(site,measure, match(pop_cat, c("all_pop", "Legal status at admission", "Legal status at snapshot", 
                                         "Severity of top charge", "Frequent utilizer", "Age", "Sex")), 
          match(sub_pop, "pretrial/awaiting action"), sub_pop,
          match(race_ethn, c("All race/ethnicities combined", "American Indian/Alaska Native", 
                             "Asian/Pacific Islander", "Black", "Latino", "People of color", "White")))

################################################


  measures <- c("ADP_admrel", "ADP_snapshot", "ALOS_rel", "ALOS_conf", "bookings") # Define the measures
  
  sites <- c("COO", "HAR", "MIL", "MUL", "NOR", "PEN", "PBC", "CHA", "PHI",
             "ALL", "BUN", "PIM") # Define the sites that we current have data for
  
 
  sites_dict <- c("PEN"="Pennington", "PIM"="Pima", 
                  "CHA"="Charleston", "ALL"="Allegheny", "HAR"="Harris",
                  "MUL"="Multnomah", "SPO"="Spokane", "NOR"="New Orleans",
                  "BUN"="Buncombe", "COO"="Cook", "MIL"="Milwaukee",
                  "PBC"="Palm Beach", "PHI"="Philadelphia") # define a dictionary for the workbook path
  
get_site <- function(site_path) {
  
  return(sites_dict[site_path]) # get the current site names for the workbook path
  
}
        
  
addDataFrames_to_template_all_sites <- function(df) {
  
  for (current_site in sites) {      
    
    workbook_path <- paste("P6. Vetting documents/Empty Templates/", 
                             get_site(current_site), " Vetting Template.xlsx", sep = "")
      
    ADP_workbook <- loadWorkbook(workbook_path)
      
    for (current_measure in measures)   {
      
      ADP_vet <- data.frame(df)
      
      ADP_vet <- ADP_vet %>% filter(measure == current_measure & site == current_site)
      
      "%!in%" <- Negate("%in%")
      
      if (0 %!in% unique(ADP_vet[, "quarter"])) {
        next # if baseline quarter is not in the data frame skip in the loop
      }
      
      if (dim(ADP_vet)[1] == 0) {
        next # if the data frame is empty skip
      }
      
      ADP_vet <- jailpm %>% 
        filter(measure == current_measure & site == current_site) %>%
        group_by(measure, race_ethn, pop_cat, sub_pop) %>% 
        select(-calc_n, -quarter, -quarter_range, -year, -sjc_year, -sjc_cohort, -race_ethn_drop) %>%
        pivot_wider(names_from = quar_num, values_from = value) %>% 
        mutate_at(vars(starts_with(c("4", "8", "12", "16", "20"))), 
                  funs(pct_chg_bl=(.-`0 Baseline`)/`0 Baseline`)) %>% 
        mutate_at(vars(ends_with("pct_chg_bl")), funs(ifelse(!is.na(small_n_baseline), NA, .))) %>% 
        select(measure, site:sub_pop, order(as.numeric(str_sub(colnames(.), 1,2))),  -small_n_baseline) %>% 
        mutate(pop_cat = ifelse(pop_cat=="all_pop", "Total", pop_cat))
      
      columns <- colnames(ADP_vet) # get columns from dataframe
      ADP_vet <- data.frame(ADP_vet) # make sure that dataset is a dataframe
      colnames(ADP_vet) <- columns # assign column names
      
      
      ADP_join_df <- select(ADP_vet, pop_cat, sub_pop, race_ethn, everything(), -measure, -site)

      sheets <- getSheets(ADP_workbook) # get the sheet names of the ADP workbook
      
      ADP_sheet <- sheets[[current_measure]] # get Sheet1 for adp
      
      addDataFrame(ADP_join_df, ADP_sheet, col.names = FALSE, row.names = FALSE,
                   startRow = 3, startColumn = 1, showNA = FALSE)
      
      save_path <- paste("P6. Vetting documents/R-Updated/", 
                         get_site(current_site), " Vetting Template Output.xlsx", sep = "") # Save dataframe
      
      saveWorkbook(ADP_workbook, file = save_path)
      cat("\n", "Saved: ", current_site, current_measure, "\n", " ")
    }
  }
}

# addDataFrames_to_template_all_sites(jailpm)

addDataFrames_to_template <- function(df, current_site) {
  
  workbook_path <- paste("P6. Vetting documents/Empty Templates/", 
                         get_site(current_site), " Vetting Template.xlsx", sep = "")
  
  ADP_workbook <- loadWorkbook(workbook_path)
  
  for (current_measure in measures)   {
    
    ADP_vet <- data.frame(df)
    
    ADP_vet <- ADP_vet %>% filter(measure == current_measure & site == current_site)
    
    "%!in%" <- Negate("%in%")
    
    if (0 %!in% unique(ADP_vet[, "quarter"])) {
      next # if baseline quarter is not in the data frame skip in the loop
    }
    
    if (dim(ADP_vet)[1] == 0) {
      next # if the data frame is empty skip
    }
    
    ADP_vet <- jailpm %>% 
      filter(measure == current_measure & site == current_site) %>%
      group_by(measure, race_ethn, pop_cat, sub_pop) %>% 
      select(-calc_n, -quarter, -quarter_range, -year, -sjc_year, -sjc_cohort, -race_ethn_drop) %>%
      pivot_wider(names_from = quar_num, values_from = value) %>% 
      mutate_at(vars(starts_with(c("4", "8", "12", "16", "20"))), 
                funs(pct_chg_bl=(.-`0 Baseline`)/`0 Baseline`)) %>% 
      mutate_at(vars(ends_with("pct_chg_bl")), funs(ifelse(!is.na(small_n_baseline), NA, .))) %>% 
      select(measure, site:sub_pop, order(as.numeric(str_sub(colnames(.), 1,2))),  -small_n_baseline) %>% 
      mutate(pop_cat = ifelse(pop_cat=="all_pop", "Total", pop_cat))
    
    columns <- colnames(ADP_vet) # get columns from dataframe
    ADP_vet <- data.frame(ADP_vet) # make sure that dataset is a dataframe
    colnames(ADP_vet) <- columns # assign column names
    
    
    ADP_join_df <- select(ADP_vet, pop_cat, sub_pop, race_ethn, everything(), -measure, -site)
    
    sheets <- getSheets(ADP_workbook) # get the sheet names of the ADP workbook
    
    ADP_sheet <- sheets[[current_measure]] # get Sheet1 for adp
    
    addDataFrame(ADP_join_df, ADP_sheet, col.names = FALSE, row.names = FALSE,
                 startRow = 3, startColumn = 1, showNA = FALSE)
    
    save_path <- paste("P6. Vetting documents/R-Updated/", 
                       get_site(current_site), " Vetting Template Output.xlsx", sep = "") # Save dataframe
    
    saveWorkbook(ADP_workbook, file = save_path)
    cat("\n", "Saved: ", current_site, current_measure, "\n", " ")
  }
}

addDataFrames_to_template(jailpm, "ALL")
# addDataFrames_to_template(jailpm, "BUN")

# HARRIS IS A SPECIAL SITE FOR THIS FUNCTION DUE TO AVAILABLE DATA
cohorts_1and2 <- c("COO", "MIL", "MUL", "NOR", "PEN", "PIM", "PBC", "CHA", "PHI") # Define cohort 1 and 2 sites
cohort3 <- c("ALL", "BUN") # Define cohort 3 sites
 

format_pct_change <- function(current_site, df) {
  
  if (current_site %in% cohorts_1and2) {
    
    percent_change_columns <- c("4 Feb 17 - Apr 17_pct_chg_bl", "8 Feb 18 - Apr 18_pct_chg_bl",
                                "12 Feb 19 - Apr 19_pct_chg_bl", "16 Feb 20 - Apr 20_pct_chg_bl",
                                "20 Feb 21 - Apr 21_pct_chg_bl")
  }
  
  else if (current_site == "HAR") {
    percent_change_columns <- c("4 Feb 17 - Apr 17_pct_chg_bl", "8 Feb 18 - Apr 18_pct_chg_bl",
                                "12 Feb 19 - Apr 19_pct_chg_bl")
  }
  
  else {
    
    percent_change_columns <- c("4 Feb 19 - Apr 19_pct_chg_bl", "8 Feb 20 - Apr 20_pct_chg_bl",
                                "12 Feb 21 - Apr 21_pct_chg_bl")    
  }
  

  for (current_measure in measures) {
    
    ADP_vet <- data.frame(df)
    
    ADP_vet <- ADP_vet %>% filter(measure == current_measure & site == current_site)
  
    "%!in%" <- Negate("%in%")
  
    if (0 %!in% unique(ADP_vet[, "quarter"])) {
      next # if baseline quarter is not in the dataframe skip in the loop
    }
  
    if (dim(ADP_vet)[1] == 0) {
      next # if the data frame is empty skip
    }
    
    ADP_vet <- jailpm %>% 
      filter(measure == current_measure & site == current_site) %>%
      group_by(measure, race_ethn, pop_cat, sub_pop) %>% 
      select(-calc_n, -quarter, -quarter_range, -year, -sjc_year, -sjc_cohort, -race_ethn_drop) %>%
      pivot_wider(names_from = quar_num, values_from = value) %>% 
      mutate_at(vars(starts_with(c("4", "8", "12", "16", "20"))), 
                funs(pct_chg_bl=(.-`0 Baseline`)/`0 Baseline`)) %>% 
      mutate_at(vars(ends_with("pct_chg_bl")), funs(ifelse(!is.na(small_n_baseline), NA, .))) %>% 
      select(measure, site:sub_pop, order(as.numeric(str_sub(colnames(.), 1,2))),  -small_n_baseline) %>% 
      mutate(pop_cat = ifelse(pop_cat=="all_pop", "Total", pop_cat))

    columns <- colnames(ADP_vet) # get columns from dataframe
    
    ADP_vet <- data.frame(ADP_vet) # make sure that dataset is a dataframe
    colnames(ADP_vet) <- columns # assign column name
    
    ADP_join_df <- select(ADP_vet, pop_cat, sub_pop, race_ethn, everything(), -measure, -site)  
    
    workbook_path <- paste("P6. Vetting documents/R-Updated/", 
                         get_site(current_site), " Vetting Template Output.xlsx", sep = "") # get file paths
  
    ADP_workbook <- loadWorkbook(workbook_path) # open workbook  
    
    sheets <- getSheets(ADP_workbook) # get the sheet names of the ADP workbook
    ADP_sheet <- sheets[[current_measure]] # get current sheet aligned with the measure      
    
    for (pct_chg in percent_change_columns) {
    
      pct_chg_column <- ADP_join_df[, pct_chg]
  
      positive_index <- which(pct_chg_column >= 0.045)
      negative_index <- which(pct_chg_column <= -0.045)
      neutral_index <- which(pct_chg_column < 0.045 & pct_chg_column > -0.045)
      null_index <- which(is.na(pct_chg_column))
  
      fill_pos_chg <- Fill(foregroundColor = "indianred1", backgroundColor = "indianred1") # positive change in ADP will be red
      fill_neg_chg <- Fill(foregroundColor = "darkolivegreen2", backgroundColor = "darkolivegreen2") # negative change in ADP will be red
      fill_neutral_chg <- Fill(foregroundColor  = "gold1", backgroundColor = "gold1") # neutral change in ADP will be red
      fill_null_chg <- Fill(foregroundColor = "grey90", backgroundColor = "grey90") # NA values in ADP will be turn blue
      
      colindex <- grep(pct_chg, colnames(ADP_join_df))
      
      cell_block <- CellBlock(ADP_sheet, 3, colindex, nrow(ADP_join_df), 1) # define a cell block for percent change
  
      cb_style <- CellStyle(ADP_workbook, dataFormat = DataFormat("0.00%"))  # Create cell style object for setColData
  
      CB.setColData(cell_block, pct_chg_column, colIndex = 1, rowOffset = 0, showNA = FALSE,  colStyle = cb_style) # population pct_change cell block with pct_chg values
  
      # Fill pct_chg column with appropriate color scheme
      if (length(positive_index > 0)) {
        CB.setFill(cell_block, fill_pos_chg, rowIndex = positive_index, colIndex = 1) # Fill positive index red
      }
      
      if (length(negative_index > 0)) {
        CB.setFill(cell_block, fill_neg_chg, rowIndex =  negative_index, colIndex = 1) # Fill negative index green
      }
      
      if (length(null_index > 0)) {
        CB.setFill(cell_block, fill_null_chg, rowIndex =  null_index, colIndex = 1) # Fill null index lightblue
      }
      
      if (length(neutral_index) > 0) {
        CB.setFill(cell_block, fill_neutral_chg, rowIndex =  neutral_index, colIndex = 1) # Fill neutral index yellow
      }
      
      save_path <- paste("P6. Vetting documents/R-Updated/", 
                     get_site(current_site), " Vetting Template Output.xlsx", sep = "") # Save dataframe
      
      saveWorkbook(ADP_workbook, file = save_path)
      
      cat("Saved: ", current_site, current_measure, pct_chg, "\n", " ") # print the screen
    } #pct_change loop
  } 
}

# Call format pct change function
format_pct_change("ALL", jailpm)
format_pct_change("BUN", jailpm)
format_pct_change("CHA", jailpm)
format_pct_change("COO", jailpm)
format_pct_change("HAR", jailpm)
format_pct_change("MIL", jailpm)
format_pct_change("MUL", jailpm)
format_pct_change("NOR", jailpm)
format_pct_change("PBC", jailpm)
format_pct_change("PEN", jailpm)
format_pct_change("PHI", jailpm)
format_pct_change("PIM", jailpm)

###### Add data to jail pm tab #############
add_jailpm_data <- function(current_site, df) {
  
  ###NOTE. we don't actually want to write out the 'site' columm, but i have left it in for filtering purposes
  jail_data_tab <- df %>% 
    select(-c(sjc_cohort, race_ethn_drop, quar_num, small_n_baseline, year)) %>%
    mutate(pop_cat=ifelse(pop_cat=="all_pop", "Total", pop_cat)) %>% 
    select(site, `SJC year` = sjc_year, `SJC quarter number`=quarter,
           `Quarter range` = quarter_range, `Race/ethncity group`=race_ethn,
           `Population category` = pop_cat, `Sub-population`=sub_pop,
           `Performance measure type`=measure, `Performance measure value`=value,
           `Sample size`=calc_n)
  
  jail_data_tab <- filter(jail_data_tab, site == current_site)
  jail_data_tab <- select(jail_data_tab, -site)
  
  columns <- colnames(jail_data_tab)
  jail_data_tab <- data.frame(jail_data_tab)
  colnames(jail_data_tab) <- columns
  
  
   workbook_path <- paste("P6. Vetting documents/R-Updated/", 
                         get_site(current_site), " Vetting Template Output.xlsx", sep = "") # get file paths
   
   ADP_workbook <- loadWorkbook(workbook_path) # open workbook
   
   sheets <- getSheets(ADP_workbook) # get the sheet names of the ADP workbook
   ADP_sheet <- sheets[["Full jail PM data"]] # get jail pm data sheet
   
   addDataFrame(jail_data_tab, ADP_sheet, col.names = TRUE, row.names = FALSE, ### Make sure that row.names == TRUE or unexpected behavior###
                startRow = 1, startColumn = 1, showNA = FALSE)
   
   save_path <- paste("P6. Vetting documents/R-Updated/", 
                      get_site(current_site), " Vetting Template Output.xlsx", sep = "") # Save dataframe
   
   saveWorkbook(ADP_workbook, file = save_path)
   
   cat("Saved ", current_site, " jail data", "\n") # print the screen
  
  # write.xlsx(jail_data_tab, file = save_path, sheetName = "Full jail PM data",
  #            col.names = TRUE, row.names = FALSE, append = TRUE)
}

# Call add jail PM data function
add_jailpm_data("ALL", jailpm)
add_jailpm_data("BUN", jailpm)
add_jailpm_data("CHA", jailpm)
add_jailpm_data("COO", jailpm)
add_jailpm_data("HAR", jailpm)
add_jailpm_data("MIL", jailpm)
add_jailpm_data("MUL", jailpm)
add_jailpm_data("NOR", jailpm)
add_jailpm_data("PBC", jailpm)
add_jailpm_data("PEN", jailpm)
add_jailpm_data("PHI", jailpm)
add_jailpm_data("PIM", jailpm)

refresh_formulas <- function(current_site) { # Functions to refresh formulas for graphs
  
  save_path <- paste("P6. Vetting documents/R-Updated/", 
                     get_site(current_site), " Vetting Template Output.xlsx", sep = "")  
  
  forceFormulaRefresh(save_path, output = NULL, verbose = FALSE) # xlsx function to reset formulas
  
  cat("Refreshed ", current_site, " formulas", "\n")
}

refresh_formulas("ALL")
refresh_formulas("BUN")
refresh_formulas("CHA")
refresh_formulas("COO")
refresh_formulas("HAR")
refresh_formulas("MIL")
refresh_formulas("MUL")
refresh_formulas("NOR")
refresh_formulas("PBC")
refresh_formulas("PEN")
refresh_formulas("PHI")
refresh_formulas("PIM")

.rs.restartR()