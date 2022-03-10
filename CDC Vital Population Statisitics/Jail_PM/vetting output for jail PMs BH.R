################################################################################
# Authors: Kailey Spencer
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
library(scales)
library("xlsx")

################################################

#IMPORTS

jailpm <- read.csv("P4. Generated Output (Final Output)/jail_pm_BLtoApr2021_allsites.csv",
                   stringsAsFactors=F)

################################################

#ADP - admissions/releases

ADP_vet <- jailpm %>% 
  mutate(quar_num=paste(quarter, quarter_range),
         value = round(value, digits=0),
         sub_pop = ifelse(sub_pop=="under 25", "24 and under", sub_pop)) %>% 
  filter(measure=="ADP_admrel" & site=="PIM" & 
           !(sub_pop %in% c("pretrial", "awaiting_action", "not_freq_utl"))) %>%
  group_by(measure, race_ethn, pop_cat, sub_pop) %>% 
  select(-calc_n, -quarter, -quarter_range, -year, -sjc_year, -sjc_cohort, -race_ethn_drop) %>%
  pivot_wider(names_from = quar_num, values_from = value) %>% 
  mutate_at(vars(starts_with(c("4", "8", "12", "16", "20"))), 
            funs(pct_chg_bl=(.-`0 Baseline`)/`0 Baseline`)) %>% 
  mutate_at(vars(ends_with("pct_chg_bl")), funs(ifelse(`0 Baseline`<20, NA, .))) %>% 
  select(measure, site:sub_pop, order(as.numeric(str_sub(colnames(.), 1,2)))) %>% 
  arrange(match(pop_cat, c("all_pop", "leg_stat_booking", "severity", "freq_utl",
                           "age", "sex")), sub_pop,
          match(race_ethn, c("all_race_ethn", "POC", "W", "AIAN", "API", "B", "L")))

columns <- colnames(ADP_vet)
ADP_vet <- data.frame(ADP_vet)

colnames(ADP_vet) <- columns

write.csv(ADP_vet, "P6. Vetting documents/sample_ADP_Pima.csv", row.names = F)

write.xlsx(ADP_vet, file = "P6. Vetting documents/sample_ADP_Pima.xlsx",  sheetName = "Sheet1",
           row.names = FALSE) # Export dataframe as a Excel Workbook


# Import template shell


ADP_workbook <- loadWorkbook("P6. Vetting documents/sample_ADP_Pima.xlsx")

ADP_workbook_empty <- loadWorkbook("P6. Vetting documents/excel template for jail PM vetting empty.xlsx")

sheets <- getSheets(ADP_workbook_empty) # get the sheet names of the ADP workbook


# Define pct_chg columns
percent_change_columns <- c("4 Feb 17 - Apr 17_pct_chg_bl", "8 Feb 18 - Apr 18_pct_chg_bl",
                            "12 Feb 19 - Apr 19_pct_chg_bl", "16 Feb 20 - Apr 20_pct_chg_bl",
                            "20 Feb 21 - Apr 21_pct_chg_bl")

sheets <- getSheets(ADP_workbook) # get the sheet names of the ADP workbook
ADP_sheet <- sheets[["sample_ADP_Pima"]] # get Sheet1 for adp

# import dataframe into the empty excel sheet
ADP_sheet <- addDataFrame(ADP_vet, ADP_sheet, col.names = TRUE, startRow = 3,
                          start)


for (pct_chg in percent_change_columns) {
  
  colindex <- grep(pct_chg, colnames(ADP_vet))
  
  cb_style <- CellStyle(ADP_workbook, dataFormat = DataFormat("0.00%"))  # Create cell style object for setColData
  
  
  cell_block <- CellBlock(ADP_sheet, 2, colindex, dim(ADP_vet)[1], 1) # define a cell block for percent change
  
  pct_chg_column <- ADP_vet[, pct_chg]
  
  positive_index <- which(pct_chg_column >= 0.045)
  negative_index <- which(pct_chg_column <= -0.045)
  neutral_index <- which(pct_chg_column < 0.045 & pct_chg_column > -0.045)
  null_index <- which(is.na(pct_chg_column))
  
  
  fill_pos_chg <- Fill(foregroundColor = "red", backgroundColor = "red") # positive change in ADP will be red
  fill_neg_chg <- Fill(foregroundColor = "green", backgroundColor = "green") # negative change in ADP will be red
  fill_neutral_chg <- Fill(foregroundColor  = "yellow", backgroundColor = "yellow") # neutral change in ADP will be red
  fill_null_chg <- Fill(foregroundColor = "lightblue", backgroundColor = "lightblue") # NA values in ADP will be turn blue

  CB.setColData(cell_block, pct_chg_column, colIndex = 1, rowOffset = 0, showNA = FALSE,  colStyle = cb_style) # population pct_change cell block with pct_chg values
  
  # Fill pct_chg column with appropriate color scheme
  CB.setFill(cell_block, fill_pos_chg, positive_index, 1)
  CB.setFill(cell_block, fill_neg_chg, negative_index, 1)
  CB.setFill(cell_block, fill_null_chg, null_index, 1)
  CB.setFill(cell_block, fill_neutral_chg, neutral_index, 1)

  
}

saveWorkbook(ADP_workbook, file = "P6. Vetting documents/sample_ADP_Pima_auto_sample.xlsx")
