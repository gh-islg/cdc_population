# CDC Population Statistics 2010-2020

This repository contains population counts for counties across the United States, including population counts for SJC Site locations. The main output of this repository is population totals for every county in each state with respect to age (adult and total populations), race, sex, and hispanic origin for every year between 2010 and 2020. For example, if you needed to find the population for a particular race in a particular county in 2013. The work for this repository was originally done in Stata, but files in this repository accomplish the same task in R.<br/><br/>
The output files are a manipulated version of the Centers for Disease Control __Bridged-Race Population Estimates__, which counts the total population in each county with respect to every age, hispanic origin, race and sex. This dataset also contains population estimates for the population of every age. For example, from the CDC file could you could find the 67-year-old population for a particular race in a particular county in 2013. The output excel and csv files from this repository sum the values for each age to get a count of populations for every age in a particular category. The original Bridged-Race Population Estimates SAS file can be downloaded as a zip file on the CDC's website here: https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#vintage2020. Documentation on the Bridged-Race Population Estimates could be found here: https://www.cdc.gov/nchs/data/nvss/bridged_race/Documentation-Bridged-PostcenV2020.pdf<br/><br/>
There are three folders in this repository. The folder __CDC Vital Population Statistics__ contains all files in the repository, most notably the ___CDC Vital Population Statistics.R___ file, which produces our outputs. The __Archive__ folder contains legacy files including the original Stata file (___cdc_clean_and_prep_2020.do___), the original output files for the Stata file, and an earlier version of the R code. The __Datasets__ folder contains four ouputs from the ___CDC Vital Population Statistics.R___ file. The ___cdc_populations_2010_2020___ file, which contains population totals for every county with respect age, race, sex, and hispanic origin. There is the also the ___sjc_site_populations_2010_2020___ files, which contains populations for counties that are specifically SJC Sites. Both of these files are in excel and csv format.<br/><br/>
The following populations for every year between 2010 and 2020 are included in the output files from the ___CDC Vital Population Statistics.R___ file and can be accessed from the __Datasets__ folder:<br/><br/>
__Total Population Any Ethnicity__<br/>
__Adult Population Any Ethnicity 18+__<br/>
__Adult Population Any Ethnicity 17+__<br/>
__Total Hispanic Population Any Race__<br/>
__Adult Hispanic Population Any Race 18+__<br/>
__Adult Hispanic Population Any Race 17+__<br/><br/>
For each race including (Black, White, Native American/Alaskan Native, and Asian/Pacific Islander) :<br/>
__Total Population__<br/>
__Adult Population Any Ethnicity +18__<br/>
__Total Population Non-Hispanic__<br/>
__Adult Population Non-Hispanic +18__<br/>
__Adult Population Any Ethnicity +17__<br/>
__Adult Population Non-Hispanic +17__
