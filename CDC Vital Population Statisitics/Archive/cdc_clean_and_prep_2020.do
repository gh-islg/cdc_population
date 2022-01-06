*preps the Vital Population Statistics
/* Updates: 10.29.2020 	- Fixed spelling of Multnomah
						- Created a seperate SJC only file
						- Created another population grouping for 0-17 for EBR 
						- Collapsed 5 NYC Counties into one NYC Site using the sum function.
						- Changed the FIPS for NYC Site to equal that of NY County (36061)
   Updates: 3.2.2021	-Fixed spelling of East Baton Rouge
						-Changed NYC to New York City
						-Added Ada (16001) and Shelby County (01117)
   Update(MK): 10/20/21 - Updated Shelby County FIPS code (should've be 047157)
*/
clear
set more off
global fileloc "R:\SJC\Site information\CDC Population Data\2020-2010\CDC"
use "$fileloc\cdc_pop_2010-20.dta"

*fixes county code
tostring ST_FIPS, replace 
tostring CO_FIPS, replace


gen st_fips_1 = ST_FIPS
replace st_fips = "0" + ST_FIPS if strlen(ST_FIPS) == 1
tab st_fips_1

gen co_fips_1 = CO_FIPS
replace co_fips_1 = "0" + CO_FIPS if strlen(CO_FIPS) == 2
replace co_fips_1 = "00" + CO_FIPS if strlen(CO_FIPS) == 1
tab co_fips_1
gen fips_state_county_code = st_fips_1 + co_fips_1
drop ST_FIPS CO_FIPS
rename co_fips_1 co_fips
rename st_fips_1 st_fips
label var st_fips "State FIPS Code"
label var co_fips "County FIPS Code"
label var fips_state_county_code "State + County FIPS"

tab age

save "$fileloc\cdc_pop_2010-20_clean_fips.dta", replace

********************************************************************************
******************************2010**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2010
// black any ethnicity
gen adult_m_b = POP2010_JUL if RACESEX == 3 & age >= 18
gen adult_f_b = POP2010_JUL if RACESEX == 4 & age >= 18
gen adult_b = POP2010_JUL if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2010_JUL if RACESEX == 3 | RACESEX == 4

// black non hispanic
gen adult_m_b_nh = POP2010_JUL if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2010_JUL if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2010_JUL if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2010_JUL if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2010_JUL if RACESEX == 1 & age >= 18
gen adult_f_a = POP2010_JUL if RACESEX == 2 & age >= 18
gen adult_a = POP2010_JUL if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2010_JUL if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2010_JUL if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2010_JUL if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2010_JUL if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2010_JUL if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2010_JUL if RACESEX == 5 & age >= 18
gen adult_f_c = POP2010_JUL if RACESEX == 6 & age >= 18
gen adult_c = POP2010_JUL if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2010_JUL if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2010_JUL if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2010_JUL if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2010_JUL if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2010_JUL if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2010_JUL if RACESEX == 7 & age >= 18
gen adult_f_de = POP2010_JUL if RACESEX == 8 & age >= 18
gen adult_de = POP2010_JUL if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2010_JUL if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2010_JUL if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2010_JUL if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2010_JUL if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2010_JUL if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2010_JUL if age >= 18

// hispanic
gen adult_i = POP2010_JUL if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2010_JUL if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2010_JUL if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2010_JUL if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2010_JUL if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2010_JUL if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2010_JUL if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2010_JUL if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2010_JUL if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2010_JUL if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2010_JUL if age >= 17 & hisp == 2
//total adult pop
gen adult_pop_17 = POP2010_JUL if age >= 17


collapse (sum) adult_pop POP2010_JUL adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2010 
rename POP2010_JUL total_pop

save `2010'
********************************************************************************
******************************2011**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2011
// black any ethnicity
gen adult_m_b = POP2011 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2011 if RACESEX == 4 & age >= 18
gen adult_b = POP2011 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2011 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2011 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2011 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2011 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2011 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2011 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2011 if RACESEX == 2 & age >= 18
gen adult_a = POP2011 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2011 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2011 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2011 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2011 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2011 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2011 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2011 if RACESEX == 6 & age >= 18
gen adult_c = POP2011 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2011 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2011 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2011 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2011 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2011 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2011 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2011 if RACESEX == 8 & age >= 18
gen adult_de = POP2011 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2011 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2011 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2011 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2011 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2011 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2011 if age >= 18

// hispanic
gen adult_i = POP2011 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2011 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2011 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2011 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2011 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2011 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2011 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2011 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2011 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2011 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2011 if age >= 17 & hisp == 2 
//total adult pop
gen adult_pop_17 = POP2011 if age >= 17

collapse (sum) adult_pop POP2011 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)


gen year = 2011 
rename POP2011 total_pop

save `2011'
********************************************************************************
******************************2012**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2012
// black any ethnicity
gen adult_m_b = POP2012 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2012 if RACESEX == 4 & age >= 18
gen adult_b = POP2012 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2012 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2012 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2012 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2012 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2012 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2012 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2012 if RACESEX == 2 & age >= 18
gen adult_a = POP2012 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2012 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2012 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2012 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2012 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2012 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2012 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2012 if RACESEX == 6 & age >= 18
gen adult_c = POP2012 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2012 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2012 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2012 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2012 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2012 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2012 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2012 if RACESEX == 8 & age >= 18
gen adult_de = POP2012 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2012 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2012 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2012 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2012 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2012 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2012 if age >= 18

// hispanic
gen adult_i = POP2012 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2012 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2012 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2012 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2012 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2012 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2012 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2012 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2012 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2012 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2012 if age >= 17 & hisp == 2
//total adult pop
gen adult_pop_17 = POP2012 if age >= 17

collapse (sum) adult_pop POP2012 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)
gen year = 2012 
rename POP2012 total_pop

save `2012'
********************************************************************************
******************************2013**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2013
// black any ethnicity
gen adult_m_b = POP2013 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2013 if RACESEX == 4 & age >= 18
gen adult_b = POP2013 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2013 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2013 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2013 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2013 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2013 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2013 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2013 if RACESEX == 2 & age >= 18
gen adult_a = POP2013 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2013 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2013 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2013 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2013 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2013 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2013 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2013 if RACESEX == 6 & age >= 18
gen adult_c = POP2013 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2013 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2013 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2013 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2013 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2013 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2013 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2013 if RACESEX == 8 & age >= 18
gen adult_de = POP2013 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2013 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2013 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2013 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2013 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2013 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2013 if age >= 18

// hispanic
gen adult_i = POP2013 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2013 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2013 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2013 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2013 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2013 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2013 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2013 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2013 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2013 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2013 if age >= 17 & hisp == 2
//total adult pop
gen adult_pop_17 = POP2013 if age >= 17

collapse (sum) adult_pop POP2013 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2013 
rename POP2013 total_pop

save `2013'
********************************************************************************
******************************2014**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2014
// black any ethnicity
gen adult_m_b = POP2014 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2014 if RACESEX == 4 & age >= 18
gen adult_b = POP2014 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2014 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2014 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2014 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2014 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2014 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2014 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2014 if RACESEX == 2 & age >= 18
gen adult_a = POP2014 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2014 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2014 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2014 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2014 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2014 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2014 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2014 if RACESEX == 6 & age >= 18
gen adult_c = POP2014 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2014 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2014 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2014 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2014 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2014 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2014 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2014 if RACESEX == 8 & age >= 18
gen adult_de = POP2014 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2014 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2014 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2014 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2014 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2014 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2014 if age >= 18

// hispanic
gen adult_i = POP2014 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2014 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2014 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2014 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2014 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2014 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2014 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2014 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2014 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2014 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2014 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2014 if age >= 17

collapse (sum) adult_pop POP2014 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)
gen year = 2014 
rename POP2014 total_pop

save `2014'
********************************************************************************
******************************2015**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2015
// black any ethnicity
gen adult_m_b = POP2015 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2015 if RACESEX == 4 & age >= 18
gen adult_b = POP2015 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2015 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2015 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2015 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2015 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2015 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2015 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2015 if RACESEX == 2 & age >= 18
gen adult_a = POP2015 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2015 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2015 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2015 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2015 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2015 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2015 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2015 if RACESEX == 6 & age >= 18
gen adult_c = POP2015 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2015 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2015 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2015 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2015 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2015 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2015 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2015 if RACESEX == 8 & age >= 18
gen adult_de = POP2015 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2015 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2015 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2015 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2015 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2015 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2015 if age >= 18

// hispanic
gen adult_i = POP2015 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2015 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2015 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2015 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2015 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2015 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2015 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2015 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2015 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2015 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2015 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2015 if age >= 17


collapse (sum) adult_pop POP2015 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2015 
rename POP2015 total_pop

save `2015'
********************************************************************************
******************************2016**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2016
// black any ethnicity
gen adult_m_b = POP2016 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2016 if RACESEX == 4 & age >= 18
gen adult_b = POP2016 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2016 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2016 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2016 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2016 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2016 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2016 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2016 if RACESEX == 2 & age >= 18
gen adult_a = POP2016 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2016 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2016 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2016 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2016 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2016 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2016 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2016 if RACESEX == 6 & age >= 18
gen adult_c = POP2016 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2016 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2016 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2016 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2016 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2016 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2016 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2016 if RACESEX == 8 & age >= 18
gen adult_de = POP2016 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2016 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2016 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2016 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2016 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2016 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2016 if age >= 18

// hispanic
gen adult_i = POP2016 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2016 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2016 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2016 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2016 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2016 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2016 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2016 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2016 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2016 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2016 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2016 if age >= 17


collapse (sum) adult_pop POP2016 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2016 
rename POP2016 total_pop

save `2016'
********************************************************************************
******************************2017**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2017
// black any ethnicity
gen adult_m_b = POP2017 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2017 if RACESEX == 4 & age >= 18
gen adult_b = POP2017 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2017 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2017 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2017 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2017 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2017 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2017 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2017 if RACESEX == 2 & age >= 18
gen adult_a = POP2017 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2017 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2017 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2017 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2017 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2017 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2017 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2017 if RACESEX == 6 & age >= 18
gen adult_c = POP2017 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2017 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2017 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2017 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2017 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2017 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2017 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2017 if RACESEX == 8 & age >= 18
gen adult_de = POP2017 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2017 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2017 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2017 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2017 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2017 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2017 if age >= 18

// hispanic
gen adult_i = POP2017 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2017 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2017 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2017 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2017 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2017 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2017 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2017 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2017 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2017 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2017 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2017 if age >= 17

collapse (sum) adult_pop POP2017 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2017 
rename POP2017 total_pop

save `2017'
********************************************************************************
******************************2018**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2018
// black any ethnicity
gen adult_m_b = POP2018 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2018 if RACESEX == 4 & age >= 18
gen adult_b = POP2018 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2018 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2018 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2018 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2018 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2018 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2018 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2018 if RACESEX == 2 & age >= 18
gen adult_a = POP2018 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2018 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2018 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2018 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2018 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2018 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2018 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2018 if RACESEX == 6 & age >= 18
gen adult_c = POP2018 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2018 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2018 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2018 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2018 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2018 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2018 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2018 if RACESEX == 8 & age >= 18
gen adult_de = POP2018 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2018 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2018 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2018 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2018 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2018 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2018 if age >= 18

// hispanic
gen adult_i = POP2018 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2018 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2018 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2018 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2018 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2018 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2018 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2018 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2018 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2018 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2018 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2018 if age >= 17

collapse (sum) adult_pop POP2018 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2018 
rename POP2018 total_pop

save `2018'
********************************************************************************
******************************2019**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2019
// black any ethnicity
gen adult_m_b = POP2019 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2019 if RACESEX == 4 & age >= 18
gen adult_b = POP2019 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2019 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2019 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2019 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2019 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2019 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2019 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2019 if RACESEX == 2 & age >= 18
gen adult_a = POP2019 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2019 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2019 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2019 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2019 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2019 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2019 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2019 if RACESEX == 6 & age >= 18
gen adult_c = POP2019 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2019 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2019 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2019 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2019 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2019 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2019 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2019 if RACESEX == 8 & age >= 18
gen adult_de = POP2019 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2019 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2019 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2019 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2019 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2019 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2019 if age >= 18

// hispanic
gen adult_i = POP2019 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2019 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2019 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2019 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2019 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2019 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2019 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2019 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2019 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2019 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2019 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2019 if age >= 17

collapse (sum) adult_pop POP2019 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2019 
rename POP2019 total_pop

save `2019'


********************************************************************************
******************************2019**********************************************
********************************************************************************
clear
use "$fileloc\cdc_pop_2010-20_clean_fips.dta"
tempfile 2020
// black any ethnicity
gen adult_m_b = POP2020 if RACESEX == 3 & age >= 18
gen adult_f_b = POP2020 if RACESEX == 4 & age >= 18
gen adult_b = POP2020 if (RACESEX == 3 | RACESEX == 4) & age >= 18
gen pop_b = POP2020 if RACESEX == 3 | RACESEX == 4
// black non hispanic
gen adult_m_b_nh = POP2020 if RACESEX == 3 & age >= 18 & hisp == 1
gen adult_f_b_nh = POP2020 if RACESEX == 4 & age >= 18 & hisp == 1
gen adult_b_nh = POP2020 if (RACESEX == 3 | RACESEX == 4) & age >= 18 & hisp == 1
gen pop_b_nh = POP2020 if (RACESEX == 3 | RACESEX == 4) & hisp == 1

// white any ethnicity
gen adult_m_a = POP2020 if RACESEX == 1 & age >= 18
gen adult_f_a = POP2020 if RACESEX == 2 & age >= 18
gen adult_a = POP2020 if (RACESEX == 1 | RACESEX == 2) & age >= 18
gen pop_a = POP2020 if (RACESEX == 1 | RACESEX == 2)
// white non hispanic
gen adult_m_a_nh = POP2020 if RACESEX == 1 & age >= 18 & hisp == 1
gen adult_f_a_nh = POP2020 if RACESEX == 2 & age >= 18 & hisp == 1
gen adult_a_nh = POP2020 if (RACESEX == 1 | RACESEX == 2) & age >= 18 & hisp == 1
gen pop_a_nh = POP2020 if (RACESEX == 1 | RACESEX == 2) & hisp == 1

//native american alaskan native any ethnicity
gen adult_m_c = POP2020 if RACESEX == 5 & age >= 18
gen adult_f_c = POP2020 if RACESEX == 6 & age >= 18
gen adult_c = POP2020 if (RACESEX == 5 | RACESEX == 6) & age >= 18
gen pop_c = POP2020 if RACESEX == 5 | RACESEX == 6
//native american alaskan native any non hispanic 
gen adult_m_c_nh = POP2020 if RACESEX == 5 & age >= 18 & hisp == 1
gen adult_f_c_nh = POP2020 if RACESEX == 6 & age >= 18 & hisp == 1
gen adult_c_nh = POP2020 if (RACESEX == 5 | RACESEX == 6) & age >= 18 & hisp == 1
gen pop_c_nh = POP2020 if (RACESEX == 5 | RACESEX == 6) & hisp == 1

//asian pacific islander any ethnicity
gen adult_m_de = POP2020 if RACESEX == 7 & age >= 18
gen adult_f_de = POP2020 if RACESEX == 8 & age >= 18
gen adult_de = POP2020 if (RACESEX == 7 | RACESEX == 8) & age >= 18
gen pop_de = POP2020 if RACESEX == 7 | RACESEX == 8 
// asian pacific islander non hispanic
gen adult_m_de_nh = POP2020 if RACESEX == 7 & age >= 18 & hisp == 1
gen adult_f_de_nh = POP2020 if RACESEX == 8 & age >= 18 & hisp == 1
gen adult_de_nh = POP2020 if (RACESEX == 7 | RACESEX == 8) & age >= 18 & hisp == 1
gen pop_de_nh = POP2020 if (RACESEX == 7 | RACESEX == 8)  & hisp == 1
// adult pop
gen adult_pop = POP2020 if age >= 18

// hispanic
gen adult_i = POP2020 if age >= 18 & hisp == 2 /*MK*/
gen pop_i = POP2020 if hisp == 2 /*MK*/

// adult defined as 17+
//black adult 17
gen adult_17_b = POP2020 if (RACESEX == 3 | RACESEX == 4) & age >= 17
gen adult_17_b_nh = POP2020 if (RACESEX == 3 | RACESEX == 4) & age >= 17 & hisp == 1
//white adult 17
gen adult_17_a = POP2020 if (RACESEX == 1 | RACESEX == 2) & age >= 17
gen adult_17_a_nh = POP2020 if (RACESEX == 1 | RACESEX == 2) & age >= 17 & hisp == 1
//AIAN adult 17
gen adult_17_c = POP2020 if (RACESEX == 5 | RACESEX == 6) & age >= 17
gen adult_17_c_nh = POP2020 if (RACESEX == 5 | RACESEX == 6) & age >= 17 & hisp == 1
//API adult 17
gen adult_17_de = POP2020 if (RACESEX == 7 | RACESEX == 8) & age >= 17
gen adult_17_de_nh = POP2020 if (RACESEX == 7 | RACESEX == 8) & age >= 17 & hisp == 1
//hispanic adult 17
gen adult_17_i = POP2020 if age >= 17 & hisp == 2
//total adult pop 17
gen adult_pop_17 = POP2020 if age >= 17

collapse (sum) adult_pop POP2020 adult_m_b adult_f_b adult_b pop_b adult_m_a adult_f_a adult_a pop_a adult_m_c adult_f_c adult_c pop_c adult_m_de adult_f_de adult_de pop_de adult_m_b_nh adult_f_b_nh adult_b_nh pop_b_nh adult_m_a_nh adult_f_a_nh adult_a_nh pop_a_nh adult_m_c_nh adult_f_c_nh adult_c_nh pop_c_nh adult_m_de_nh adult_f_de_nh adult_de_nh pop_de_nh adult_i pop_i adult_17_b adult_17_b_nh adult_17_a adult_17_a_nh adult_17_c adult_17_c_nh adult_17_de adult_17_de_nh adult_17_i adult_pop_17, by(fips_state_county_code)

gen year = 2020 
rename POP2020 total_pop

save `2020'

********************************************************************************
******************************APPEND********************************************
********************************************************************************
append using `2019'
append using `2018'
append using `2017'
append using `2016'
append using `2015'
append using `2014'
append using `2013'
append using `2012'
append using `2011'
append using `2010'


label var fips_state_county_code "FIPS State County Code"
label var adult_pop "Total Adult Population 18+, Any Ethnicity"
label var total_pop "Total Population, Any Ethnicity"
label var adult_m_b "Total Adult Population: Black Male, Any Ethnicity"
label var adult_f_b "Total Adult Population: Black Female, Any Ethnicity"
label var adult_b "Total Adult Population: Black, Any Ethnicity"
label var pop_b "Total Population: Black, Any Ethnicity"
label var adult_m_a "Total Adult Population: White Male, Any Ethnicity"
label var adult_f_a "Total Adult Population: White Female, Any Ethnicity"
label var adult_a "Total Adult Population: White, Any Ethnicity"
label var pop_a "Total Population: White, Any Ethnicity"
label var adult_m_c "Total Adult Population: American Indian Alaskan Native Male, Any Ethnicity"
label var adult_f_c "Total Adult Population: American Indian Alaskan Native Female, Any Ethnicity"
label var adult_c "Total Adult Population: American Indian Alaskan Native, Any Ethnicity"
label var pop_c "Total Population: American Indian Alaskan Native, Any Ethnicity"
label var adult_m_de "Total Adult Population: Asian Pacific Male, Any Ethnicity"
label var adult_f_de "Total Adult Population: Asian Pacific Female, Any Ethnicity"
label var adult_de "Total Adult Population: Asian Pacific, Any Ethnicity"
label var pop_de "Total Population Asian Pacific, Any Ethnicity"
label var adult_i "Total Adult Population: Hispanic, Any Race"
label var pop_i "Total Population: Hispanic, Any Race"
label var year "Year"



label var adult_m_b_nh "Total Adult Population: Black Male, Non-Hispanic"
label var adult_f_b_nh "Total Adult Population: Black Female, Non-Hispanic"
label var adult_b_nh "Total Adult Population: Black, Non-Hispanic"
label var pop_b_nh "Total Population: Black, Non-Hispanic"
label var adult_m_a_nh "Total Adult Population: White Male, Non-Hispanic"
label var adult_f_a_nh "Total Adult Population: White Female, Non-Hispanic"
label var adult_a_nh "Total Adult Population: White, Non-Hispanic"
label var pop_a_nh "Total Population: White, Non-Hispanic"
label var adult_m_c_nh "Total Adult Population: American Indian Alaskan Native Male, Non-Hispanic"
label var adult_f_c_nh "Total Adult Population: American Indian Alaskan Native Female, Non-Hispanic"
label var adult_c_nh "Total Adult Population: American Indian Alaskan Native, Non-Hispanic"
label var pop_c_nh "Total Population: American Indian Alaskan Native, Non-Hispanic"
label var adult_m_de_nh "Total Adult Population: Asian Pacific Male, Non-Hispanic"
label var adult_f_de_nh "Total Adult Population: Asian Pacific Female, Non-Hispanic"
label var adult_de_nh "Total Adult Population: Asian Pacific, Non-Hispanic"
label var pop_de_nh "Total Population Asian Pacific, Non-Hispanic"

label var adult_17_b "Total Adult Population 17+: Black"
label var adult_17_b_nh "Total Adult Population 17+: Black, Non-Hispanic"
label var adult_17_a "Total Adult Population 17+: White"
label var adult_17_a_nh "Total Adult Population 17+: White, Non-Hispanic"
label var adult_17_c "Total Adult Population 17+: American Indian Alaskan Native"
label var adult_17_c_nh "Total Adult Population 17+: American Indian Alaskan Native, Non-Hispanic"
label var adult_17_de "Total Adult Population 17+: Asian Pacific"
label var adult_17_de_nh "Total Adult Population 17+: Asian Pacific, Non-Hispanic"
label var adult_17_i "Total Adult Population 17+: Hispanic"
label var adult_pop_17 "Total Adult Population 17+"


gen site_name = ""
replace site_name = "Ada" if fips_state_county_code == "16001"
replace site_name = "Shelby" if fips_state_county_code == "47157"
replace site_name = "Pennington" if fips_state_county_code == "46103"
replace site_name = "Missoula" if fips_state_county_code == "30063"
replace site_name = "Minnehaha" if fips_state_county_code == "46099"
replace site_name = "Pima" if fips_state_county_code == "04019"
replace site_name = "Charleston" if fips_state_county_code == "45019"
replace site_name = "New York City" if fips_state_county_code == "36047" //kings county
replace site_name = "New York City" if fips_state_county_code == "36061" // New York county
replace site_name = "New York City" if fips_state_county_code == "36005" // Bronx County
replace site_name = "New York City" if fips_state_county_code == "36085" // Richmond County
replace site_name = "New York City" if fips_state_county_code == "36081" // Queens County
replace site_name = "Mecklenburg" if fips_state_county_code == "37119"
replace site_name = "Los Angeles" if fips_state_county_code == "06037"
replace site_name = "Allegheny" if fips_state_county_code == "42003"
replace site_name = "Harris" if fips_state_county_code == "48201"
replace site_name = "Clark" if fips_state_county_code == "32003"
replace site_name = "Multnomah" if fips_state_county_code == "41051"
replace site_name = "Spokane" if fips_state_county_code == "53063"
replace site_name = "New Orleans" if fips_state_county_code == "22071"
replace site_name = "Buncombe" if fips_state_county_code == "37021"
replace site_name = "Cook" if fips_state_county_code == "17031"
replace site_name = "East Baton Rouge" if fips_state_county_code == "22033"
replace site_name = "Lake" if fips_state_county_code == "17097"
replace site_name = "Lucas" if fips_state_county_code == "39095"
replace site_name = "Milwaukee" if fips_state_county_code == "55079"
replace site_name = "Missoula" if fips_state_county_code == "30063"
replace site_name = "Palm Beach" if fips_state_county_code == "12099"
replace site_name = "Philadelphia" if fips_state_county_code == "42101"
replace site_name = "San Francisco" if fips_state_county_code == "06075"
replace site_name = "St. Louis" if fips_state_county_code == "29189"
label var site_name "Site Name"

save "$fileloc\cdc_pop_2010_20_master.dta", replace

use "$fileloc\cdc_pop_2010_20_master.dta", clear

rename adult_b pop_adult_black_any_ethnicity
rename pop_b pop_total_black_any_ethnicity
rename total_pop pop_total_any_ethnicity
rename adult_pop pop_adult_any_ethnicity
rename adult_a pop_adult_white_any_ethnicity
rename pop_a pop_total_white_any_ethnicity
rename adult_c pop_adult_AIAN_any_ethnicity
rename pop_c pop_total_AIAN_any_ethnicity
rename adult_de pop_adult_API_any_ethnicity
rename pop_de pop_total_API_any_ethnicity

rename adult_b_nh pop_adult_black_non_hispanic
rename pop_b_nh pop_total_black_non_hispanic

rename adult_a_nh pop_adult_white_non_hispanic
rename pop_a_nh pop_total_white_non_hispanic

rename adult_c_nh pop_adult_AIAN_non_hispanic
rename pop_c_nh pop_total_AIAN_non_hispanic

rename adult_de_nh pop_adult_API_non_hispanic
rename pop_de_nh pop_total_API_non_hispanic

rename adult_i pop_adult_hispanic_any_race
rename pop_i pop_total_hispanic_any_race

rename adult_17_b pop_adult_17_black_any_ethnicity
rename adult_17_b_nh pop_adult_17_black_non_hispanic
rename adult_17_a pop_adult_17_white_any_ethnicity
rename adult_17_a_nh pop_adult_17_white_non_hispanic
rename adult_17_c pop_adult_17_AIAN_any_ethnicity
rename adult_17_c_nh pop_adult_17_AIAN_non_hispanic
rename adult_17_de pop_adult_17_API_any_ethnicity
rename adult_17_de_nh pop_adult_17_API_non_hispanic
rename adult_17_i pop_adult_17_hispanic_any_race
rename adult_pop_17 pop_adult_17_any_ethnicity


export excel fips_state_county_code site_name year pop_total_any_ethnicity pop_adult_any_ethnicity pop_total_white_any_ethnicity pop_adult_white_any_ethnicity pop_total_black_any_ethnicity pop_adult_black_any_ethnicity pop_total_AIAN_any_ethnicity pop_adult_AIAN_any_ethnicity pop_total_API_any_ethnicity pop_adult_API_any_ethnicity pop_total_hispanic_any_race pop_adult_hispanic_any_race pop_total_white_non_hispanic pop_adult_white_non_hispanic pop_total_black_non_hispanic pop_adult_black_non_hispanic pop_total_AIAN_non_hispanic pop_adult_AIAN_non_hispanic pop_total_API_non_hispanic pop_adult_API_non_hispanic pop_adult_17_black_any_ethnicity pop_adult_17_black_non_hispanic pop_adult_17_white_any_ethnicity pop_adult_17_white_non_hispanic pop_adult_17_AIAN_any_ethnicity pop_adult_17_AIAN_non_hispanic pop_adult_17_API_any_ethnicity pop_adult_17_API_non_hispanic pop_adult_17_hispanic_any_race pop_adult_17_any_ethnicity using "$fileloc\cdc_pop_2010_2020.xlsx", firstrow(variables) replace

save "$fileloc\cdc_pop_2010-20_renamed_for_excel.dta", replace

********************************************************************************
//preps file for SJC only sites
use "$fileloc\cdc_pop_2010-20_renamed_for_excel.dta", clear
collapse (first) fips_state_county_code (sum) pop_adult_any_ethnicity-pop_adult_17_any_ethnicity, by(year site_name)

replace fips_state_county_code = "36061" if site_name == "NYC"

export excel fips_state_county_code site_name year pop_total_any_ethnicity pop_adult_any_ethnicity pop_total_white_any_ethnicity pop_adult_white_any_ethnicity pop_total_black_any_ethnicity pop_adult_black_any_ethnicity pop_total_AIAN_any_ethnicity pop_adult_AIAN_any_ethnicity pop_total_API_any_ethnicity pop_adult_API_any_ethnicity pop_total_hispanic_any_race pop_adult_hispanic_any_race pop_total_white_non_hispanic pop_adult_white_non_hispanic pop_total_black_non_hispanic pop_adult_black_non_hispanic pop_total_AIAN_non_hispanic pop_adult_AIAN_non_hispanic pop_total_API_non_hispanic pop_adult_API_non_hispanic pop_adult_17_black_any_ethnicity pop_adult_17_black_non_hispanic pop_adult_17_white_any_ethnicity pop_adult_17_white_non_hispanic pop_adult_17_AIAN_any_ethnicity pop_adult_17_AIAN_non_hispanic pop_adult_17_API_any_ethnicity pop_adult_17_API_non_hispanic pop_adult_17_hispanic_any_race pop_adult_17_any_ethnicity using "$fileloc\sjc_only_cdc_pop_2010_2020.xlsx" if site_name !="", firstrow(variables) replace