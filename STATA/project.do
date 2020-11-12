*-----------------------------------------------------------------------------*
* Stats 506, F20 Project
* 
* Topic: Estimating linear or non-linear combinations of regression 
* coefficients (including standard errors).

* Author: Yan Chen yanchann@umich.edu
* Updated: Nov 13, 2020
*-----------------------------------------------------------------------------*
// 79: ---------------------------------------------------------------------- *

// load data: --------------------------------------------------------------- *
version 16.0

// delimited data: ---------------------------------------------------------- *
local base_url ///
https://raw.githubusercontent.com/aravind1338/506F20GroupProject/main

// read from web and then save
import delimited `base_url'/master.csv, clear
keep ïcountry year sex age suicides_no population gdp_for_year 

// rename variables 
rename (ïcountry suicides_no gdp_for_year) ///
	   (country count gdp)


// create dummy variable for age>20 and ohddests
generate byte gender = 1 if sex == "male"
replace gender = 0 if sex == "female"
label define gender_label 0 "Female" 1 "Male" , replace
label values gender gender_label

// conveert gdp to numeric
destring gdp, replace ignore(",")

// convert age from string to categorical
generate byte age_group=1 if age == "5-14 years"
replace age_group = 2 if age == "15-24 years"
replace age_group = 3 if age == "25-34 years"
replace age_group = 4 if age == "35-54 years"
replace age_group = 5 if age == "55-74 years"
replace age_group = 6 if age == "75+ years"
label define ag_label 1 "5-14 years" 2 "15-24 years" 3 "25-34 years" ///
					  4 "35-54 years" 5 "55-74 years" 6 "75+ years"
label values age_group ag_label

// drop columns
drop sex age

// save data
save cleaned.dta, replace
use cleaned, clear 

// EDA
tabstat count, by(gender) stats(mean sd n)
tabstat count, by(age_group) stats(mean sd n)

// Fit a Poisson model using age_group, year, gdp and gender
poisson count i.age_group i.gender i.year gdp, exposure(population), irr

// linear combinations of coefficients
// 25-34 males in 2015 vs 25-34 males 2005;  different year
// change the reference level to 25-34 and male
poisson count ib3.age_group ib1.gender i.year gdp, exposure(population), irr
lincom 2015.year - 2005.year , irr //

// 35-54 males in 2015 vs 35-54 females 2015; different gender
poisson count ib4.age_group i.gender ib2015.year gdp, exposure(population), irr
lincom 1.gender, irr //

// 75+ males in 2015 vs 15-24 males in 2015; different age
poisson count i.age_group ib1.gender ib2015.year gdp, exposure(population), irr
lincom 5.age_group - 2.age_group, irr // 


// non-linear combinations of coefficients
poisson count i.age_group i.gender i.year gdp, exposure(population)
nlcom (E_gender:exp(_b[1.gender]))







// 79: ---------------------------------------------------------------------- *
