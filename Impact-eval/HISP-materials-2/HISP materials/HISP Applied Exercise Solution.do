/* Impact Evaluation in Practice 
HISP Case Study
Applied Exercises */

clear
clear matrix
set more off

*========================*;

log using "solution_log.txt", replace t
*Open the cleaned data set
use "evaluation.dta" 
*============================*
*Macros
*============================*
global variables1 health_expenditures age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance
global controls  age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance
global controls1 age_hh age_sp 
global controls2 hhsize dirtfloor bathroom land hospital_distance

*============================*
*Start solution
*============================*
describe

*Case 1: Before and After 
/*Q1*/ ttest health_expenditures if eligible ==1 & treatment_locality ==1, by(round) 

*----------------------------------------------------*
*Case 2: Self Selected Treatments

*Q1
ci  

*Q2
foreach x of global controls {
	describe `x'
	ttest `x' if round ==1 & treatment_locality ==1, by(enrolled_ro)
	}
ttest health_expenditures if round ==1 & treatment_locality ==1, by(enrolled_ro)

*Q3
reg health_expenditures enrolled_ro if round ==1 & treatment_locality ==1, cl( locality_identifier)
reg health_expenditures enrolled_ro $controls1 if round ==1 & treatment_locality ==1, cl( locality_identifier)

*----------------------------------------------------*
*Case 3: Randomized Assignment

*Q1 
ttest health_expenditures if round ==0 & eligible ==1, by(treatment_locality)
foreach x of global controls {
	describe `x'
	ttest `x' if round ==0 & eligible ==1, by(treatment_locality)
	}

*Q2 
ttest health_expenditures if eligible ==1 & round ==1, by(treatment_locality)

*Q3
reg health_expenditures treatment_locality if eligible ==1 & round ==1, cl(locality_identifier)
reg health_expenditures treatment_locality $controls1 if eligible ==1 & round ==1, cl(locality_identifier)
reg health_expenditures treatment_locality $controls if eligible ==1 & round ==1, cl(locality_identifier)

*Q6
reg health_expenditures treatment_locality if eligible ==0 & round ==1, cl(locality_identifier)

*----------------------------------------------------*
*Case 4: Instrumental Variables

*Q1
ivreg health_expenditures (enrolled_rp = promotion_locality) if round ==1, first
ivreg health_expenditures (enrolled_rp = promotion_locality) $controls1 if round ==1
ivreg health_expenditures (enrolled_rp = promotion_locality) $controls if round ==1

*----------------------------------------------------*
*Case 5: RDD 

*Normalize the poverty index at 0 and create left and right side variables
gen poverty_index_left=poverty_index-58 if poverty_index<=58 
	replace poverty_index_left=0 if poverty_index>58
gen poverty_index_right=poverty_index-58 if poverty_index>58 
	replace poverty_index_right=0 if poverty_index<=58

*Q1 
reg health_expenditures poverty_index_left poverty_index_right eligible if round ==1 & treatment_locality ==1

*Q2
predict he_pred1
graph7 he_pred1 poverty_index if round ==1 & treatment_locality ==1

*Q3
reg health_expenditures eligible poverty_index_left poverty_index_right if round ==1 & treatment_locality ==1

*Q4 
gen poverty_index_left2=poverty_index_left^2
gen poverty_index_right2=poverty_index_right^2
gen poverty_index_left3=poverty_index_left^3
gen poverty_index_right3=poverty_index_right^3
gen poverty_index_left4=poverty_index_left^4
gen poverty_index_right4=poverty_index_right^4

reg health_expenditures eligible poverty_index_left poverty_index_right poverty_index_left2 poverty_index_right2 poverty_index_left3 poverty_index_right3 poverty_index_left4 poverty_index_right4 if round ==1 & treatment_locality ==1

*Q5
predict he_pred2 
graph7 he_pred2  poverty_index if round ==1 & treatment_locality ==1

*Q6
reg health_expenditures eligible poverty_index if round ==0 & treatment_locality ==1
predict he_pred3
graph7 he_pred3 poverty_index if round ==0 & treatment_locality ==1

*----------------------------------------------------*
*Case 6: Dif in Dif   

*Q1 
sort household_identifier round
gen delta_he=health_expenditures-health_expenditures[_n+1] if household_identifier==household_identifier[_n+1] 

/*Q2*/ ttest delta_he if treatment_locality==1, by(enrolled_ro)

*Q3 
/*Create the DD variable*/ gen enrolled_round = enrolled_ro*round

reg health_expenditures enrolled_round round enrolled_ro if treatment_locality ==1, cl(locality_identifier)
reg health_expenditures enrolled_round round enrolled_ro $controls1 if treatment_locality ==1, cl(locality_identifier)
reg health_expenditures enrolled_round round enrolled_ro $controls if treatment_locality ==1, cl(locality_identifier)

*----------------------------------------------------*
*Case 7: Matching 

*Q1
logit enrolled_ro $controls if round ==0
predict pscore

*Q2 
xtile quintiles = pscore, nq(5)

*Q3
sort quintiles
by quintile: ttest health_expenditures if round ==1 & treatment_locality ==1, by(enrolled_ro)

foreach x of global controls {
	describe `x'
	by quintile: ttest `x' if round ==1 & treatment_locality ==1, by (enrolled_ro)
	}

*Q4
reg health_expenditures enrolled_ro if round ==1 & treatment_locality ==1 & quintile>1 & quintile<5, cl( locality_identifier)

gen age_hh2=age_hh^2
gen age_hh3=age_hh^3
gen educ_hh2=educ_hh^2
gen educ_hh3=educ_hh^3
gen hhsize2=hhsize^2
gen hhsize3=hhsize^3

reg health_expenditures enrolled_ro pscore age_hh2 age_hh3 educ_hh2 educ_hh3 hhsize2 hhsize3 if round ==1 & treatment_locality ==1 & quintile>1 & quintile<5, cl( locality_identifier)
reg health_expenditures enrolled_ro $controls if round ==1 & treatment_locality ==1 & quintile>1 & quintile<5, cl( locality_identifier)

*Q7
psmatch2 enrolled_ro $controls if round ==1 & treatment_locality ==1, out(health_expenditures) /*Note: You can install psmatch2 with the command 'ssc install psmatch2, replace'*/

*----------------------------------------------------*
*Case 8: Power Calculations 

*Note: Focus on the randomized assignment case 
drop if eligible==0


*Q1
sum health_expenditures if round==1 & treatment_locality==1 
	local m1 = `r(mean)' /*This saves the mean which will be used as m1 in power calculations below*/
	local sd = `r(sd)' /*This saves the standard deviation which will be used as sd1 and sd2 in power calclulations below*/

sum hospital if round==1 & treatment_locality==1
	local m1_h = `r(mean)'
	local sd_h = `r(sd)'

*Q2
*Compute mean2 wih the minimum detectable effect
local mde_1 = `m1'-1
local mde_2 = `m1'-2
local mde_3 = `m1'-3

sampsi `m1' `mde_1', p(0.9) r(1) sd1(`sd') sd2(`sd') /*Note: Use the command sampncti when sample sizes are small*/
sampsi `m1' `mde_2', p(0.9) r(1) sd1(`sd') sd2(`sd')
sampsi `m1' `mde_3', p(0.9) r(1) sd1(`sd') sd2(`sd')

sampsi `m1' `mde_1', p(0.8) r(1) sd1(`sd') sd2(`sd')
sampsi `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
sampsi `m1' `mde_3', p(0.8) r(1) sd1(`sd') sd2(`sd')

*Q3
*Compute mean2 wih the minimum detectable effects for hospitalization rates (in %)
local mde_h_1 = `m1_h'+0.01
local mde_h_2 = `m1_h'+0.02
local mde_h_3 = `m1_h'+0.03

sampsi `m1_h' `mde_h_1', p(0.9) r(1) sd1(`sd_h') sd2(`sd_h')
sampsi `m1_h' `mde_h_2', p(0.9) r(1) sd1(`sd_h') sd2(`sd_h')
sampsi `m1_h' `mde_h_3', p(0.9) r(1) sd1(`sd_h') sd2(`sd_h')

sampsi `m1_h' `mde_h_1', p(0.8) r(1) sd1(`sd_h') sd2(`sd_h')
sampsi `m1_h' `mde_h_2', p(0.8) r(1) sd1(`sd_h') sd2(`sd_h')
sampsi `m1_h' `mde_h_3', p(0.8) r(1) sd1(`sd_h') sd2(`sd_h')

*Q5
iclassr health_expenditures locality_identifier if round==1 & treatment_locality==1, noisily /*This gives you the intra-cluster correlation, or rho*/
	local rho = $S_1 /*This saves the intra-cluster correlation, or rho, which will be used in clustered power calculations below*/
	display `rho'
	
*Q6
sampsi `m1' `mde_1', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(100) rho(`rho') /*This corrects for clusters, Note: You may need to install the sampclus command*/
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(100) rho(`rho') 
sampsi  `m1' `mde_3', p(0.8) r(1) sd1(`sd') sd2(`sd')
 	sampclus, numclus(100) rho(`rho') 


*Q7
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(30) rho(`rho')
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(58) rho(`rho')
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(81) rho(`rho')
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(90) rho(`rho')
sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(120) rho(`rho')


*============================*

*End


log close
