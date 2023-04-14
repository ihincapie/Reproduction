
*** 1. INTRODUCTION ***
/*
Description: 	Generates .tex file for Table 2 (Health-Seeking Behaviors)
Uses: 			marriedwoman16to49_allwaves_master.dta, child0to36months_allwaves_master.dta
Creates: 		table2.tex
*/

*** 2. SET ENVIRONMENT ***
version 14.1 		
set more off
set matsize 10000		
clear all			
pause on 			


//Prompt for directory if not filled in above
if "$PKH" == "" {
	display "Enter the directory where the PKH files can be located: " _request(PKH)
	* add a \ if they forgot one
	quietly if substr("$PKH",length("PKH"),1) != "/" & substr("$PKH",length("$PKH"),1) != "\" {
		global PKH = "$PKH\"	
	}
}

local PKH = "$PKH"
local pkhdata = "`PKH'data/raw/"
local output = "`PKH'output"
local latex = "`output'/latex"
local TARGET2 = "$TARGET2"

*** 3. IMPORT ADJUSTED P-VALUES FROM R-W OUTPUT ***
cd "`PKH'output"
//import excel document 
import excel using "rwolf_pvalues_table2.xls", firstrow clear
//loop over number of variables 
qui count 
local num_vars = `r(N)'
//loop for each variable
forv i = 1/`num_vars' {
	local var_name = A[`i']
	//loop for each column
	forv j = 1/2 {
		local `var_name'_p_`j': di %05.3f c`j'[`i'] // round to 3 decimal places
		//change 0 to <0.001
		if ``var_name'_p_`j'' == 0 {
			local `var_name'_p_`j' = "[" + "<0.001" + "]"
		}
		else {
			local `var_name'_p_`j' = "[" + "``var_name'_p_`j''" + "]"
		}
	}
}


*** 4. PREPARE FOR REGRESSION ***
cd "`PKH'data/coded"
use "marriedwoman16to49_allwaves_master.dta", clear
keep if iir01<40 & iir01>20

//drop *_miss variables related to food/non-food expenditure
drop logpcexp_food_baseline_miss logpcexp_nonfood_baseline_miss

//create local with all baseline characteristics (INCLUDES LOG PCE) 

local baseline_controls /// 
	  hh_head_agr_baseline_nm hh_head_serv_baseline_nm hh_educ*baseline_nm /// 
	  roof_type*baseline_nm wall_type*baseline_nm floor_type*baseline_nm clean_water_baseline_nm ///
	  own_latrine_baseline_nm square_latrine_baseline_nm own_septic_tank_baseline_nm /// 
	  electricity_PLN_baseline_nm hhsize_ln_baseline_nm logpcexp_baseline_nm *miss ///
	  

//generate indicator variables for kabupaten fixed effects
tab kabupaten, gen(kabu_)

//now also generate interactions with survey round variable for "stack and cluster" approach 
//to calculating 2-vs-6-year p-values
//generate dummy for survey round 
gen survey_round_1 = (survey_round == 1) if survey_round != 0
gen survey_round_2 = (survey_round == 2) if survey_round != 0

//baseline controls, endogenous regressor, and instrument 
foreach var of varlist `baseline_controls' {
	gen `var'1 = `var'*survey_round_1
	gen `var'2 = `var'*survey_round_2

	local baseline_controls_1 `baseline_controls_1' `var'1
	local baseline_controls_2 `baseline_controls_2' `var'2
}

//to be able to call in program for p-values 
global baseline_controls_1 `baseline_controls_1'
global baseline_controls_2 `baseline_controls_2'

//fixed effects
foreach var of varlist kabu_* {
	gen d`var'_1 = `var'*survey_round_1
	gen d`var'_2 = `var'*survey_round_2
}

//endogenous regressors (don't include in loop above so that they don't get added to baseline controls local)
foreach var of varlist pkh_by_this_wave L07 {
	gen `var'1 = `var'*survey_round_1
	gen `var'2 = `var'*survey_round_2
}


*** 5. PANEL A REGRESSIONS ***
cd "`output'"

preserve 
drop kabu_28
**Outcomes: Good assisted delivery, delivery at health facility, post-natal visits, 90+ iron pills
forvalues i = 1/1 {
	*local depvar: word `i' of "good_assisted_delivery" "delivery_facility" "post_natal_visits" "iron_pills_dummy"
	local depvar: word `i' of "good_assisted_delivery" 
	
	forvalues j = 1/2 {

		eststo model`i'`j': ivregress 2sls `depvar' kabu_* /// 
		(pkh_by_this_wave = L07) if survey_round == `j', vce(cluster kecamatan)

		//add RW adjusted p-value
		estadd local rwpval "``depvar'_p_`j''"

		//add control mean 
		summ `depvar' if L07 == 0 & survey_round == `j' & e(sample) == 1
		estadd scalar control_mean `r(mean)'
	}

	//Calculate 2-vs-6-year p-value! 
	eststo pval_26_`i': ivregress 2sls `depvar'  /// 
	dkabu_* (pkh_by_this_wave? = L07?), vce(cluster kecamatan)

	//test 2 vs 6 year 
	test pkh_by_this_wave1 = pkh_by_this_wave2
	estadd scalar pval26 `r(p)'


}

restore

forvalues i = 1/1 {
	local depvar: word `i' of "Delivery assisted by skilled midwife or doctor" "Delivery at health facility" "Number of post-natal visits" ///
							"90+ iron pills during pregnancy"
	
	#delimit ;

	esttab model`i'1 model`i'2 pval_26_`i'
		using "table2_robustness checks.tex", booktabs label se
		keep(pkh_by_this_wave)
		b(%12.3f)
		se(%12.3f)
		nostar
		varlab(pkh_by_this_wave "\\ `depvar'")
		scalars("rwpval \ " "control_mean \ " "pval26 \ ")
		nomtitles
		nogaps
		nonumbers
		noobs
		nonotes
		nolines
		fragment
		append;

		#delimit cr
}

#delimit cr



