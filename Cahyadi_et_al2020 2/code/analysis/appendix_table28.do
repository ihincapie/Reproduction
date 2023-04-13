
*** 2. SET ENVIRONMENT ***
version 14.1 
set more off		
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


*** PANEL A REGRESSIONS ***
cd "`PKH'data/coded"
use "child6to15_fertility_marriage_outcomes.dta", clear

//drop *_miss variables related to food/non-food expenditure
drop logpcexp_food_baseline_miss logpcexp_nonfood_baseline_miss

//create local with all baseline characteristics (INCLUDES LOG PCE)
local baseline_controls /// 
	  hh_head_agr_baseline_nm hh_head_serv_baseline_nm hh_educ*baseline_nm /// 
	  roof_type*baseline_nm wall_type*baseline_nm floor_type*baseline_nm clean_water_baseline_nm ///
	  own_latrine_baseline_nm square_latrine_baseline_nm own_septic_tank_baseline_nm /// 
	  electricity_PLN_baseline_nm hhsize_ln_baseline_nm logpcexp_baseline_nm *miss

//generate interaction variables
foreach x of varlist `baseline_controls' {
	generate `x'_i = `x'*L07
}
local interactions *_i

//generate indicator variables for kabupaten fixed effects
tab kabupaten, gen(kabu_)

//generate dummy version of PKH variable to be able to omit certain output 
gen pkh_by_this_wave_new = pkh_by_this_wave

*** 4. PANEL A REGRESSIONS ***
cd "`output'"

**Outcome: Fertility 16-17
//Midline, Lottery Only 
eststo model_a: ivregress 2sls has_child_in_hh_16to17 `baseline_controls' kabu_* /// 
	(pkh_by_this_wave = L07) if survey_round == 1, vce(cluster kecamatan)

	//add control mean 
	summ has_child_in_hh_16to17 if L07 == 0 & survey_round == 1 & e(sample) == 1
	estadd scalar control_mean `r(mean)'


//Endline, Lottery Only 
eststo model_b: ivregress 2sls has_child_in_hh_16to17 `baseline_controls' kabu_* /// 
	(pkh_by_this_wave = L07) if survey_round == 2, vce(cluster kecamatan)

	//add control mean 
	summ has_child_in_hh_16to17 if L07 == 0 & survey_round == 2 & e(sample) == 1
	estadd scalar control_mean `r(mean)'


**Outcome: Fertility 18-21
//Midline, Lottery Only 
eststo model_c: ivregress 2sls has_child_in_hh_18to21 `baseline_controls' kabu_* /// 
	(pkh_by_this_wave_new = L07) if survey_round == 2, vce(cluster kecamatan)

//Endline, Lottery Only 
eststo model_d: ivregress 2sls has_child_in_hh_18to21 `baseline_controls' kabu_* /// 
	(pkh_by_this_wave = L07) if survey_round == 2, vce(cluster kecamatan)

	//add control mean 
	summ has_child_in_hh_18to21 if L07 == 0 & survey_round == 2 & e(sample) == 1
	estadd scalar control_mean `r(mean)'



*** 5. OUTPUT TO LATEX ***
cd "`latex'"

//16-17 outcome
#delimit ;
esttab model_a model_b
	using "appendix_table28.tex", booktabs label se
	keep(pkh_by_this_wave)
	b(%12.3f)
	se(%12.3f)
	nostar
	mlabels("2-Year" "6-Year", lhs("Outcome:"))
	varlab(pkh_by_this_wave "\midrule Has Child in Roster (Ages 16-17)")
	stats(control_mean, labels(" ") fmt("%12.3f"))
	nogaps
	noobs
	nonotes
	nolines
	fragment
	replace;

#delimit cr 

//18-21 outcome
#delimit ;
esttab model_c model_d
	using "appendix_table28.tex", booktabs label se
	keep(pkh_by_this_wave)
	b(%12.3f)
	se(%12.3f)
	nostar
	varlab(pkh_by_this_wave "\\ Has Child in Roster (Ages 18-21)")
	stats(control_mean, labels(" ") fmt("%12.3f"))
	nomtitles
	nogaps
	nonumbers
	noobs
	nonotes
	nolines
	fragment
	append;

#delimit cr
