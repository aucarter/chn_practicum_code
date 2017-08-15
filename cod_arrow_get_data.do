** prepare an arrow diagram with all-age rates and age-standardized rates

// prep stata
clear all
set more off
// set mem 40g
set maxvar 32000
pause on
if c(os) == "Unix" {
	global prefix "/home/j"
	set odbcmgr unixodbc
}
else if c(os) == "Windows" {
	global prefix "J:"
}

// Run "$prefix/WORK/10_gbd/00_library/functions/"
adopath++"/home/j/temp/central_comp/libraries/current/stata/"

// Set the connection settings
quiet run "$prefix/WORK/10_gbd/00_library/functions/create_connection_string.ado"
create_connection_string, server("modeling-mortality-db") database("shared") user("dbview") password("E3QNSLvQTRJm")
local conn_string = r(conn_string)

// DALYnator run
get_outputs, topic(cause) gbd_round_id(4) version("latest")
	
// Set locals
local metric = 1
local measure_id = 1
local year1 = 1990
local year2 = 2016
local age_group = 1
local change = 10001
local sex = 2
local sex_name = "females"
local rank = 20
local title "Leading `rank' level 3 causes of mortality rate (per 100,000 live births) for `sex_name', mainland China `year1' and `year2'"
local outdir "$prefix/temp/eeldren/mchs"
local codedir "/homes/eeldren/chn_practicum_code"

// Query most detailed causes
odbc load, exec("call shared.view_cause_hierarchy_history (shared.active_cause_set_version(3,3))") `conn_string' clear
keep cause_id acause most_detailed sort_order level yll_only yld_only cause_outline
rename (sort_order level cause_outline) (cause_sort cause_level cause)
tempfile detailed
save `detailed', replace

odbc load, exec("SELECT cause_id, cause_medium FROM shared.cause") `conn_string' clear
merge 1:1 cause_id using `detailed', keep(3) nogen
save `detailed', replace

// Getting provinces with which to aggregate up to get maindland China
get_location_metadata, location_set_id(22) clear
keep if parent_id==44533 | location_id==44533
keep location_id location_name
levelsof location_id, local(location)

// load results
// Order causes by the rank of means, not the mean rank calculated from the draws.  However, leave in this figure the UI calculated from the draws. This makes the figure more consistent with the viz tools
// Get mean ranks
get_outputs, topic(cause) measure_id(`measure_id') metric_id(`metric') year_id(`year1' `year2') cause_id(all) sex_id(`sex') age_group_id(`age_group') location_id(`location') rank_level(3) gbd_round_id(4) version("latest") clear

merge m:1 cause_id using `detailed', keep(1 3) nogen
drop if yld_only == 1
keep if cause_level == 3

// Summing provinces to create China mainland
bysort cause_id year_id: egen valsum = total(val)
bysort cause_id year_id: egen uppsum = total(upper)
bysort cause_id year_id: egen lowsum = total(lower)
keep if location_id==44533
replace val = valsum
replace upper = uppsum
replace lower = lowsum
drop valsum uppsum lowsum
drop if val == .

**merge 1:1 year_id cause_id using `mean', keep(2 3) nogen
tempfile mean
save `mean', replace

// Generating mortality rate per 100,000 live births
get_covariate_estimates, covariate_name_short("live_births_by_sex") location_id(44533) year_id(1990 2016) sex_id(3) clear
rename mean_value live_births
keep location_id year_id live_births

merge 1:m location_id year_id using `mean', keep(3) nogen

bysort cause_id year_id: gen val_mr = (val/live_births)*100000
replace val = val_mr
drop val_mr

	// Generate row_id to reflect cause ranks
	gsort year -val
	bysort year: gen row_id = _n

// Keep needed variables
keep year cause cause_medium row_id
replace cause = substr(cause,1,1)
gen mean_rank_`metric' = row_id
rename * *_

// Reshape data to match arrow diagram
preserve
	keep if row_id <= `rank'
	reshape wide cause_ cause_medium_ mean_rank_`metric'_ , i(row_id) j(year_)
	levelsof cause_medium_`year1', local (causes_`year1')
	levelsof cause_medium_`year2', local (causes_`year2')
	rename row_id row_id_`year1'
	gen row_id_`year2' = row_id_`year1'
	tempfile top
	save `top', replace
restore

gen resid = .
foreach cause in `causes_`year1'' `causes_`year2'' {
	replace resid = 1 if row_id > `rank' & cause_medium == "`cause'"
}
keep if !mi(resid)
bysort year: gen piv = _n
reshape wide cause_ cause_medium_ mean_rank_`metric'_ row_id_, i(piv) j(year_)
drop piv
tempfile resids
save `resids', replace
use `top', clear
append using `resids'
	
// Format
foreach var of varlist mean* {
	replace `var' = round(`var',.1)
	tostring `var', replace force format(%16.1fc)	
}
gen rank_`year1' = mean_rank_`metric'_`year1' if mean_rank_`metric'_`year1' != "."
gen rank_`year2' = mean_rank_`metric'_`year2' if mean_rank_`metric'_`year2' != "."

// Tempfile
keep rank* cause* row_id* resid
tempfile ranks
save `ranks', replace

// Rename variables so that formatting code doesn't need to be changed
foreach var of varlist _all {
	cap rename *_`year1' *_year1
	cap rename *_`year1'* *_year1*
	cap rename *_`year2' *_year2
	cap rename *_`year2'* *_year2*
}

	
// Export
export delimited using "arrowset_double_ages_98_99_GBD2016_`sex'", replace

// Run arrow code
!python "`codedir'/cod_arrow_make_figure.py" --outdir "`outdir'" --title "`title'" --daly_v "`daly_v'"  --year1 "`year1'" --year2 "`year2'"	--sex "`sex'"
