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

	//run "$prefix/WORK/10_gbd/00_library/functions/"
	adopath++"/home/j/temp/central_comp/libraries/current/stata/"
	
// set the connection settings
	quiet run "$prefix/WORK/10_gbd/00_library/functions/create_connection_string.ado"
	create_connection_string, server("modeling-mortality-db") database("shared") user("dbview") password("E3QNSLvQTRJm")
	local conn_string = r(conn_string)

// DALYnator run
	get_outputs, topic(cause) gbd_round_id(4) version("latest")
	// keep if compare_version_status == "Current best" & regexm(gbd_process_version_note, "2013 backfill") != 1
	// duplicates drop compare_version_id, force
	// local daly_v = compare_version_id	
	
// Set locals
	local metric = 1
	local measure_id = 1
	local year1 = 1990
	local year2 = 2016
	local age_group = 1
	local change = 10001
	// WILL NEED TO EXCLUDE HK & MACAU
	**local location = 6
	local sex = 3
	local rank = 20
	local title "Leading `rank' level 3 causes of mortality rate (per 100,000 live births) for both sexes, China `year1' and `year2'"
	local outdir "$prefix/temp/eeldren/mchs"
	local codedir "/homes/eeldren/chn_practicum_code"

// query most detailed causes
odbc load, exec("call shared.view_cause_hierarchy_history (shared.active_cause_set_version(3,3))") `conn_string' clear
keep cause_id acause most_detailed sort_order level yll_only yld_only cause_outline
rename (sort_order level cause_outline) (cause_sort cause_level cause)
tempfile detailed
save `detailed', replace

odbc load, exec("SELECT cause_id, cause_medium FROM shared.cause") `conn_string' clear
merge 1:1 cause_id using `detailed', keep(3) nogen
save `detailed', replace

// load results
// order causes by the rank of means, not the mean rank calculated from the draws.  However, leave in this figure the UI calculated from the draws. This makes the figure more consistent with the viz tools
// get mean ranks
get_outputs, topic(cause) measure_id(`measure_id') metric_id(`metric') year_id(`year1' `year2') cause_id(all) sex_id(`sex') age_group_id(`age_group') location_id(`location') gbd_round_id(4) version("latest") clear

	merge m:1 cause_id using `detailed', keep(1 3) nogen
	drop if yld_only == 1
	keep if cause_level == 3
	drop if val == .
	
// reshape data to match arrow diagram
	keep year val cause_id cause cause_medium upper lower
	gsort year -val
	bysort year: gen row_id = _n
	drop val
	tempfile mean
	save `mean', replace
	
	** bring in ranks from draws to get uncertainty
	//get_outputs, topic(cause) measure_id(`measure_id') rank year_id(`year1' `year2') cause_id(all) sex_id(`sex') age_group_id(22) location_id(`location') rank_level(3) gbd_round(2016) clear
	get_outputs, topic(cause) measure_id(`measure_id') metric_id(`metric') year_id(`year1' `year2') cause_id(all) sex_id(`sex') age_group_id(`age_group') location_id(`location') rank_level(3) gbd_round_id(4) version("latest") clear
	// TRYING TO ADD MANUAL RANK CODE IN STATA
	// Sort (by value, descending) within each group of rows
		// which are grouped by having the same values in columns ending with "id" other than cause_id
	/*
	gsort age_group_id location_id metric_id year_id measure_id sex_id -val, generate(row_id)
	gsort year_id age_group_id location_id metric_id measure_id sex_id -val
	bysort year_id age_group_id location_id metric_id measure_id sex_id
	
	order age_group_id location_id metric_id year_id measure_id sex_id val
	gen negupp = -upper
	gen neglow = -lower
	drop upper lower
	bysort age_group_id location_id metric_id year_id measure_id sex_id (negupp): gen upper = _n
	bysort age_group_id location_id metric_id year_id measure_id sex_id (neglow): gen lower = _n
	drop neg*
	*/

	merge 1:1 year_id cause_id using `mean', keep(2 3) nogen


	** keep needed variables
	keep year cause cause_medium row_id
	replace cause = substr(cause,1,1)
	gen mean_rank_`metric' = row_id
	rename * *_
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
	by year: gen piv = _n
	reshape wide cause_ cause_medium_ mean_rank_`metric'_ row_id_, i(piv) j(year_)
	drop piv
	tempfile resids
	save `resids', replace
	use `top', clear
	append using `resids'
	
// format
	foreach var of varlist mean* {
		replace `var' = round(`var',.1)
		tostring `var', replace force format(%16.1fc)	
	}
	gen rank_`year1' = mean_rank_`metric'_`year1' if mean_rank_`metric'_`year1' != "."
	gen rank_`year2' = mean_rank_`metric'_`year2' if mean_rank_`metric'_`year2' != "."
	
// tempfile
	keep rank* cause* row_id* resid
	tempfile ranks
	save `ranks', replace
	
// get age stdr percent change
	/*
	get_outputs, topic(cause) measure_id(`measure_id') metric_id(all) location_id(`location') year_start_id(`year1') year_end_id(`year2') age_group_id(22 27) sex_id(`sex') cause_id(all) version("latest") clear	
	
	merge m:1 cause_id using `detailed', keep(1 3) nogen
	drop if yld_only == 1
	keep if cause_level == 3
	
	keep if metric_name == "Number"
	rename (val upper lower age_group_id) (mean_abs_`metric' upper_abs_`metric' lower_abs_`metric'  age)
	gen year = `change'
	replace age = 98 if age == 27
	replace age = 99 if age == 22
	
	keep cause_medium *abs* year age
	rename (mean* upper* lower*) (mean*_ upper*_ lower*_)
	unab reshape_list: mean* upper* lower*
	reshape wide `reshape_list', i(cause_medium age) j(year)
	rename (mean* upper* lower*) (mean*_ upper*_ lower*_)
	unab reshape_list: mean* upper* lower*
	reshape wide `reshape_list', i(cause_medium) j(age)
	rename (*`change'_*) (*`year1'_`year2'_*)
	rename (mean_abs_`metric'_`year1'_`year2'_* upper_abs_`metric'_`year1'_`year2'_* lower_abs_`metric'_`year1'_`year2'_*) (mean_`year1'_`year2'_* upper_`year1'_`year2'_* lower_`year1'_`year2'_*)
	
	foreach var of varlist mean* upper* lower* {
		replace `var' = round(`var'*100)
		tostring `var', replace force format(%16.0fc)
	}
	gen change_`year1'_`year2'_99 = mean_`year1'_`year2'_99 + "%" + " (" + lower_`year1'_`year2'_99 + " to " + upper_`year1'_`year2'_99 + "%)"
	gen change_`year1'_`year2'_98 = mean_`year1'_`year2'_98 + "%" + " (" + lower_`year1'_`year2'_98 + " to " + upper_`year1'_`year2'_98 + "%)"
	drop mean* upper* lower*
	gen cause_medium_`year2' = cause_medium
	tempfile one
	save `one', replace

// merge
	merge 1:m cause_medium_`year2' using `ranks', nogen keep(2 3)
	order rank_`year1' cause_medium_`year1' cause_medium_`year2' rank_`year2' change*
	gsort row_id_`year2'
*/
// rename variables so that formatting code doesn't need to be changed
foreach var of varlist _all {
	cap rename *_`year1' *_year1
	cap rename *_`year1'* *_year1*
	cap rename *_`year2' *_year2
	cap rename *_`year2'* *_year2*
}

	
// export
	export delimited using "arrowset_double_ages_98_99_GBD2016", replace

// run arrow code
	!python "`codedir'/cod_arrow_make_figure.py" --outdir "`outdir'" --title "`title'" --daly_v "`daly_v'"  --year1 "`year1'" --year2 "`year2'"	
