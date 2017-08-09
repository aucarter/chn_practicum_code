// Draft for stacked bar of global child mortality
// try one bar for all years > 1990 (use get_env_results) and also one every 5yrs (1990, 1995, 2000 ...)
// joe mikesell -- 04/14/2016
// - 04/27/2016 -- modify to go back to 1980 and also to not have any gaps between the bars 

// setup ----------------------------------------------------------------------
clear all
set more off
if c(os)=="Unix" global root "/home/j"
if c(os)=="Windows" global root "J:"

// use "J:\WORK\02_mortality\04_outputs\02_results\envelope.dta"

run "$root/Usable/Tools/ADO/pdfmaker_Acrobat11.do"
run "$root/WORK/10_gbd/00_library/functions/get_outputs.ado" 

get_outputs, topic(cause) measure_id(1) location_id(1) age_group_id(2 3 4 5) year_id(all) clear

keep year_id age_group_id sex val

// reshape to get data wide on age_group_id and long on year_id
rename val val_
reshape wide val_, i(year_id) j(age_group_id)
// replace val_* = val_* / 1000000

// graph with 5yr bars --------------------------------------------------------

pdfstart using "$root/WORK/10_gbd/04_journals/gbd2015_capstone_lancet_child/03_temp/global_under5_mort_bar_5yrs.pdf"
graph bar (asis) val_2 val_3 val_4 val_5, over(year_id) stack ///
	legend(label(1 "Early neonatal") label(2 "Late neonatal") label(3 "Postneonatal") label(4 "1 to 4 years")) ///
	title("Global under-five deaths by age-group and year", size(medium)) ytitle("Deaths") ylabel(5000000(5000000)15000000, format(%10.0fc)) ///
	graphregion(color(white))
	pdfappend
pdffinish // append for just one?

// graph with all years -------------------------------------------------------

clear all
global root "J:"
run "$root/Project/Mortality/shared/functions/get_env_results.ado"
run "$root/Usable/Tools/ADO/pdfmaker_Acrobat11.do"

get_env_results
keep if sex_id == 3 & year_id >= 1980 & inlist(age_group_id, 2, 3, 4, 5) & location_id == 1
keep year_id age_group_id mean_env_whiv
rename mean_env_whiv val_ 

reshape wide val_, i(year_id) j(age_group_id)

pdfstart using "$root/WORK/10_gbd/04_journals/gbd2015_capstone_lancet_child/03_temp/global_u5_mort_stackbar_1yr_1980_4.pdf"
graph bar (asis) val_2 val_3 val_4 val_5, over(year_id, gap(0) label(angle(45) labsize(vsmall))) stack ///
	legend(label(1 "Early neonatal") label(2 "Late neonatal") label(3 "Postneonatal") label(4 "1 to 4 years") tstyle(smbody)) ///
	title("Global under-five deaths by age-group and year", size(small)) ytitle("Deaths", size(small)) ylabel(5000000(5000000)15000000, format(%10.0fc) labsize(vsmall)) ///
	graphregion(color(white))
	pdfappend
pdffinish, view