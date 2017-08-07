################################################################################
## Purpose: Pull results for China under 5 risks paper, then generate figures
## Date created: 08/02/2017
## Date modified:
## Author: Will Godwin, wgodwin@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/risk_factors/")

## Packages
library(data.table)
library(ggplot2)
library(RMySQL)
library(maptools)
library(gridExtra)
library(ggrepel)

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_model_results.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "/Project/Mortality/shared/functions/get_age_map.r"))
source(paste0(root, "WORK/05_risk/central/code/maps/global_map.R"))


### Tables
loc.table <- get_location_metadata(location_set_id = 22)
age.table <- data.table(get_age_map(type = "all"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)
sdi.table <- get_covariate_estimates(covariate_name_short = "sdi")
setnames(sdi.table, "mean_value", "sdi")

### Code
# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)

############################################
###################### SEVs ################
############################################

# Get outputs of SEV's for risks of interest (WaSH, malnutrition, HAP, breastfeeding)
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 339, 100, 96, 97)
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = c(1990,2016), measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", sex_id = 3)
setnames(sev.dt, "val", "sev")
sev.dt <- merge(sev.dt, sdi.table, by = c("location_id", "year_id"))

## Loop through risks and scatter the sdi vs sev
scatter.path <- paste0(root, "temp/wgodwin/chn/sev_sdi_scatter.pdf")
pdf(scatter.path)
for(risk in risk_ids) {
	for(year in c(1990,2016)) {
		risk.dt <- sev.dt[rei_id == risk & year_id == year]
		rei_name <- unique(risk.dt$rei_name)
		gg <- ggplot(risk.dt, 
					aes(x = sdi, 
						y = sev)) + 
			geom_point() +
	    	ggtitle(paste0("SEV vs SDI: ", rei_name, " in ", year)) +
	    	geom_text_repel(aes(label = location_name.x)) +
	    	labs(x = "Socio-Demographic Index", 
	    		y = "Summary Exposure Value")
	    print(gg)
	    print(paste(rei_name, year))
	}
}
dev.off()

# Get outputs of SEV's for top 6 risks
risk_ids <- c(86, 87, 335, 334, 100, 240, 136)
years <- seq(1990, 2016)
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = years, measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", sex_id = c(1, 2, 3))
setnames(sev.dt, "val", "sev")
sexes <- unique(sev.dt$sex_id)

# loop through locations and scatter sev's over time
scatter.path <- paste0(root, "temp/wgodwin/chn/sev_time_scatter.pdf")
pdf(scatter.path)
legend_title <- "Risk Name"
for(loc in prov.list) {
	for(sx in sexes) {
		temp.dt <- sev.dt[location_id == loc & sex_id == sx,]
		loc_name <- unique(temp.dt$location_name)
		sex_name <- unique(temp.dt$sex)
		gg <- ggplot(temp.dt, 
			aes(x = year_id, 
				y = sev, 
				color = rei_name,
				group = rei_name)) +
			geom_line(data = temp.dt[!is.na(temp.dt$sev),]) + 
			geom_point() + 
			ggtitle(paste0("Summary Exposure Value-1990 to 2016 in ", loc_name, "-", sex_name)) +
		    	labs(x = "Year", 
		    		y = "Summary Exposure Value (proportion exposed)") +
			scale_color_discrete(legend_title)
			#facet_wrap(~location_name, ncol = 2, nrow = 2)
		print(gg)
		print(paste(loc, sex_name))
	}
}
dev.off()

#################################################
##### Cause-specific Attributable mortality #####
#################################################

# Get outputs of mortality rate due to risks of interest
risk_ids <- c(86, 87, 339, 100, 240, 136)
years <- seq(1990, 2016)
mort.dt <-	get_outputs(topic = "rei", location_id = prov.list, year_id = years, measure_id = 1, rei_id = risk_ids, metric_id = 3, 
			age_group_id = 1, version = "latest", sex_id = 3)
mort.dt[, rate := val * 100000]

## Loop through risks and scatter mort rate across time for both sexes
scatter.path <- paste0(root, "temp/wgodwin/chn/mort_scatter_prov.pdf")
pdf(scatter.path)
legend_title <- "Risk Name"
for(loc in prov.list) {
	temp.dt <- mort.dt[location_id == loc]
	loc_name <- unique(temp.dt$location_name)
	gg <- ggplot(temp.dt, 
		aes(x = year_id, 
			y = rate, 
			color = rei_name,
			group = rei_name)) +
		geom_line(data = temp.dt[!is.na(temp.dt$rate),]) + 
		geom_point() + 
		ggtitle(paste0("All-cause mortality by Risk factor in ", loc_name)) +
		labs(x = "Year", 
			 y = "Attributable Mortality Rate (per 100,000)") +
		scale_color_discrete(legend_title)
		#facet_wrap(~location_name, ncol = 2, nrow = 2)
	print(gg)
	print(paste(loc))
}
dev.off()

################################################
########## All-cause mortality burden ##########
################################################

#Death rate attributed to all risk factors
all.mr <- get_outputs(topic = "rei", location_id = prov.list, year_id = c(1990, 1995, 2000, 2005, 2010, 2016), measure_id = 1, metric_id = 3, 
	age_group_id = 1, version = "latest")
all.mr[, rate := val * 100000]
all.mr <- merge(all.mr, loc.table, by="location_id")


