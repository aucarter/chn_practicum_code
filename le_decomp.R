################################################################################
## Purpose: Decompose province-level life-expetancy into birth defects and other
## Date created: 07/30/2017
## Date modified:
## Author: Austin Carter, aucarter@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/HIV/")

## Packages
library(data.table)

## Arguments
# args <- commandArgs(trailingOnly = TRUE)
# if(length(args) > 0) {

# } else {

# }

### Paths
lt.path <- "/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/compiled_summary_lt.csv"


### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_model_results.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source(paste0(root,"/Project/Mortality/shared/functions/get_age_map.r"))

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
age.table <- data.table(get_age_map(type = "all"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)

### Code
# Get Chinese provinces including Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)

# Life-tables for provinces for each sex in years 1990 and 2016
lt.dt <- fread(lt.path)[location_id %in% prov.list & year %in% c(1990, 2016)]
setnames(lt.dt, "year", "year_id")

# Handle the older age groups that aren't reported elsewhere

ages <- c(setdiff(unique(lt.dt$age_group_id), c(33, 44, 45, 148)), 235)


# cause.ages <- unique(cause.dt$age_group_id)
# age.table[age_group_id %in% cause.ages, age_group_name_short]


# model.dt <- get_model_results(gbd_team = "cod", gbd_id = c.cause, location_id = prov.list, year_id = c(1990, 2016),
# 					  age_group_id = ages, measure_id = 1)
# # Get draws of birth defects and population to make rates
c.cause <- meta[cause_name == "Congenital birth defects", cause_id]
cause.dt <- get_draws(gbd_id_field = "cause_id", gbd_id = c.cause, location_ids = prov.list, year_id = c(1990, 2016),
					  age_group_id = ages, source = "codcorrect", measure_ids = 1)
pop.dt <- get_population(location_id = prov.list, year_id = c(1990, 2016), age_group_id = ages, sex_id = -1)
rate.dt <- merge(cause.dt, pop.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
for(i in 0:999) {
	print(i)
	rate.dt[, (paste0("draw_", i)) := get(paste0("draw_", i)) / population]
}

# Merge and prep mortality proportions
merge.dt <- merge(lt.dt, rate.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
for(i in 0:999) {
	print(i)
	merge.dt[, (paste0("prop_", i)) := get(paste0("draw_", i)) / mx]
}

# Draws of mortality proportions long
melt.dt <- melt(merge.dt, id.vars = c("year_id", "sex_id", "age_group_id", "location_id"), 
			   measure.vars = paste0("prop_", 0:999), value.name = "mx_prop")
melt.dt[, draw := as.integer(gsub("prop_", "", variable))]
melt.dt[, variable := NULL]

# Merge on proportions an calculate cause deleted px
lt <- merge(lt.dt, melt.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
lt[, pxdel := px ^ (1 - mx_prop)]
lt[age_group_id == max(ages), pxdel := 0]

# Set first age group lx to 1 and calculate resulting lx
lt[age_group_id == ages[1], lxdel := lx]
for(i in 2:length(ages)) {
	age <- ages[i]
	prior.age <- ages[i - 1]
	prior.lx <- lt[age_group_id == prior.age, lxdel]
	px <- lt[age_group_id == age, pxdel]
	temp.lx <- prior.lx * px
	lt[age_group_id == age, lxdel := temp.lx]
}

# dx





### End