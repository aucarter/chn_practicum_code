################################################################################
## Purpose: Combine all birth defects output that is location-specific
## Date created: 08/08/2017
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
library(data.table); library(parallel)

## Arguments
# args <- commandArgs(trailingOnly = TRUE)
# if(length(args) > 0) {

# } else {

# }
ncores <- 2

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))

### Paths
table.dir <- paste0(root, "temp/aucarter/le_decomp/tables/")
e0.table.dir <- paste0(table.dir, "e0/")
decomp.table.dir <- paste0(table.dir, "decomp/")
deleted.table.dir <- paste0(table.dir, "deleted")
daly.decomp.table.dir <- paste0(table.dir, "daly_decomp/")

### Tables
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)

### Code
# Combine life expectancy files
e0.files <- list.files(e0.table.dir)
e0.dt <- rbindlist(mclapply(e0.files, function(file) {
	dt <- fread(paste0(e0.table.dir, file))
}, mc.cores = ncores))
e0.dt <- e0.dt[order(year, sex, location_name)]
write.csv(e0.dt, paste0(table.dir, "e0 (Table 1).csv"), row.names = F)

# Combine life expectancy decomp files
cause.list <- meta[level %in% 1:3, cause_id]
decomp.dt <- data.table()
for(cause in cause.list) {
	dir <- paste0(decomp.table.dir, cause, "/")
	cause.name <- meta[cause_id == cause, cause_name]
	cause.level <- meta[cause_id == cause, level]
	decomp.files <- list.files(dir)
	if(length(decomp.files) == 0) {
		print(cause.name)
		next
	}
	temp.dt <- rbindlist(mclapply(decomp.files, function(file) {
		dt <- fread(paste0(dir, file))
	}, mc.cores = ncores))
	temp.dt[, cause_id := cause][, cause_name := cause.name][, level := cause.level]
	decomp.dt <- rbind(decomp.dt, temp.dt)
}
decomp.dt <- decomp.dt[order(sex, location_name)]
write.csv(decomp.dt, paste0(table.dir, "decomp (Table 3).csv"), row.names = F)

# Combine cause-deleted life expectancy files
deleted.files <- list.files(deleted.table.dir)
deleted.dt <- rbindlist(mclapply(deleted.files, function(file) {
	dt <- fread(paste0(deleted.table.dir, "/", file))
}, mc.cores = ncores))
deleted.dt <- deleted.dt[order(year, sex, location_name)]
write.csv(deleted.dt, paste0(table.dir, "deleted (Table 4).csv"), row.names = F)

# Combine single age cause-deleted life expectancy files
age.group.table <- data.table(age_group_id = c(28, 5, 6, 7), age_group = c("0-1", "1-4", "5-9", "10-14"))

combined.dt <- data.table()
for(del.age in c(28, 5, 6, 7)) {
	age.group <- age.group.table[age_group_id == del.age, age_group]
	age.table.dir <- paste0(table.dir, del.age, "_deleted")
	deleted.files <- list.files(age.table.dir)
	deleted.dt <- rbindlist(mclapply(deleted.files, function(file) {
		dt <- fread(paste0(table.dir, del.age, "_deleted/", file))
	}, mc.cores = ncores))
	deleted.dt[, age_group := age.group]
	deleted.dt <- deleted.dt[order(year, sex, age_group, location_name), .(year, sex, age_group, location_name, diff_mean, diff_lower, diff_upper)]
	combined.dt <- rbind(combined.dt, deleted.dt)
}
write.csv(combined.dt, paste0(table.dir, "age_deleted (Table 5).csv"), row.names = F)

# Combine life expectancy decomp files
daly.decomp.files <- list.files(daly.decomp.table.dir)
daly.decomp.dt <- rbindlist(mclapply(daly.decomp.files, function(file) {
	dt <- fread(paste0(daly.decomp.table.dir, file))
}, mc.cores = ncores))
daly.decomp.dt <- daly.decomp.dt[order(sex, location_name, age)]
daly.decomp.dt <- daly.decomp.dt[, lapply(.SD, sum), by = c("location_name", "sex")]
daly.decomp.dt[, age := NULL]
daly.decomp.dt[, diff := daly_count_2016 - daly_count_1990]
daly.decomp.dt[, (paste0(c("population", "age_structure", "rate"), "_prop")) := .(population_effect / diff, age_structure_effect / diff, rate_effect / diff)]
write.csv(daly.decomp.dt, paste0(table.dir, "daly_decomp (Table 6).csv"), row.names = F)

### End