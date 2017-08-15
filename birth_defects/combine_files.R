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

### Paths
table.dir <- paste0(root, "temp/aucarter/le_decomp/tables/")
e0.table.dir <- paste0(table.dir, "e0/")
decomp.table.dir <- paste0(table.dir, "decomp/")
deleted.table.dir <- paste0(table.dir, "deleted")
daly.decomp.table.dir <- paste0(table.dir, "daly_decomp/")


### Code
# Combine life expectancy files
e0.files <- list.files(e0.table.dir)
e0.dt <- rbindlist(mclapply(e0.files, function(file) {
	dt <- fread(paste0(e0.table.dir, file))
}, mc.cores = ncores))
e0.dt <- e0.dt[order(year, sex, location_name)]
write.csv(e0.dt, paste0(table.dir, "e0 (Table 1).csv"), row.names = F)

# Combine life expectancy decomp files
decomp.files <- list.files(decomp.table.dir)
decomp.dt <- rbindlist(mclapply(decomp.files, function(file) {
	dt <- fread(paste0(decomp.table.dir, file))
}, mc.cores = ncores))
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
for(del.age in c(28, 5)) {
	age.group <- ifelse(del.age == 28, "0-1", "1-4+")
	age.table.dir <- paste0(table.dir, del.age, "_deleted")
	deleted.files <- list.files(age.table.dir)
	deleted.dt <- rbindlist(mclapply(deleted.files, function(file) {
		dt <- fread(paste0(table.dir, del.age, "_deleted/", file))
	}, mc.cores = ncores))
	deleted.dt <- deleted.dt[order(year, sex, location_name)]
	write.csv(deleted.dt, paste0(table.dir, age.group, "_deleted (Table 5).csv"), row.names = F)
}

# Combine life expectancy decomp files
daly.decomp.files <- list.files(daly.decomp.table.dir)
daly.decomp.dt <- rbindlist(mclapply(daly.decomp.files, function(file) {
	dt <- fread(paste0(daly.decomp.table.dir, file))
}, mc.cores = ncores))
daly.decomp.dt <- daly.decomp.dt[order(sex, location_name, age)]
write.csv(daly.decomp.dt, paste0(table.dir, "daly_decomp (Table 6).csv"), row.names = F)

### End