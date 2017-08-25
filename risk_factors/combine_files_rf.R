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
table.dir <- paste0(root, "temp/wgodwin/chn/decomp/data/")
death.decomp.table.dir <- paste0(table.dir, "death_decomp/")


### Code
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
death.decomp.files <- list.files(table.dir)
death.decomp.dt <- rbindlist(mclapply(death.decomp.files, function(file) {
	dt <- fread(paste0(table.dir, file))
}, mc.cores = ncores))
death.decomp.dt <- death.decomp.dt[order(sex, location_name, age_group_id)]
write.csv(death.decomp.dt, paste0(table.dir, "death_decomp_compile.csv"), row.names = F)

### End