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
ncores <- 10

### Paths
# e0
table.dir <- paste0(root, "temp/aucarter/le_decomp/tables/")
e0.table.dir <- paste0(table.dir, "e0/")
decomp.table.dir <- paste0(table.dir, "decomp/")
deleted.table.dir <- paste0(table.dir, "deleted/")

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))

### Tables
loc.table <- get_location_metadata(location_set_id = 22)

### Code
# Combine life expectancy files
e0.files <- list.files(e0.table.dir)
e0.dt <- rbindlist(mclapply(e0.files, function(file) {
	dt <- fread(paste0(e0.table.dir, file))
}, mc.cores = ncores))
e0.dt <- e0.dt[order(year, sex, location_name)]
write.csv(e0.dt, paste0(table.dir, "e0.csv"), row.names = F)

# Combine life expectancy decomp files
decomp.files <- list.files(decomp.table.dir)
decomp.dt <- rbindlist(mclapply(decomp.files, function(file) {
	dt <- fread(paste0(decomp.table.dir, file))
}, mc.cores = ncores))
decomp.dt <- decomp.dt[order(sex, location_name)]
write.csv(decomp.dt, paste0(table.dir, "decomp.csv"), row.names = F)

# Combine cause-deleted life expectancy files
deleted.files <- list.files(deleted.table.dir)
deleted.dt <- rbindlist(mclapply(deleted.files, function(file) {
	dt <- fread(paste0(deleted.table.dir, file))
}, mc.cores = ncores))
deleted.dt <- deleted.dt[order(year, sex, location_name)]
write.csv(deleted.dt, paste0(table.dir, "deleted.csv"), row.names = F)

### End