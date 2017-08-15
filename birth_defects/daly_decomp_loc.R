################################################################################
## Purpose: Decompose changes in DALYs counts into population, age-structure
##          and rate changes
## Date created: 08/14/2017
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
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  loc <- args[1]
} else {
  loc <- "CHN_44533"
}
years <- c(1990, 2016)
ages <- c(28, 5, 6, 7)
cause <- "Congenital birth defects"

### Paths
daly.dir <- paste0(root, "temp/aucarter/le_decomp/tables/daly_decomp/")
dir.create(daly.dir, showWarnings = F)
out.path <- paste0(daly.dir, loc, ".csv")

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_rei_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
cause.meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)
risk.meta <- get_rei_metadata(rei_set_id = 2, gbd_round_id = 4)
regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))

### Code

# Sort out region stuff
if(loc != "CHN_44533") {
  region <- regions[ihme_loc_id == loc, region]
  if(region) {
    loc.table <- regions
  }
} else {
  region <- 1
}
loc.id <- loc.table[ihme_loc_id == loc, location_id]
loc.name <- loc.table[ihme_loc_id == loc, location_name]
if(region) {
  if(loc != "CHN_44533") {
    loc.id <- loc.table[region_name == loc.name, location_id]
  } else{
    loc.id <- loc.table[parent_id == 44533, location_id]
  }
}
## Read in population
pop.dt <- get_population(location_id = loc.id, year_id = years, age_group_id = ages, sex_id = 1:3)
pop.dt[, process_version_map_id := NULL]
if(region) {
  pop.dt <- pop.dt[, lapply(.SD, sum), by = .(year_id, sex_id, age_group_id)]
  pop.dt[, location_id := NULL]
  hold.pop <- copy(pop.dt)
}
pop.dt[, age_structure := population / sum(population), by = c("year_id", "sex_id")]
pop.dt[, population := sum(population), by = c("year_id", "sex_id")]
pop.dt[, year := ifelse(year_id == years[1], 1, 2)]
pop.cast <- dcast(pop.dt, age_group_id + sex_id ~ year, value.var = c("age_structure", "population"))

## Read in DALYs
cause.id <- cause.meta[cause_name == cause, cause_id]
daly.dt <- get_outputs(topic = "cause", cause_id = cause.id, 
                       measure_id = 2, metric_id = c(1, 3), location_id = loc.id, year_id = years, 
                       age_group_id = ages, sex_id = 1:3, gbd_round_id = 4, compare_version_id = 212)
if(region) {
  daly.dt <- daly.dt[metric_id == 1, lapply(.SD, sum), by = .(year_id, sex_id, age_group_id), .SDcols = "val"]
  merge.daly <- merge(daly.dt, hold.pop, by = c("year_id", "sex_id", "age_group_id"))
  merge.daly[, rate := val / population] 
  setnames(merge.daly, "val", "count")
  merge.daly[, year := ifelse(year_id == years[1], 1, 2)]
  daly.cast <- dcast(merge.daly, age_group_id + sex_id ~ year, value.var = c("count", "rate"))    
} else {
  daly.dt[, year := ifelse(year_id == years[1], 1, 2)]
  daly.dt[, metric := ifelse(metric_id == 1, "count", "rate")]
  daly.cast <- dcast(daly.dt, age_group_id + sex_id ~ metric + year, value.var = "val")  
}

## Combine and reshape
dt <- merge(pop.cast, daly.cast, by = c("age_group_id", "sex_id"))

# Calculate 3-factor effects
dt[, population_effect := ((age_structure_1 * rate_1 + age_structure_2 * rate_2) / 3 +
                           (age_structure_1 * rate_2 + age_structure_2 * rate_1) / 6) * (population_2 - population_1)]
dt[, age_structure_effect := ((rate_1 * population_1 + rate_2 * population_2) / 3 +
                              (rate_1 * population_2 + rate_2 * population_1) / 6) * (age_structure_2 - age_structure_1)]
dt[, rate_effect := ((age_structure_1 * population_1 + age_structure_2 * population_2) / 3 +
                     (age_structure_1 * population_2 + age_structure_2 * population_1) / 6) * (rate_2 - rate_1)]

dt[, ihme_loc_id := loc]
dt[, location_name := loc.name]

write.csv(dt, out.path, row.names = F)
### End