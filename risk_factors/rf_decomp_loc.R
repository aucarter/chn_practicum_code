################################################################################
## Purpose: Decompose changes in death counts into population, age-structure,
##          risk-deleted changes, and risk exposure changes
## Date created: 08/14/2017
## Date modified:
## Author: Austin Carter, aucarter@uw.edu
## adapted by: Will Godwin
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

# Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  loc <- args[1]
  risk <- as.integer(args[2])
} else {
  loc <- "CHN_492"  ## Test case
  risk <- 86
}
years <- c(1990, 2016)
ages <- c(2,3,4,5)

### Paths
decomp.dir <- paste0(root, "temp/wgodwin/chn/decomp/data/")
dir.create(decomp.dir, showWarnings = F)
out.path <- paste0(decomp.dir, loc, "_", risk, ".csv")

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_rei_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")
source(paste0(root,"/Project/Mortality/shared/functions/get_age_map.r"))

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
cause.meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)
risk.meta <- get_rei_metadata(rei_set_id = 2, gbd_round_id = 4)
risk.name <- risk.meta[rei_id == risk, rei_name]
sex.table <- data.table(sex_id = 1:3, sex = c("Male", "Female", "All"))
age.table <- data.table(get_age_map(type = "all"))

### Code

# Identify location_id for specific iso3
loc.id <- loc.table[ihme_loc_id == loc, location_id]
loc.name <- loc.table[ihme_loc_id == loc, location_name]

## Read in population and calculate proportion of population in each age group, then reshape
pop.dt <- get_population(location_id = loc.id, year_id = years, age_group_id = ages, sex_id = 1:3)
pop.dt[, process_version_map_id := NULL]
pop.dt[, age_structure := population / sum(population), by = c("year_id", "sex_id")]
pop.dt[, population := sum(population), by = c("year_id", "sex_id")]
pop.dt[, year := ifelse(year_id == years[1], 1, 2)]
pop.cast <- dcast(pop.dt, age_group_id + sex_id ~ year, value.var = c("age_structure", "population"))

## Read in risks
risk.dt <- get_outputs(topic = "rei", rei_id = risk, 
                       measure_id = 1, metric_id = c(1, 2, 3), location_id = loc.id, year_id = years, 
                       age_group_id = ages, sex_id = 1:3, gbd_round_id = 4,  version = "latest")
  risk.dt[, year := ifelse(year_id == years[1], 1, 2)]
  risk.dt[is.na(val), val := 0]
  risk.dt[, metric := ifelse(metric_id == 1, "count", ifelse(metric_id == 2, "paf", "rate"))]
  risk.cast <- dcast(risk.dt, age_group_id + sex_id ~ metric + year, value.var = "val")  

# Calculate risk-deleted factor and exposure change factor
risk.cast[, under_rate_1 := (rate_1/paf_1) * (1-paf_1)]
risk.cast[, under_rate_2 := (rate_2/paf_2) * (1-paf_2)]
risk.cast[, risk_1 := paf_1/(1-paf_1)]
risk.cast[, risk_2 := paf_2/(1-paf_2)]

## Combine and reshape
dt <- merge(pop.cast, risk.cast, by = c("age_group_id", "sex_id"))

  # dt[, test1 := population_1 * age_structure_1 * under_rate_1 * risk_1]
  # dt[, test2 := population_2 * age_structure_2 * under_rate_2 * risk_2]
  # dt[, popsum2 := sum(age_structure_2), by = "sex_id"]
  # dt[, ratecheck1 := under_rate_1 * risk_1]

  # dt[, decompsum := population_effect + age_structure_effect + under_rate_effect + risk_effect]


# Calculate 3-factor effects
dt[, population_effect := ((age_structure_1 * under_rate_1 * risk_1 + age_structure_2 * under_rate_2 * risk_2) / 4 +
                           (age_structure_1 * under_rate_1 * risk_2 + age_structure_1 * under_rate_2 * risk_1) +
                           (age_structure_2 * under_rate_1 * risk_1 + age_structure_2 * under_rate_2 * risk_1) +
                           (age_structure_2 * under_rate_1 * risk_2 + age_structure_1 * under_rate_2 * risk_2)/ 12) * (population_2 - population_1)]
dt[, age_structure_effect := ((population_1 * under_rate_1 * risk_1 + population_2 * under_rate_2 * risk_2) / 4 +
                              (population_1 * under_rate_1 * risk_2 + population_2 * under_rate_1 * risk_1) +
                              (population_1 * under_rate_2 * risk_1 + population_2 * under_rate_2 * risk_1) +
                              (population_1 * under_rate_2 * risk_2 + population_2 * under_rate_1 * risk_2)/ 12) * (age_structure_2 - age_structure_1)]
dt[, under_rate_effect :=  ((population_1 * age_structure_1 * risk_1 + population_2 * age_structure_2 * risk_2) / 4 +
                            (population_1 * age_structure_1 * risk_2 + population_2 * age_structure_1 * risk_1) +
                            (population_1 * age_structure_2 * risk_1 + population_2 * age_structure_2 * risk_1) +
                            (population_1 * age_structure_2 * risk_2 + population_2 * age_structure_1 * risk_2)/ 12)* (under_rate_2 - under_rate_1)]
dt[, risk_effect :=  ((population_1 * age_structure_1 * under_rate_1 + population_2 * age_structure_2 * under_rate_2) / 4 +
                      (population_1 * age_structure_1 * under_rate_2 + population_2 * age_structure_1 * under_rate_1) +  
                      (population_1 * age_structure_2 * under_rate_1 + population_2 * age_structure_2 * under_rate_1) + 
                      (population_1 * age_structure_2 * under_rate_2 + population_2 * age_structure_1 * under_rate_2)/ 12)* (risk_2 - risk_1)]

#Add informative variables and prep for saving
dt[, ihme_loc_id := loc]
dt[, location_name := loc.name]
dt[, rei_name := risk.name]
sex.dt <- merge(dt, sex.table, by = "sex_id")
age.dt <- merge(sex.dt, age.table[, .(age_group_id, age_group_years_start)], by = "age_group_id")
setnames(age.dt, "age_group_years_start", "age")

#Clean and write
out.dt <- age.dt[order(sex, age_group_id), .(location_name, rei_name, sex, age_group_id, 
                 count_1, count_2, population_effect, age_structure_effect, under_rate_effect, 
                 risk_effect)]
write.csv(out.dt, out.path, row.names = F)
### End