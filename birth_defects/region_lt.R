################################################################################
## Purpose: Create China Regional Life Tables
## Date created: 
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
library(data.table);library(parallel)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
	loc <- args[1]
} else {
	loc <- "CHN_44534"
}
ncores <- 10
years <- c(1996, 2006, 2016)


### Paths
lt.dir <- "/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/"
out.dir <- paste0(root, "temp/aucarter/le_decomp/region_lts/")
dir.create(out.dir, showWarnings = F)

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_life_table.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))

### Code
region.name <- regions[ihme_loc_id == loc, location_name]
prov.list <- regions[region_name == region.name, location_id]

## Provincial life tables
lt.dt <- rbindlist(mclapply(prov.list, function(prov) {
	temp.dt <- fread(paste0(lt.dir, "lt_", prov, ".csv"))[year_id %in% years]
}, mc.cores = ncores))

## Populations
pop.dt <- get_population(location_id = prov.list, year_id = years, age_group_id = -1, sex_id = -1)
pop.dt[, process_version_map_id := NULL]
pop.dt <- pop.dt[, lapply(.SD, sum), by = c("location_id", "year_id", "sex_id", "age_group_id")]
# Repeat 95+ for all older age groups
lt_granular_95_groups <- c(33,44,45,148)
over_95_pops <- pop.dt[age_group_id==235,]
over_95_pops[,age_group_id:=NULL]
map <- data.table(expand.grid(age_group_id=lt_granular_95_groups,sex_id=unique(over_95_pops[,sex_id]),
                              location_id=unique(over_95_pops[,location_id]),year_id=unique(over_95_pops[,year_id])))
over_95_pops <- merge(over_95_pops,map,by=c("sex_id","year_id","location_id"))
pop.dt <- pop.dt[!age_group_id %in% c(235,lt_granular_95_groups),]
pop.dt <- rbindlist(list(pop.dt,over_95_pops),use.names=T)

# Merge together LT and pop and region map
merge.dt <- merge(lt.dt, pop.dt, by = c("location_id", "year_id", "sex_id", "age_group_id"))
region.merge <- merge(merge.dt, regions[, .(location_id, region_name)], by = "location_id")
region.merge[, mx := mx * population]
region.merge[, ax := ax * mx]
region.dt <- region.merge[, lapply(.SD, sum), by = .(region_name, year_id, sex_id, age_group_id, draw), .SDcols = c("ax", "mx", "population")]
region.dt[, ax := ax / mx]
region.dt[, mx := mx / population]
region.dt[, c("region_name", "population") := NULL]

## Complete Life Table
# n
region.dt[, n := 5]
region.dt[age_group_id == 28, n := 1]
region.dt[age_group_id == 5, n := 4]

# qx
region.dt[, qx := (n * mx)/ (1 + (n - ax) * mx)]
region.dt[age_group_id == 148, qx := 1]

# px
region.dt[, px := 1 - qx]

# lx
region.dt[age_group_id == 28, lx := 100000]
age.list <- c(28, setdiff(unique(region.dt$age_group_id), 28))
for(i in 2:length(age.list)) {
	c.age <- age.list[i]
	prior.age <- age.list[i - 1]
	prior.lx <- region.dt[age_group_id == prior.age, lx]
	px <- region.dt[age_group_id == prior.age, px]
	temp.lx <- prior.lx * px
	region.dt[age_group_id == c.age, lx := temp.lx]
}

# dx
region.dt[, lx_lead := shift(lx, type = "lead"), by = c("sex_id", "draw", "year_id")]
region.dt[, dx := lx - lx_lead]
region.dt[age_group_id == max(age_group_id), dx := lx]

# nLx
region.dt[, nLx := n * lx_lead + ax * dx]
region.dt[age_group_id == max(age_group_id), nLx := lx / mx]
region.dt[, lx_lead := NULL]

# Tx
region.dt[, Tx := rev(cumsum(rev(nLx))), by = c("sex_id", "draw", "year_id")]

# ex
region.dt[, ex := Tx / lx]

loc.id <- regions[ihme_loc_id == loc, location_id]
region.dt[, location_id := loc.id]

## Write
write.csv(region.dt, paste0(out.dir, loc, "_lt.csv"), row.names = F)

### End