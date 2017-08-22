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

## Packages
library(data.table); library(ggplot2);library(parallel)

## Arguments
single.cause <- "Congenital birth defects"
cause.names <- c(single.cause, "All causes")
years <- c(1996, 2006, 2016)
ncores <- 10

### Paths
table.dir <- paste0(root, "temp/aucarter/le_decomp/tables/")
dir.create(table.dir, showWarnings = F)
bd.prop.path <- paste0(table.dir, "bd_prop (Table 2).csv")

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root,"/Project/Mortality/shared/functions/get_age_map.r"))

matrix.div <- function(data, num, denom) {
	matrix <- as.matrix(data[, num, with = F])
	vec <- data[[denom]]
	value <- t(t(matrix) %*% diag(1 / vec))
	out.dt <- cbind(data[, setdiff(names(data), num), with = F], value)
	return(out.dt)
}

my.summary <- function(value.var) {
	list(mean = mean(value.var), lower=quantile(value.var, 0.025, names=F), upper=quantile(value.var, 0.975, names=F))
}
 
summarize.draws <- function(dt, value.vars, id.vars) {
	summary.dt <- dt[, as.list(unlist(lapply(.SD, my.summary))), by=id.vars, .SDcols=value.vars]
	setnames(summary.dt, names(summary.dt), gsub("\\.", "_", names(summary.dt), fixed=F))
}

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
age.table <- data.table(get_age_map(type = "all"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)
regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))
sex.table <- data.table(sex_id = 1:3, sex = c("Male", "Female", "All"))

### Code
prov.list <- loc.table[parent_id == 44533, location_id]

## Get draws of birth defects to make proportions
c.causes <- sapply(cause.names, function(cause) {
	meta[cause_name == cause, cause_id]
})
cause.dt <- rbindlist(lapply(c.causes, function(cause) {
	temp.dt <- get_draws(gbd_id_field = "cause_id", gbd_id = cause, location_ids = prov.list, year_id = years,
					  source = "codcorrect", measure_ids = 1, sex_id = 3, age_group_id = 22)
}))
cause.dt[, c("measure_id", "output_version_id", "metric_id") := NULL]
melt.cause <- melt(cause.dt, id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "cause_id"))
cast.cause <- dcast(melt.cause, location_id + year_id + sex_id + age_group_id + variable ~ cause_id)
setnames(cast.cause, paste0(c.causes), names(c.causes))
cast.cause[, draw := as.integer(gsub("draw_", "", variable))]
cast.cause[, variable := NULL]

# Regions
regions.dt <- merge(cast.cause, regions[, .(location_id, region_name)], by = "location_id")
regions.dt[, location_name := region_name]
regions.dt <- regions.dt[, lapply(.SD, sum, na.rm = T), by = .(location_name, year_id, sex_id, draw), .SDcols = names(c.causes)]
regions.dt <- merge(regions.dt, regions[, .(location_name, location_id)], by = "location_name")
regions.dt[, location_name := NULL]
bound.dt <- rbind(cast.cause, regions.dt, fill = T)

# Mainland
mainland.dt <- copy(cast.cause)
mainland.dt[, location_id := 44533]
mainland.dt <- mainland.dt[, lapply(.SD, sum, na.rm = T), by = .(location_id, year_id, sex_id, draw), .SDcols = names(c.causes)]
bound.dt <- rbind(bound.dt, mainland.dt, fill = T)

# Rates
bound.dt[, pct := get(names(c.causes)[1]) / get(names(c.causes)[2]) * 100]

# Summarize and prep for output
summary.bd <- summarize.draws(bound.dt, id.vars = c("location_id", "year_id", "sex_id"), value.vars = "pct")
summary.bd <- merge(summary.bd, regions[, .(location_id, location_name, region_name)], by = "location_id", all.x = T)
summary.bd[location_id == 44533, location_name := "China (without Hong Kong and Macao)"]
setnames(summary.bd, c("year_id"), c("year"))
summary.bd <- merge(summary.bd, sex.table, by = "sex_id")
summary.bd <- summary.bd[order(year, sex, -region_name, location_name), .(location_name, region_name, year, sex, pct_mean, pct_lower, pct_upper)]
write.csv(summary.bd, bd.prop.path, row.names = F)

### End