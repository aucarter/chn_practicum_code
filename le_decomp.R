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
library(data.table); library(ggplot2)

## Arguments
# args <- commandArgs(trailingOnly = TRUE)
# if(length(args) > 0) {

# } else {

# }
plot.e0 <- F
year.start <- 1990
year.end <- 2016

### Paths
lt.path <- "/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/compiled_summary_lt.csv"
plot.dir <- paste0(root, "temp/aucarter/le_decomp/")
dir.create(plot.dir, showWarnings = F)
e0.plot.path <- paste0(plot.dir, "e0_plots.pdf")


### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_best_model_versions.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_model_results.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source(paste0(root,"/Project/Mortality/shared/functions/get_age_map.r"))

matrix.div <- function(data, num, denom) {
	matrix <- as.matrix(data[, num, with = F])
	vec <- data[[denom]]
	value <- t(t(matrix) %*% diag(1 / vec))
	out.dt <- cbind(data[, setdiff(names(data), num), with = F], value)
	return(out.dt)
}

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
age.table <- data.table(get_age_map(type = "all"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)

### Code
# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- loc.table[parent_id == 44533, location_id]

# Life-tables for provinces for each sex in years of analysis
lt.dt <- fread(lt.path)[location_id %in% prov.list]

# Life-expectancy plots
if(plot.e0) {
	pdf(e0.plot.path, width = 11, height = 8.5)
	e0.plot.dt <- merge(lt.dt[age_group_id== 28 & sex_id %in% 1:2, .(year, ex, sex_id, ihme_loc_id)], 
						loc.table[, .(ihme_loc_id, location_name)], by = "ihme_loc_id")
	for(csex in 1:2){
		e0.plot.sex <- e0.plot.dt[sex_id == csex]
		sex.name <- ifelse(csex == 1, "Male", "Female")
		gg <- ggplot(e0.plot.sex[year >= 1990]) + geom_line(aes(x = year, y = ex, color = location_name)) +
			  ggtitle(paste0(sex.name, " Provincial Life-Expectancy")) + theme(legend.position = "bottom") +
			  ylab("Life-Expectancy") + xlab("Year") + guides(color=guide_legend(title="Province"))
		print(gg)
	}
	dev.off()
}

lt.dt <- lt.dt[year %in% c(year.start, year.end)]
setnames(lt.dt, "year", "year_id")

## Get draws of birth defects and population to make rates
c.cause <- meta[cause_name == "Congenital birth defects", cause_id]
cause.dt <- get_draws(gbd_id_field = "cause_id", gbd_id = c.cause, location_ids = prov.list, year_id = c(year.start, year.end),
					  source = "codcorrect", measure_ids = 1)
pop.dt <- get_population(location_id = prov.list, year_id = c(year.start, year.end), age_group_id = -1, sex_id = -1)
rate.dt <- merge(cause.dt, pop.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
rate.dt <- rate.dt[, c("year_id", "sex_id", "age_group_id", "location_id", "population", paste0("draw_", 0:999)), with = F]
# Collapse under-1
rate.dt[age_group_id %in% 2:4, age_group_id := 28]
rate.dt <- rate.dt[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "location_id")]
# Calculate rates
rate.dt <- matrix.div(rate.dt, num = paste0("draw_", 0:999), denom ="population")
rate.dt[, population := NULL]
# Fill in missing ages with 0
ages <- unique(lt.dt$age_group_id)
fill.ages <- setdiff(ages, unique(rate.dt$age_group_id))
temp.dt <- rate.dt[age_group_id == 5]
temp.dt[, (paste0("draw_", 0:999)) := 0]
fill.dt <- rbindlist(lapply(fill.ages, function(age) {
	out.dt <- copy(temp.dt)
	out.dt[, age_group_id := age]
}))
rate.dt <- rbind(rate.dt, fill.dt)

# Merge and prep mortality proportions
merge.dt <- merge(lt.dt, rate.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
merge.dt <- matrix.div(merge.dt, paste0("draw_", 0:999), "mx")

# Draws of mortality proportions long
melt.dt <- melt(merge.dt, id.vars = c("year_id", "sex_id", "age_group_id", "location_id"), 
			   measure.vars = paste0("draw_", 0:999), value.name = "mx_prop")
melt.dt[, draw := as.integer(gsub("draw_", "", variable))]
melt.dt[, variable := NULL]

# Merge on proportions and calculate cause deleted px
lt <- merge(lt.dt, melt.dt, by = c("year_id", "sex_id", "age_group_id", "location_id"))
lt <- merge(lt, age.table[, .(age_group_id, age_group_years_start)], by = "age_group_id")
setnames(lt, "age_group_years_start", "age")
lt <- lt[order(ihme_loc_id, year_id, sex_id, draw, age),]
lt[, pxdel := px ^ (1 - mx_prop)]
lt[age == max(age), pxdel := 0]

# Set first age group lx to 100000 and calculate resulting lx
lt[age == 0, lxdel := lx]
age.list <- sort(unique(lt$age))
for(i in 2:length(age.list)) {
	c.age <- age.list[i]
	prior.age <- age.list[i - 1]
	prior.lx <- lt[age == prior.age, lxdel]
	px <- lt[age == prior.age, pxdel]
	temp.lx <- prior.lx * px
	lt[age == c.age, lxdel := temp.lx]
}

# dx
lt[, lxdel_lead := shift(lxdel, type = "lead"), by = c("sex_id", "location_id", "draw", "year_id")]
lt[, dxdel := lxdel - lxdel_lead]
lt[age == max(age), dxdel := lxdel]

# update ax

# nLx







### End