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
# args <- commandArgs(trailingOnly = TRUE)
# if(length(args) > 0) {

# } else {

# }
single.cause <- "Congenital birth defects"
cause.names <- c(single.cause, "All causes")
year.start <- 1990
year.end <- 2016
plot.e0 <- F
table.e0 <- T
plot.mx <- F
plot.props <- T
ncores <- 10

### Paths
lt.path <- "/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/compiled_summary_lt.csv"
plot.dir <- paste0(root, "temp/aucarter/le_decomp/")
dir.create(plot.dir, showWarnings = F)
e0.plot.path <- paste0(plot.dir, "e0_plots.pdf")
mx.plot.path <- paste0(plot.dir, "mx_plots.pdf")
prop.plot.path <- paste0(plot.dir, "le_prop_plot.pdf")

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

regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))

### Code
# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- loc.table[parent_id == 44533, location_id]

# Life-tables for provinces for each sex in years of analysis
# lt.dt <- fread(lt.path)[location_id %in% c(prov.list, 6)]
lt.dt <- rbindlist(mclapply(c(prov.list, 6), function(loc.id) {
	dt <- fread(paste0("/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/lt_", loc.id, ".csv"))
}, mc.cores = ncores))
# Set last age group n to 5
lt.dt[age_group_id == max(age_group_id), n := 5]
# Life expectancy plots
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

# Life expectancy table
if(table.e0) {
	e0.years <- c(1996, 2006, 2016)
	e0.prov <- merge(lt.dt[age_group_id == 28 & year %in% e0.years, .(year, ex, Tx, lx, sex_id, ihme_loc_id)],
			       loc.table[, .(ihme_loc_id, location_name)], by = "ihme_loc_id")
	e0.prov <- merge(e0.prov, region.locs[, .(location_name, region_name)], by = "location_name")
	e0.region <- e0.prov[, lapply(.SD, sum), by = .(region_name, sex_id, year), .SDcols = c("Tx", "lx")]
	e0.region[, ex := Tx / lx]
	e0.region[, location_name := region_name]
	e0.nat <- e0.prov[, lapply(.SD, sum), by = .(sex_id, year), .SDcols = c("Tx", "lx")]
	e0.nat[, ex := Tx / lx]
	e0.nat[, location_name := "China"]
	e0.dt <- rbindlist(list(e0.prov, e0.region, e0.nat), use.names = T, fill = T)
}

# Set radix to 1 instead of 100,000 and adjust and nLx deaths accordingly 
lt.dt[, c("lx", "dx", "nLx") := .(lx / 100000, dx / 100000, nLx / 100000)]
lt.dt <- lt.dt[year %in% c(year.start, year.end)]
setnames(lt.dt, "year", "year_id")

## Get draws of birth defects and all cause to make proportions
c.causes <- sapply(cause.names, function(cause) {
	meta[cause_name == cause, cause_id]
})
cause.dt <- rbindlist(lapply(c.causes, function(cause) {
	temp.dt <- get_draws(gbd_id_field = "cause_id", gbd_id = cause, location_ids = prov.list, year_id = c(year.start, year.end),
					  source = "codcorrect", measure_ids = 1)
}))
cause.dt[, c("measure_id", "output_version_id") := NULL]
melt.cause <- melt(cause.dt, id.vars = c("location_id", "year_id", "sex_id", "age_group_id", "cause_id"))
cast.cause <- dcast(melt.cause, location_id + year_id + sex_id + age_group_id + variable ~ cause_id)
setnames(cast.cause, paste0(c.causes), names(c.causes))
cast.cause[, draw := as.integer(gsub("draw_", "", variable))]
cast.cause[, variable := NULL]
# Collapse under-1
cast.cause[age_group_id %in% 2:4, age_group_id := 28]
cast.cause <- cast.cause[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "location_id", "draw")]
# Calculate prop
cast.cause[, mx_prop := get(names(c.causes)[1]) / get(names(c.causes)[2])]
cast.cause[is.na(mx_prop), mx_prop := 0]
cast.cause[, names(c.causes) := NULL]
# Fill in missing ages with 0
ages <- unique(lt.dt$age_group_id)
fill.ages <- setdiff(ages, unique(cast.cause$age_group_id))
temp.dt <- cast.cause[age_group_id == 5]
temp.dt[, mx_prop := 0]
fill.dt <- rbindlist(lapply(fill.ages, function(age) {
	out.dt <- copy(temp.dt)
	out.dt[, age_group_id := age]
}))
cast.cause <- rbind(cast.cause, fill.dt)

# Read in population for weighting in regional aggretation of LT
pop.dt <- get_population(location_id = prov.list, year_id = c(year.start, year.end), age_group_id = -1, sex_id = -1)

# Merge with life table
lt <- merge(lt.dt, cast.cause, by = c("year_id", "sex_id", "age_group_id", "location_id"))
lt <- merge(lt, age.table[, .(age_group_id, age_group_years_start)], by = "age_group_id")
setnames(lt, "age_group_years_start", "age")
lt <- lt[order(ihme_loc_id, year_id, sex_id, draw, age),]

# Plot birth-defect mx
if(plot.mx) {
	pdf(mx.plot.path, width = 11, height = 8.5)
	mx.plot.dt <- merge(lt[year_id == 1990 & sex_id %in% 1:2 & draw == 0, .(year_id, mx_prop, sex_id, ihme_loc_id, age)], 
						loc.table[, .(ihme_loc_id, location_name)], by = "ihme_loc_id")
	for(csex in 1:2){
		mx.plot.sex <- mx.plot.dt[sex_id == csex]
		sex.name <- ifelse(csex == 1, "Male", "Female")
		gg <- ggplot(mx.plot.sex) + geom_line(aes(x = age, y = mx_prop, color = location_name)) +
			  ggtitle(paste0(sex.name, " Provincial Birth Defect Mortality Proportions")) + theme(legend.position = "bottom") +
			  ylab("Mortality Proportion") + xlab("Age") + guides(color=guide_legend(title="Province"))
		print(gg)
	}
	dev.off()	
}

## Calculate cause deleted life table
# px
lt[, pxdel := px ^ (1 - mx_prop)]
lt[age == max(age), pxdel := 0]

# Set first age group lx to 1 and calculate resulting lx
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
lt[, axdel := ax]
ax.ages <- c(0, 1, 5, max(age.list))
lt[age %in% ax.ages, axdel := n + (1 - mx_prop) * ((1 - px) / (1 - pxdel)) * (ax - n)]

# nLx
lt[, nLxdel := n * lxdel_lead + axdel * dxdel]
lt[age == max(age), nLxdel := (ex / (1 - mx_prop)) * lxdel]

# Decompose
lt[, nLxcause := ifelse(nLxdel == 0, 0, (nLx / nLxdel) * n)]
cast.lt <- dcast(lt, age + sex_id + location_id + draw + n ~ year_id, value.var = c("nLxcause", "nLxdel"))
cast.lt[, decomp := (get(paste0("nLxcause_", year.end)) - get(paste0("nLxcause_", year.start))) * ((get(paste0("nLxdel_", year.end)) + get(paste0("nLxdel_", year.start))) / (2 * n)) ]
collapse.decomp <- cast.lt[, .(total_decomp = sum(decomp)), by = .(sex_id, location_id, draw)]

# LE diff
diff.dt <- lt.dt[age_group_id == 28 & sex_id %in% 1:2 & year_id %in% c(year.start, year.end), .(year_id, ex, sex_id, location_id)]
cast.diff <- dcast(diff.dt, sex_id + location_id ~ year_id, value.var = "ex")
cast.diff[, diff := get(as.character(year.end)) - get(as.character(year.start))]
write.csv(cast.diff, "/home/j/temp/aucarter/le_decomp/le_diff.csv", row.names = F)

# Convert to pct of total change in LE
merge.decomp <- merge(collapse.decomp, cast.diff[, .(sex_id, location_id, diff)], by = c("sex_id", "location_id"))
merge.decomp[, pct := total_decomp / diff * 100]

# Collapse and summarize
summary.decomp <- merge.decomp[, .(mean = mean(pct), lower = quantile(pct, 0.025), 
								  upper = quantile(pct, 0.975)), by = .(sex_id, location_id)]

summary.decomp[, Sex := ifelse(sex_id == 1, "Male", "Female")]
summary.decomp <- merge(summary.decomp, loc.table[,.(location_id, location_name)], by = "location_id")

# Plot proportion of life expectancy improvement from birth defects
if(plot.props) {
	pdf(prop.plot.path, width = 11, height = 8.5)	
	gg <- ggplot(summary.decomp) + geom_bar(aes(x = location_name, y = mean, fill = Sex), position = "dodge", stat = "identity") +
		  geom_errorbar(aes(x = location_name, ymin = lower, ymax = upper, group = Sex), position = position_dodge(0.9), width = 0.5) +
		  ggtitle("Proportion of Life Expectancy Gains Due to Improvement in Birth Defects Mortality") + 
		  theme(legend.position = "bottom") + ylab("Life Expectancy Proportion (Percent)") + xlab("Province") +
		  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))		  
		  print(gg)
	dev.off()		
}
### End