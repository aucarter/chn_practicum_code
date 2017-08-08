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
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
	loc <- args[1]
} else {	
	loc <- "CHN_491"
}
single.cause <- "Congenital birth defects"
cause.names <- c(single.cause, "All causes")
years <- c(1996, 2006, 2016)
table.e0 <- T
table.bd.count <- T
table.mx <- F
plot.props <- T
ncores <- 4

### Paths
lt.dir <- "/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/"

# Tables
table.dir <- paste0(root, "temp/aucarter/le_decomp/tables/")
dir.create(table.dir, showWarnings = F)
e0.table.dir <- paste0(table.dir, "e0/")
dir.create(e0.table.dir, showWarnings = F)
bd.count.table.dir <- paste0(table.dir, "bd_dist/")
dir.create(bd.count.table.dir, showWarnings = F)

# Plots
plot.dir <- paste0(root, "temp/aucarter/le_decomp/")
dir.create(plot.dir, showWarnings = F)
e0.plot.path <- paste0(plot.dir, "e0_plots.pdf")
mx.plot.path <- paste0(plot.dir, "mx_plots.pdf")
prop.plot.path <- paste0(plot.dir, "le_prop_plot.pdf")

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_life_table.R"))
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
loc.id <- loc.table[ihme_loc_id == loc, location_id]
loc.name <- loc.table[ihme_loc_id == loc, location_name]

# Life-tables for each sex in years of analysis
lt.dt <- fread(paste0(lt.dir, "lt_", loc.id, ".csv"))[year_id %in% years]

# Write life expectancy at birth
if(table.e0) {
	e0.dt <- merge(lt.dt[age_group_id == 28 & year_id %in% years, .(year_id, ex, sex_id, location_id, draw)],
			       loc.table[, .(location_id, location_name)], by = "location_id")
	e0.dt <- summarize.draws(e0.dt, value.vars = "ex", id.vars = c("location_name", "year_id", "sex_id"))
	setnames(e0.dt, c("year_id"), c("year"))
	e0.dt <- merge(e0.dt, sex.table, by = "sex_id")
	e0.dt <- e0.dt[order(location_name, year, sex), .(location_name, year, sex, ex_mean, ex_lower, ex_upper)]
	write.csv(e0.dt, paste0(e0.table.dir, loc, ".csv"), row.names = F)
}

# Set last age group n to 5
lt.dt[age_group_id == max(age_group_id), n := 5]

# Set radix to 1 instead of 100,000 and adjust and nLx deaths accordingly 
lt.dt[, c("lx", "dx", "nLx", "Tx") := .(lx / 1e5, dx / 1e5, nLx / 1e5, Tx / 1e5)]
lt.dt <- lt.dt[year_id %in% years]

## Get draws of birth defects and all cause to make proportions
c.causes <- sapply(cause.names, function(cause) {
	meta[cause_name == cause, cause_id]
})
cause.dt <- rbindlist(lapply(c.causes, function(cause) {
	temp.dt <- get_draws(gbd_id_field = "cause_id", gbd_id = cause, location_ids = loc.id, year_id = years,
					  source = "codcorrect", measure_ids = 1, sex_id = 1:3)
}))
cause.dt[, c("measure_id", "output_version_id", "location_id", "metric_id") := NULL]
melt.cause <- melt(cause.dt, id.vars = c("year_id", "sex_id", "age_group_id", "cause_id"))
cast.cause <- dcast(melt.cause, year_id + sex_id + age_group_id + variable ~ cause_id)
setnames(cast.cause, paste0(c.causes), names(c.causes))
cast.cause[, draw := as.integer(gsub("draw_", "", variable))]
cast.cause[, variable := NULL]
# Collapse under-1
cast.cause[age_group_id %in% 2:4, age_group_id := 28]
cast.cause <- cast.cause[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "draw")]
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

# Merge with life table
lt <- merge(lt.dt, cast.cause, by = c("year_id", "sex_id", "age_group_id", "draw"))
lt <- merge(lt, age.table[, .(age_group_id, age_group_years_start)], by = "age_group_id")
setnames(lt, "age_group_years_start", "age")
lt <- lt[order(year_id, sex_id, draw, age),]

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

# Tx
lt[, Txdel := rev(cumsum(rev(nLx))), by = c("sex_id", "draw", "year_id")]

# ex
lt[, exdel := Txdel / lxdel]

# Decompose
lt[, nLxcause := ifelse(nLxdel == 0, 0, (nLx / nLxdel) * n)]
cast.lt <- dcast(lt, age + sex_id + location_id + draw + n ~ year_id, value.var = c("nLxcause", "nLxdel"))
decomp.cols <- c()
for (y1 in years) {
	for (y2 in years[-1]) {
	    year.start <- y1
	    if (y1 >= y2) {
	    	next
	    } else {
	    	year.end <- y2
	    }
		cast.lt[, (paste0("decomp_", year.start, "_", year.end)) := (get(paste0("nLxcause_", year.end)) - get(paste0("nLxcause_", year.start))) * ((get(paste0("nLxdel_", year.end)) + get(paste0("nLxdel_", year.start))) / (2 * n)) ]
		decomp.cols <- c(decomp.cols, paste0("decomp_", year.start, "_", year.end))
	}
}

collapsed.decomp <- cast.lt[, lapply(.SD, sum), by = .(sex_id, draw), .SDcols = decomp.cols]
summary.decomp <- summarize.draws(collapsed.decomp, value.vars = decomp.cols, id.vars = "sex_id")
melt.summary <- melt(summary.decomp, id.vars = "sex_id")
melt.summary[, year1 := as.integer(tstrsplit(as.character(variable), "_")[[2]])]
melt.summary[, year2 := as.integer(tstrsplit(as.character(variable), "_")[[3]])]
melt.summary[, stat := tstrsplit(as.character(variable), "_")[[4]]]
cast.summary <- dcast(melt.summary, sex_id + year1 + year2 ~ stat, value.var = "value")

# LE diff
diff.dt <- lt.dt[age_group_id == 28 & sex_id %in% 1:2 & year_id %in% years, .(year_id, ex, sex_id, location_id)]
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