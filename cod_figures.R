#########################################################################################
## Purpose: Create dataset of under-5 causes of mortality for China at the province-level
## Date created: 08/03/2017
## Date modified:
## Author: Erika Eldrenkamp
## Notes:
#########################################################################################

###### SETUP ######
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

# Packages
library(data.table)
library(reshape2)
library(ggplot2)

# Paths
fig_path <- "/homes/eeldren/"

# Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_ids.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))

# Tables
loc.table <- get_location_metadata(location_set_id = 22)
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)


###### DATA PREP ######
# Get Chinese provinces including Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)

# Get cause of death estimates at province-level
cod_full <- get_outputs("cause", location_id=prov.list, year_id="all", version="latest", age_group_id=1, sex_id="1 2",
	measure_id=1, metric_id=3, cause_id="lvl4")

cod <- cod_full[, c("age_group_id", "sex_id", "location_id", "year_id", "cause_id", "cause_name", "location_name", "val",
	"upper", "lower"), with=FALSE]

# Create dataset for cause_id-cause_name mapping
cod_key <- setkey(cod_full, "cause_id")
cod_name_ids <- unique(cod_key)
cod_name_ids <- subset(cod_name_ids, select=c("cause_name", "cause_id"))

# Get cause_ids for selected causes
cause_id_sub <- cod_name_ids[grepl("encephalopathy|Lower|complications|Congenital heart|Drowning|Other neonatal|
	|Pedestrian|Unintentional suffocation|Digestive congenital|Other congenital", cod_name_ids$cause_name), ]

# Subset COD estimates to get selected causes
cod_sub <- cod[which(cod$cause_id %in% cause_id_sub$cause_id)]

# Make COD wide so each cause is a set of three variables (value, lower, upper)
#cod_melt <- melt(cod_sub, id.vars=c("age_group_id", "sex_id", "location_id", "location_name", "year_id", "cause_id"))
#cod_reshape <- dcast(setDT(cod_melt), age_group_id + sex_id + location_name + location_id + year_id ~ variable + cause_id, value.var=c("value"))

# Get SDI at province-level
sdi_full <- get_covariate_estimates(covariate_id=881, location_id=prov.list)
sdi <- sdi_full[, c("location_id", "location_name", "year_id", "mean_value", "lower_value", "upper_value"), with=FALSE]
setnames(sdi, old=c("mean_value", "lower_value", "upper_value"), new=c("val_sdi", "lower_sdi", "upper_sdi"))

# Merge COD and SDI datasets
cod_sdi <- merge(cod_sub, sdi, by=c("location_id", "year_id", "location_name"), all=FALSE)


###### FIGURES ######
# Scatterplot of mortality rate vs. SDI for selected causes by province, all years
locs <- prov.list
pdf(fig_path)
for (loc in locs) {
	gg <- ggplot(cod_sdi, aes(val_sdi, val, color=cause_name)) + geom_point() + theme_minimal()
	print(gg)
}
dev.off



# Proportional stacked area graph
cod_sdi_val <- cod_sdi[, grepl("val_sdi|location_|year_|age_|sex_|val_", colnames(cod_sdi))]

# Categorize into discrete SDI levels (e.g., (0.1-0.2], (0.2-0.3], etc.)
