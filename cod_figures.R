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
library(gridExtra)
library(directlabel)
library(maptools)

# Paths
path <- paste0(root, "temp/eeldren/mchs/")

# Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_ids.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source(paste0(root, "WORK/05_risk/central/code/maps/global_map.R"))

# Tables
loc.table <- get_location_metadata(location_set_id = 22)
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)


###### DATA PREP ######
# Get Chinese provinces including Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)

# Get cause of death estimates at province-level
cod_full <- get_outputs("cause", location_id=prov.list, year_id="all", version="latest", age_group_id=1, sex_id="all",
	measure_id=1, metric_id=3, cause_id="lvl4")

cod <- cod_full[, c("age_group_id", "sex_id", "sex", "location_id", "year_id", "cause_id", "cause_name", "location_name", "val",
	"upper", "lower"), with=FALSE]

# Create dataset for cause_id-cause_name mapping
cod_key <- setkey(cod_full, "cause_id")
cod_name_ids <- unique(cod_key)
cod_name_ids <- subset(cod_name_ids, select=c("cause_name", "cause_id"))

# Get cause_ids for selected causes
cause_id_sub <- cod_name_ids[grepl("encephalopathy|Lower|complications|Congenital heart|Drowning|Other neonatal|
	|Pedestrian|Unintentional suffocation|Digestive congenital|Other congenital|Neonatal sepsis|
	|Diarrheal disease|Whooping|Neural tube|Protein-energy",
	cod_name_ids$cause_name), ]

# Subset COD estimates to get selected causes
cod_sub <- cod[which(cod$cause_id %in% cause_id_sub$cause_id)]

# Get SDI at province-level
sdi_full <- get_covariate_estimates(covariate_id=881, location_id=prov.list)
sdi <- sdi_full[, c("location_id", "location_name", "year_id", "mean_value", "lower_value", "upper_value"), with=FALSE]
setnames(sdi, old=c("mean_value", "lower_value", "upper_value"), new=c("val_sdi", "lower_sdi", "upper_sdi"))

# Merge COD and SDI datasets
cod_sdi <- merge(cod_sub, sdi, by=c("location_id", "year_id", "location_name"), all=FALSE)


###### FIGURES ######
# Scatterplot of mortality rate vs. SDI for selected causes by province, all years
fig.path <- paste0(path, "cod10_sdi.pdf")
pdf(fig.path)
for (loc in prov.list) {
	gg <- ggplot(cod_sdi[location_id==loc & sex_id==sex], aes(val_sdi, val, color=cause_name)) + geom_point()
	print(gg)
	#ggsave(paste('/home/j/temp/eeldren/mchs/plot_', loc, '.pdf', sep=''))
}
#ggsave("test.pdf", do.call("marrangeGrob", c(unlist(gg,recursive=FALSE),nrow=2,ncol=1)))
#graphics.off()
dev.off

# Line graph of year vs. mortality rate by province for multiple causes
loc.name <- unique(cod_sdi$location_name)
ggplot(cod_sdi[location_id==354 & sex_id==1],
	aes(x=year_id,
		y=val,
		color=cause_name)) + 
	geom_line() +
	geom_dl(aes(label=cause_name), method=list(dl.combine("first.points", "last.points"), cex=0.8)) +
	ggtitle(paste0("Causes of under-5 mortality in ", "loc_name", " for ", "sex")) +
	labs(x="Year",
		y="Mortality rate (per 100,000)") +
	theme(legend.position="bottom",
		legend.direction="vertical",
		plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
	scale_color_discrete(name="Cause name") +
	guides(col=guide_legend(nrow=12,
		byrow=TRUE,
		override.aes=list(size=4)))
ggsave("/home/j/temp/eeldren/mchs/test2.pdf", width=25, height=25)

# Line graph of year vs. mortality rate by cause for all provinces
loc.name <- unique(cod_sdi$location_name)
ggplot(cod_sdi[cause_id==322 & sex_id==1],
	aes(x=year_id,
		y=val,
		color=location_name)) + 
	geom_line() +
	geom_dl(aes(label=location_name), method=list(dl.combine("first.points", "last.points"), cex=0.8)) +
	ggtitle(paste0("Causes of under-5 mortality for ", "cause_name", " for ", "sex")) +
	labs(x="Year",
		y="Mortality rate (per 100,000)") +
	theme(legend.position="bottom",
		legend.direction="vertical",
		plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
	scale_color_discrete(name="Province") +
	guides(col=guide_legend(nrow=12,
		byrow=TRUE,
		override.aes=list(size=4)))
ggsave("/home/j/temp/eeldren/mchs/test3.pdf", width=25, height=25)
