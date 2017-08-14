## Stacked bar of global child mortality by cause
## Author: Joe Mikesell (Stata); re-written in R by Erika Eldrenkamp
## Date: 04/14/2016; modified 08/09/2017

## SETUP ############################################################
## Bring in libraries
rm(list=ls())
library(foreign)
library(plyr)
library(data.table)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(RMySQL)

## Setup environment
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

## Source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))


## CODE ##############################################################
## Get location metadata
loc.table <- get_location_metadata(location_set_id = 22)

## Generate list of relevant provinces
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macau
prov.list <- setdiff(prov.list, remove)
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

## Generate China mainland estimates (w/o HK & Macau) for mortality rates per 100,000 live births
# Get outputs of MR due to selected causes at province-level
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
cause_ids <- c(322, 381, 382, 641, 698)
mort.dt <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all",
	measure_id=1, metric_id=1, cause_id=cause_ids, version="latest")
cause.table <- subset(mort.dt, cause_id %in% cause_ids)
cause.table <- subset(cause.table, select=c("cause_id", "cause_name"))
cause.table <- setkey(cause.table, NULL)
cause.table <- unique(cause.table)
loc.table <- subset(loc.table, select=c("location_id", "location_name"))

# Sum province-level outputs to create China mainland output
mainland.dt <- copy(mort.dt)
mainland.dt <- mainland.dt[, lapply(.SD, sum), by = .(year_id, sex_id, sex, cause_id), .SDcols = "val"]
mainland.dt[, location_id := 44533]
deaths.dt <- rbind(mort.dt[, .(location_id, year_id, sex_id, sex, cause_id, val)], mainland.dt)

# Read in live births
lbirths.dt <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=c(prov.list, 44533))

# Merge together and calculate rates
lbirths.dt <- subset(lbirths.dt, select=c(sex_id, location_id, year_id, mean_value))
setnames(lbirths.dt, "mean_value", "live_births")
cod.mr <- merge(lbirths.dt, deaths.dt, by=c("location_id", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]
cod.mr <- merge(cod.mr, cause.table, by="cause_id")
cod.mr <- merge(cod.mr, loc.table, by="location_id")

## Merge with SDI estimates
# Pull SDI at province-level
sdi <- get_covariate_estimates(covariate_id=881, location_id=c(prov.list, 44533))
sdi <- subset(sdi, select=c("location_id", "location_name", "year_id", "mean_value"))
setnames(sdi, old="mean_value", new="sdi")

cod.mr <- merge(cod.mr, sdi, by=c("location_id", "location_name", "year_id"))

## Subset to get selected causes
# Get cause_ids for selected causes
cause_id_sub <- cod.mr[grepl("encephalopathy|Lower|Congenital birth|Drowning|Neonatal preterm", cod.mr$cause_name), ]

# Subsetting
cod <- cod.mr[which(cod.mr$cause_id %in% cause_id_sub$cause_id)]


## GRAPHING #########################################################
# Generating total mortality rate with which to order provinces
cod[, mr_sum := sum(rate), by=c("location_id", "year_id", "sex")]

# Set desired sexes
sexes <- unique(cod$sex)

# Set order of dataset so bars are in correct order
index <- c("Lower respiratory infections", "Neonatal preterm birth complications",
	"Neonatal encephalopathy due to birth asphyxia and trauma", "Congenital birth defects", "Drowning")
values <- c(1, 2, 3, 4, 5)
cod$cause_order <- values[match(cod$cause_name, index)]

cod <- cod[with(cod, order(desc(mr_sum), cause_order))]
cod$cause_name <- factor(cod$cause_name, levels=c("Lower respiratory infections",
	"Neonatal preterm birth complications", "Neonatal encephalopathy due to birth asphyxia and trauma",
	"Congenital birth defects", "Drowning"))

# Making mainland China name shorter to fit in figure
cod$location_name[cod$location_name == "China (without Hong Kong and Macao)"] <- "China without \n Hong Kong and Macao"

## Generating PDF figure for order by MR and order by SDI
# Generate figure with provinces ordered by MR
# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_sbar_mr.pdf",sep=""),width=12,height=8)

# Looping through years and sexes
for(year in years) {
	for(sx in sexes) {
		cod_temp <- cod[year_id==year & sex==sx]
		title=paste("Under-5 mortality by province for", year, ",", sx, sep=" ")

		print(ggplot(data=cod_temp, 
			aes(reorder(location_name, mr_sum), rate)) +
			geom_bar(width=0.85,
				stat="identity",
				position=position_fill(),
				aes(fill=cause_name)) +
			coord_flip() +
			labs(title=paste(title, sep=""),
				y="Mortality rate (per 100,000 live births)",
				x="Province (in order of all-cause mortality rate, high to low)",
				fill="Cause of death"))
	}
}
dev.off()

# Generate figure with provinces ordered by SDI
# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_sbar_sdi.pdf",sep=""),width=12,height=8)

# Looping through years and sexes
for(year in years) {
	for(sx in sexes) {
		cod_temp <- cod[year_id==year & sex==sx]
		title=paste("Under-5 mortality by province for", year, ",", sx, sep=" ")

		print(ggplot(data=cod_temp, 
			aes(reorder(location_name, desc(sdi)), rate)) +
			geom_bar(width=0.85,
				stat="identity",
				position=position_fill(),
				aes(fill=cause_name)) +
			coord_flip() +
			labs(title=paste(title, sep=""),
				y="Mortality rate (per 100,000 live births)",
				x="Province (in order of SDI, low to high)",
				fill="Cause of death"))
	}
}
dev.off()


