## Stacked bar of global child mortality by cause
## Author: Joe Mikesell (Stata); re-written in R by Erika Eldrenkamp
## Date: 04/14/2016; modified 08/09/2017

## SETUP #########################################################
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

## Get location metadata
loc.table <- get_location_metadata(location_set_id = 22)

## Generate list of relevant provinces
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)
prov.list[32] <- 6 # China national
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

## Create mortality rate per 100,000 live births by cause
years <- c(1990, 1995, 2000, 2005, 2010, 2016)

# Getting under-5 deaths by cause
deaths <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all",
	measure_id=1, metric_id=1, cause_id="lvl3", version="latest")

# Getting live births
lbirths <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=prov.list)

# Generating mortality rate
lbirths <- subset(lbirths, select=c(location_id, location_name, year_id, mean_value))
setnames(lbirths, "mean_value", "live_births")
deaths <- subset(deaths, select=-c(measure_id, metric_id, age_group_id, age_group_name, expected, measure_name, metric_name))
cod_mr <- merge(lbirths, deaths, by=c("location_id", "year_id", "location_name"), allow.cartesian=TRUE)
cod_mr[, rate:=(val/live_births)*100000]

## Subset to get selected causes
# Get cause_ids for selected causes
cause_id_sub <- cod_mr[grepl("encephalopathy|Lower|Congenital birth|Drowning|Neonatal preterm", cod_mr$cause_name), ]

#cause_id_sub <- cod.mr[grepl("encephalopathy|Lower", cod.mr$cause_name), ]

# Subsetting
cod <- cod_mr[which(cod_mr$cause_id %in% cause_id_sub$cause_id)]

## GRAPHING #########################################################

# Generating total mortality rate with which to order provinces
cod[, mr_sum := sum(rate), by=c("location_id", "year_id", "sex")]

#cod$cause_name <- with(cod, reorder(cause_name, mr_sum))
#cod$cause_name <- factor(cod$cause_name, levels=cod$val)
#cod <- cod[year_id==2000 & sex=="Female"]
#cod <- cod[with(cod, order(desc(mr_sum), desc(val), cause_name))]

# Generating color palette
# pal15 <- c("#72163b", "#e8995d", "#8752ff", "#7b0810", "#8e4d00", "#cba64d", "#5d9600", "#b6af21",
# 	"#245f00", "#006739", "#019eea", "#024fd1", "#ff7f9e", "#ed82f0", "#db00a0")

# Set desired sexes
sexes <- unique(cod$sex)
#cod$cause_name <- factor(cod$cause_name, levels=cod$cause_name)

# Generate PDF plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_sbar.pdf",sep=""),width=12,height=8)

# Looping through years and sexes
for(year in years) {
	for(sx in sexes) {
		cod_temp <- cod[year_id==year & sex==sx]
		cod_temp <- cod_temp[with(cod_temp, order(desc(mr_sum), desc(val), cause_name))]
		cod_temp$cause_name <- factor(cod_temp$cause_name, levels=cod_temp$cause_name)
		title=paste("Under-5 mortality by province for", year, ",", sx, sep=" ")

		print(ggplot(data=cod_temp, 
			aes(reorder(location_name, mr_sum), rate)) +
			#aes(location_name, rate)) +
			geom_bar(width=0.85,
				stat="identity",
				position=position_fill(),
				aes(fill=cause_name)) +
			coord_flip() +
			#scale_fill_discrete(breaks=rev(levels(cod$val))) +
			#guides(fill=guide_legend(reverse=TRUE)) +
			#scale_fill_manual(values=pal15) +
			labs(title=paste(title, sep=""),
				y="Mortality rate (per 100,000)",
				x="Province",
				fill="Cause of death"))
	}
}
dev.off()


