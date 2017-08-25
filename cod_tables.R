## Table of mortality causes for mainland China and provinces
## Author: Erika Eldrenkamp
## Date: 08/24/2017

## SETUP ############################################################
## Bring in libraries
rm(list=ls())
library(foreign)
library(data.table)
library(lattice)
library(latticeExtra)
library(plyr)

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
prov.list <- loc.table[parent_id == 44533, location_id]
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

### Generate China mainland estimates (w/o HK & Macau) for mortality rates per 100,000 live births
## Sum province-level outputs to create China mainland output
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
cause.list <- c(294, 322, 381, 382, 385, 641, 689, 698, 704, 487, 383)

# Get outputs of mortality rate at province-level
mort.dt <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all",
	measure_id=1, metric_id=1, cause_id=cause.list, version="latest")
mort.dt[sex=="Male", sex:="Males"]
mort.dt[sex=="Female", sex:="Females"]
mort.dt[sex=="Both", sex:="Both sexes"]
mainland.dt <- copy(mort.dt)
mainland.dt <- mainland.dt[, lapply(.SD, sum), by = .(year_id, sex_id, sex, cause_id, cause_name), .SDcols = "val"]
mainland.dt[, location_id:=44533]
mainland.dt[, location_name:="China (without Hong Kong and Macao)"]
deaths.dt <- rbind(mort.dt[, .(location_id, location_name, year_id, sex_id, sex, cause_id, cause_name, val)], mainland.dt)

# Read in live births
lbirths.dt <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=c(prov.list, 44533))

# Merge together and calculate rates
lbirths.dt <- subset(lbirths.dt, select=c(sex_id, location_id, year_id, mean_value))
setnames(lbirths.dt, "mean_value", "live_births")
cod.mr <- merge(lbirths.dt, deaths.dt, by=c("location_id", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]

### ALL-CAUSE MORTALITY TABLE
# Calculate percent change and AROC, 1990-2016
cod.acmr <- cod.mr[(year_id==1990|year_id==2016) & cause_id==294]
cod.acmr$rate <- round(cod.acmr$rate, 2)
cod.acmr <- dcast(cod.acmr, location_id + location_name + sex_id + sex + cause_id + cause_name ~ year_id, value.var="rate")
setnames(cod.acmr, "1990", "year_1990")
setnames(cod.acmr, "2016", "year_2016")
cod.acmr[, pch:=round(((cod.acmr$year_2016-cod.acmr$year_1990)/cod.acmr$year_1990)*100, 2)]
cod.acmr[, aroc:=round((((cod.acmr$year_2016/cod.acmr$year_1990)^(1/26))-1)*100, 2)]

# Clean data for export as table
cod.acmr <- cod.acmr[sex_id==3]
cod.acmr <- subset(cod.acmr, select=c(location_name, year_1990, year_2016, pch, aroc))

setnames(cod.acmr, "location_name", "Location name")
setnames(cod.acmr, "year_1990", "1990")
setnames(cod.acmr, "year_2016", "2016")
setnames(cod.acmr, "pch", "Percent change, 1990-2016")
setnames(cod.acmr, "aroc", "Annualized rate of change, 1990-2016")

write.csv(cod.acmr, paste0(root, "temp/", user, "/mchs/cod_data_acmr_9016.csv",sep=""), row.names=FALSE)

### CAUSE-SPECIFIC MORTALITY TABLE
# Get causes wide and set cause order
cod.csmr <- cod.mr[year_id==2016 & cause_id!=294]
cod.csmr$rate <- round(cod.csmr$rate, 2)
cod.csmr <- subset(cod.csmr, select=-cause_id)
cod.csmr <- dcast(cod.csmr, location_id + location_name + sex_id + sex + year_id ~ cause_name, value.var="rate")

# Clean data for export as table
cod.csmr <- cod.csmr[sex_id==3]
cod.csmr <- subset(cod.csmr, select=-c(location_id, sex_id, sex))
cod.csmr <- setcolorder(cod.csmr, c("location_name", "year_id", "Congenital birth defects", "Neonatal preterm birth complications",
	"Neonatal encephalopathy due to birth asphyxia and trauma", "Lower respiratory infections", "Drowning", "Other neonatal disorders",
	"Road injuries", "Exposure to mechanical forces", "Neonatal sepsis and other neonatal infections", "Leukemia"))

setnames(cod.csmr, "location_name", "Location name")
setnames(cod.csmr, "year_id", "Year")

write.csv(cod.csmr, paste0(root, "temp/", user, "/mchs/cod_data_csmr_16.csv",sep=""), row.names=FALSE)