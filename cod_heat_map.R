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
library(Rcpp)
#library("Rcpp", lib.loc="/snfs2/HOME/eeldren/R/x86_64-unknown-linux-gnu-library/3.1")
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
prov.list <- loc.table[parent_id == 44533, location_id]
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

### Generate China mainland estimates (w/o HK & Macau) for mortality rates per 100,000 live births
## Sum province-level outputs to create China mainland output
years <- c(1990, 1995, 2000, 2005, 2010, 2016)

# Get outputs of MR due to all causes at province-level
mort.dt <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all",
	measure_id=1, metric_id=1, cause_id="lvl3", version="latest")
mort.dt[sex=="Male", sex:="Males"]
mort.dt[sex=="Female", sex:="Females"]
mort.dt[sex=="Both", sex:="Both sexes"]
mainland.dt <- copy(mort.dt)
mainland.dt <- mainland.dt[, lapply(.SD, sum), by = .(year_id, sex_id, sex, cause_id, cause_name), .SDcols = "val"]
mainland.dt[, location_id:=44533]
mainland.dt[, location_name:="China (without Hong Kong and Macao)"]
deaths.dt <- rbind(mort.dt[, .(location_id, location_name, year_id, sex_id, sex, cause_id, cause_name, val)], mainland.dt)

## Get top 15 causes for each year/sex combo
# Keep top 15 causes for mainland China for each year/sex combo
dtemp.dt <- deaths.dt[location_id==44533]
dtemp15.dt <- dtemp.dt[order(year_id, sex_id, -val), .SD[1:15], by=c("year_id", "sex_id")]
sexes <- unique(dtemp15.dt$sex_id)

# Keep top 15 causes (according to mainland China rates) for each province
cod.dt <- NULL
for(year in years){
	for(sx in sexes) {
		cause.list <- dtemp15.dt[year_id==year & sex_id==sx, cause_id]
		ysdeaths.dt <- deaths.dt[year_id==year & sex_id==sx]

		top15.dt <- subset(ysdeaths.dt, cause_id %in% cause.list)
		cod.dt <- rbind(cod.dt, top15.dt)
	}
}

# Read in live births
lbirths.dt <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=c(prov.list, 44533))

# Merge together and calculate rates
lbirths.dt <- subset(lbirths.dt, select=c(sex_id, location_id, year_id, mean_value))
setnames(lbirths.dt, "mean_value", "live_births")
cod.mr <- merge(lbirths.dt, cod.dt, by=c("location_id", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]


## GRAPHING #########################################################
## Convential heat map
# Rank causes for each province to be used for heat map color fill
rcod <- setorder(cod.mr, year_id, sex_id, location_id, -rate)
n <- length(rcod$cause_name)/15
rcod[, values:=rep(1:15, times=n)]

# Sum mortality rates to set province order
rcod[, mr_sum := sum(rate), by=c("location_id", "year_id", "sex_id")]

# Set desired sexes
sexes <- unique(rcod$sex)

# Shortening names
rcod$location_name[rcod$location_name == "China (without Hong Kong and Macao)"] <- "China without \n Hong Kong and Macao"
rcod$cause_name[rcod$cause_name == "Hemolytic disease and other neonatal jaundice"] <- "Hemolytic disease and \n other neonatal jaundice"
rcod$cause_name[rcod$cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma"] <- "Neonatal encephalopathy \n due to birth asphyxia \n and trauma"
rcod$cause_name[rcod$cause_name == "Exposure to mechanical forces"] <- "Exposure to mechanical \n forces"
rcod$cause_name[rcod$cause_name == "Neonatal preterm birth complications"] <- "Neonatal preterm birth \n complications"
rcod$cause_name[rcod$cause_name == "Neonatal sepsis and other neonatal infections"] <- "Neonatal sepsis and \n other neonatal infections"
rcod$cause_name[rcod$cause_name == "Other cardiovascular and circulatory diseases"] <- "Other cardiovascular and \n circulatory diseases"
rcod$cause_name[rcod$cause_name == "Paralytic ileus and intestinal obstruction"] <- "Paralytic ileus and intestinal \n obstruction"

# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_hmap_top15.pdf",sep=""),width=15,height=9)

for(year in years) {
	for(sx in sexes) {
		yscod <- rcod[year_id==year & sex==sx]
		title=paste("Top 15 under-5 causes of death by province (mortality rate per 100,000 live births) for", year, ",", sx, sep=" ")

		print(ggplot(yscod, aes(reorder(location_name, -mr_sum), reorder(cause_name, rate))) +
			geom_tile(aes(fill=-values)) +
			geom_text(aes(label=round(rate, 0))) +
			scale_fill_gradient(low="white", high="red", guide=FALSE) +
			labs(title=paste(title, sep=""),
				x="Province (in order of all-cause mortality rate, high to low)",
				y="Cause name") +
			theme(axis.text.x.top=element_text(angle=45, vjust=0, hjust=0),
				plot.margin=unit(c(1,1,1,1), "cm"),
				plot.title=element_text(hjust=0.5)) +
			scale_x_discrete(position="top"))
	}
}

dev.off()



### TEST MAP WITH ONLY ONE SEX-YEAR
pdf(paste0(root, "temp/", user, "/mchs/china_cod_hmap_top15_TEST.pdf",sep=""),width=12,height=8)
test <- rcod[year_id==1990&sex_id==1]
print(ggplot(test, aes(reorder(location_name, mr_sum), reorder(cause_name, -rate))) +
	geom_tile(aes(fill=-values)) +
	geom_text(aes(label=round(rate, 0))) +
	scale_fill_gradient(low="white", high="red") +
	labs(title="Top 15 under-5 causes of death by province (mortality rate per 100,000 live births)",
		x="Province (in order of all-cause mortality rate, high to low)",
		y="Cause name") +
	coord_flip() +
	theme(axis.text.x.top=element_text(angle=45, vjust=0, hjust=0)) +
	scale_y_discrete(position="top"))
dev.off()
