## Stacked bar of global child mortality by cause
## Author: Joe Mikesell (Stata); re-written in R by Erika Eldrenkamp
## Date: 04/14/2016; modified 08/09/2017

## SETUP ############################################################
## Bring in libraries
rm(list=ls())
library(foreign)
library(data.table)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(Rcpp)
#library("Rcpp", lib.loc="/snfs2/HOME/eeldren/R/x86_64-unknown-linux-gnu-library/3.1")
library(ggplot2)
library(plyr)
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

## Get top 10 causes and "all other causes" for each year/sex combo
# Keep top 10 causes for mainland China for each year/sex combo
dtemp.dt <- deaths.dt[location_id==44533]
dtemp15.dt <- dtemp.dt[order(year_id, sex_id, -val), .SD[1:15], by=c("year_id", "sex_id")]
sexes <- unique(dtemp15.dt$sex_id)
cod.dt <- NULL

for(year in years){
	for(sx in sexes) {
		cause.list <- dtemp15.dt[year_id==year & sex_id==sx, cause_id]
		ysdeaths.dt <- deaths.dt[year_id==year & sex_id==sx]

		top15.dt <- subset(ysdeaths.dt, cause_id %in% cause.list)
		remain.dt <- subset(ysdeaths.dt, !(cause_id %in% cause.list), select=-c(cause_id, cause_name))
		remain.dt[, cause_id:=999]
		remain.dt[, cause_name:="All other causes"]
		remain.dt <- remain.dt[, lapply(.SD, sum, na.rm=TRUE),
			by = .(sex_id, sex, year_id, location_id, location_name, cause_id, cause_name), .SDcols="val"]
		top15rem.dt <- rbind(top15.dt, remain.dt, fill=TRUE)
		cod.dt <- rbind(cod.dt, top15rem.dt)
	}
}

cause_list <- c("Congenital birth defects", "Neonatal preterm birth complications", "Neonatal encephalopathy due to birth asphyxia and trauma",
	"Lower respiratory infections", "Other neonatal disorders", "Drowning", "Road injuries", "Exposure to mechanical forces", "Leukemia",
	"Neonatal sepsis and other neonatal infections", "All other causes")

cod.dt <- subset(cod.dt, cause_name %in% cause_list)

# Read in live births
lbirths.dt <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=c(prov.list, 44533))

# Merge together and calculate rates
lbirths.dt <- subset(lbirths.dt, select=c(sex_id, location_id, year_id, mean_value))
setnames(lbirths.dt, "mean_value", "live_births")
cod.mr <- merge(lbirths.dt, cod.dt, by=c("location_id", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]


## GRAPHING #########################################################
# Generating summed mortality rate with which to order provinces
cod <- cod.mr
cod[, mr_sum := sum(rate), by=c("location_id", "year_id", "sex")]

# Set desired sexes
sexes <- unique(cod$sex)

# Generating color palette
pal11 <- c("#1a8be9",
"#7cc3e9",
"#ec0591",
"#ff80ce",
"#25a311",
"#8dca84",
"#ec9d00",
"#fad282",
"#9107df",
"#cb9ce6",
"#c8c8c8")

# # Actually plot
# pdf(paste0(root, "temp/", user, "/mchs/china_cod_sbar_top10.pdf",sep=""),width=12,height=8)

# for(year in years){
# 	for(sx in sexes) {
# 		# Set order of dataset so bars are in correct order
# 		yscod <- cod[year_id==year & sex==sx]
# 		mlcauses <- dtemp10.dt[year_id==year & sex==sx]
# 		setorder(mlcauses, -val)
# 		mlcname <- subset(mlcauses, select=cause_name)
# 		mlcname[, values:=1:10]
# 		yscod$cause_order <- mlcname$values[match(yscod$cause_name, mlcname$cause_name)]

# 		yscod <- yscod[with(yscod, order(desc(mr_sum), cause_order))]
# 		ysorder <- setorder(yscod, location_id, cause_order, na.last=TRUE)
# 		ysorder$cause_name <- factor(ysorder$cause_name)
# 		ysorder$cause_name <- reorder(ysorder$cause_name, ysorder$cause_order)

# 		ysorder$location_name[yscod$location_name == "China (without Hong Kong and Macao)"] <- "China without \n Hong Kong and Macao"


# 		title=paste("Under-5 mortality by province for", year, ",", sx, sep=" ")

# 		print(ggplot(data=ysorder,
# 			aes(reorder(location_name, mr_sum), rate)) +
# 			geom_bar(width=0.85,
# 				stat="identity",
# 				position=position_fill(reverse=TRUE),
# 				aes(fill=cause_name)) +
# 			scale_fill_manual(values=pal11) +
# 			coord_flip() +
# 			theme(plot.title=element_text(hjust=0.5)) +
# 			labs(title=paste(title, sep=""),
# 				y="Proportion of total mortality burden due to cause",
# 				x="Province (in order of all-cause mortality rate, high to low)",
# 				fill="Cause of death"))
# 	}
# }

# dev.off()


## WRAPPING SBAR FOR 2016 BY SEX
# Set order of dataset so bars are in correct order
index <- c("Congenital birth defects", "Neonatal preterm birth complications", "Neonatal encephalopathy due to birth asphyxia and trauma",
	"Lower respiratory infections", "Other neonatal disorders", "Drowning", "Road injuries", "Exposure to mechanical forces", "Leukemia",
	"Neonatal sepsis and other neonatal infections", "All other causes")
values <- c(1, 2, 3, 4, 5)
cod$cause_order <- values[match(cod$cause_name, index)]

cod <- cod[with(cod, order(desc(mr_sum), cause_order))]
cod$cause_name <- factor(cod$cause_name, levels=c("Congenital birth defects", "Neonatal preterm birth complications",
	"Neonatal encephalopathy due to birth asphyxia and trauma", "Lower respiratory infections", "Other neonatal disorders", "Drowning",
	"Road injuries", "Exposure to mechanical forces", "Leukemia", "Neonatal sepsis and other neonatal infections", "All other causes"))

# Making mainland China name shorter to fit in figure
cod$location_name[cod$location_name == "China (without Hong Kong and Macao)"] <- "China without \n Hong Kong and Macao"

# Setting province order to be same as both sexes 2016
cod_order <- cod[year_id==2016 & sex_id==3]
cod_order[, mr_order:=.GRP, by=location_name]
cod_order <- subset(cod_order[, head(.SD, 1), by=location_name], select=c(location_name, mr_order))
cod_pord <- cod[cod_order, on="location_name"]

mf <- c("Males", "Females")

# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_sbar_top10_2016.pdf",sep=""),width=18,height=8)

#for(sx in mf) {
	#cod_temp <- cod_pord[year_id==2016 & sex==sx]
	cod_temp <- cod_pord[year_id==2016 & sex_id!=3]
	title=paste("Under-5 mortality by province for", year, ",", sx, sep=" ")

	print(ggplot(data=cod_temp,
		aes(reorder(location_name, mr_order), rate)) +
		geom_bar(width=0.85,
			stat="identity",
			position=position_fill(reverse=TRUE),
			aes(fill=cause_name)) +
		facet_wrap(~sex) +
		scale_fill_manual(values=pal11) +
		coord_flip() +
		theme(plot.title=element_text(hjust=0.5)) +
		labs(title=paste(title, sep=""),
			y="Proportion of total mortality burden due to cause",
			x="Province (in order of all-cause mortality rate, high to low)",
			fill="Cause of death"))
#}

dev.off()
