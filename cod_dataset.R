## Tables of underlying dataset for COD figures
## Author: Erika Eldrenkamp
## Date: 08/15/2017


## SETUP ############################################################
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

### DATASET FOR MAPS AND STACKED BAR
## Generate China mainland estimates (w/o HK & Macau) for mortality rates per 100,000 live births for top 5 causes and all-cause
# Get outputs of MR due to selected causes at province-level
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
cause_ids <- c(294, 322, 381, 382, 641, 698)
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
cod <- merge(cod.mr, loc.table, by="location_id")

## Cleaning dataset for exporting as a table
cod <- subset(cod, select=-c(sex_id, val, live_births, cause_id, location_id))
cod$rate <- round(cod$rate, 2)

setnames(cod, "year_id", "Year")
setnames(cod, "rate", "Mortality rate")
setnames(cod, "sex", "Sex")
setnames(cod, "cause_name", "Cause name")
setnames(cod, "location_name", "Location name")

write.csv(cod, paste0(root, "temp/", user, "/mchs/cod_data_province_top5.csv",sep=""), row.names=FALSE)


### DATASET FOR ARROW FIGURES