######################################################################
## Maya Fraser, updated by Erika Eldrenkamp
## 4/7/16, updated 8/7/17
## Create maps of China mortality rates for 5q0 causes by province
######################################################################

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
library(stringr)

## Setup environment
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
    cause <- args[1]
} else {
    cause <- 302
}

## Source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))


## CODE ##############################################################
## Read in shape files 
provinces <- readShapeSpatial(paste0(root, "/DATA/SHAPE_FILES/GBD_geographies/selected_subnationals/CHN/ADM1/GIS/CHN_adm1.shp"))
provinces <- fortify(provinces, region="location_i")
provinces$id <- as.integer(provinces$id)

## Get location metadata to merge onto data
loc.table <- get_location_metadata(location_set_id = 22)

## Generate list of relevant provinces
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)
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
lbirths <- subset(lbirths, select=c(sex_id, location_id, location_name, year_id, mean_value))
setnames(lbirths, "mean_value", "live_births")
deaths <- subset(deaths, select=-c(measure_id, metric_id, age_group_id, age_group_name, expected, measure_name, metric_name))
cod.mr <- merge(lbirths, deaths, by=c("location_id", "location_name", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]

## Subset to get selected causes
# Get cause_ids for selected causes
cause_id_sub <- cod.mr[grepl("encephalopathy|Lower|Congenital birth|Drowning|Neonatal preterm", cod.mr$cause_name), ]
#cause_id_sub <- cod.mr[grepl("encephalopathy", cod.mr$cause_name), ]

# Subsetting
cod.mr <- cod.mr[which(cod.mr$cause_id %in% cause_id_sub$cause_id)]

## Merge onto location metadata
cod.mr <- merge(cod.mr, locations, by="location_id")
setnames(cod.mr, "location_id", "id")

## Merge onto shapefile
data <- merge(provinces, cod.mr, by=c("id"), all.x=T)


## GRAPHING #########################################################
## Set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

## Set desired causes
causes <- unique(cod.mr$cause_name)

## Generate maps
# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_maps.pdf",sep=""),width=9.6,height=6.4)

# Looping through years and sexes
for(cause in causes) {
    for(year in years) {
        for(sex in 1:3) {
            data_temp <- data[data$sex_id == sex & data$year_id == year,]
            sex.name <- ifelse(sex==1, "Males", ifelse(sex==2, "Females", "Both"))
            ylim <- range(data_temp$val, na.rm=TRUE)
            # Order the variables to make sure the graphing doesn't get messed up
            data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
            title = paste("Mortality due to", cause, "in", year, "for Children Under 5,", 
                          sex.name, sep=" ")
            #ltitle = paste("Mortality rate \n (per 100,000 live births")
            
            print(ggplot(data_temp) +
                    geom_polygon(aes(x=long, y=lat, group=group, fill=val)) +
                    scale_fill_gradientn(colours=colors, limits=ylim)  + 
                    geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
                    scale_x_continuous("", breaks=NULL) + 
                    scale_y_continuous("", breaks=NULL) + 
                    coord_fixed(ratio=1) + 
                    guides(fill=guide_colourbar(title="Mortality rate \n (per 100,000 \n live births)", barheight=10)) + 
                    theme_bw(base_size=10) +  
                    labs(title=str_wrap(paste(title, sep=""), width=85)))  
        }
    }
}
dev.off()