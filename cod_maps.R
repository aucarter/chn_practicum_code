######################################################################
## Maya Fraser, updated by Erika Eldrenkamp
## 4/7/16, updated 8/7/17
## Create maps of China mortality rates for 5q0 causes by province
######################################################################

## SETUP ############################################################
## Bring in libraries
rm(list=ls())
library(foreign)
library(data.table)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(Rcpp)
library(ggplot2)
library(plyr)
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
prov.list <- loc.table[parent_id == 44533, location_id]
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

## Create mortality rate per 100,000 live births by cause
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
cause_ids <- c(294, 322, 381, 382, 641, 698)
#cause_ids <- 322

# Getting under-5 deaths by cause
deaths <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all",
    measure_id=1, metric_id=1, cause_id=cause_ids, version="latest")

# Getting live births
lbirths <- get_covariate_estimates(covariate_name_short="live_births_by_sex", location_id=prov.list)

# Generating mortality rate
lbirths <- subset(lbirths, select=c(sex_id, location_id, location_name, year_id, mean_value))
setnames(lbirths, "mean_value", "live_births")
deaths <- subset(deaths, select=-c(measure_id, metric_id, age_group_id, age_group_name, expected, measure_name, metric_name))
cod.mr <- merge(lbirths, deaths, by=c("location_id", "location_name", "year_id", "sex_id"))
cod.mr[, rate:=(val/live_births)*100000]

# Read in location file to map ids to MR data
source(paste0(root, "/Project/Mortality/shared/functions/get_locations.r"))
locations <- get_locations()
locations <- locations[,c("ihme_loc_id", "location_name", "location_id", "local_id_2013")]
locations <- rename(locations, c("local_id_2013" = "iso3"))

## Merge location and data
cod.mr <- merge(cod.mr, locations, by=c("location_id", "location_name"))
setnames(cod.mr, "location_id", "id")

# Generate AROC values, 1990-2016
laroc <- cod.mr[year_id==1990|year_id==2016]
waroc <- dcast(laroc, id + location_name + sex_id + sex + cause_id + cause_name + ihme_loc_id + iso3 ~ year_id, value.var="rate")
setnames(waroc, "1990", "year_1990")
setnames(waroc, "2016", "year_2016")
waroc[, aroc:=(((waroc$year_2016/waroc$year_1990)^(1/26))-1)*100]

## Merge onto shapefile
data_yy <- merge(provinces, cod.mr, by=c("id"), all.x=T)
data_aroc <- merge(provinces, waroc, by=c("id"), all.x=T)


## GRAPHING #########################################################
## Set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

## Generate maps for year-by-year
# Actually plot
#pdf(paste0(root, "temp/", user, "/mchs/china_cod_maps_actop5yy.pdf",sep=""),width=9.6,height=6.4)

# Looping through years and sexes
# for(cause in causes) {
#     for(year in years) {
#         for(sex in 1:3) {
#             data_temp <- data_yy[data_yy$sex_id==sex & data_yy$year_id==year,]
#             sex.name <- ifelse(sex==1, "Males", ifelse(sex==2, "Females", "Both"))
#             ylim <- range(data_temp$rate, na.rm=TRUE)
#             # Order the variables to make sure the graphing doesn't get messed up
#             data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
#             title = paste("Mortality due to", cause, "in", year, "for Children Under 5,", 
#                           sex.name, sep=" ")
#             #ltitle = paste("Mortality rate \n (per 100,000 live births")
            
#             print(ggplot(data_temp) +
#                     geom_polygon(aes(x=long, y=lat, group=group, fill=rate)) +
#                     scale_fill_gradientn(colours=colors, limits=ylim)  + 
#                     geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
#                     scale_x_continuous("", breaks=NULL) + 
#                     scale_y_continuous("", breaks=NULL) + 
#                     coord_fixed(ratio=1) + 
#                     guides(fill=guide_colourbar(title="Mortality rate \n (per 100,000 \n live births)", barheight=10)) + 
#                     theme_bw(base_size=10) +
                        # theme(plot.title=element_text(hjust=0.5)) +  
#                     labs(title=str_wrap(paste(title, sep=""), width=85)))  
#         }
#     }
# }
# dev.off()


# ## Generate maps for AROC
# ## Set desired causes
# causes <- unique(cod.mr$cause_name)

# # Actually plot
# pdf(paste0(root, "temp/", user, "/mchs/china_cod_maps_actop5_aroc.pdf",sep=""),width=9.6,height=6.4)

# # Looping through years and sexes
# for(cause in causes) {
#     for(sex in 1:3) {
#         data_temp <- data_aroc[data_aroc$sex_id == sex & !is.na(data_aroc$cause_name),]
#         sex.name <- ifelse(sex==1, "Males", ifelse(sex==2, "Females", "Both"))
#         ylim <- range(data_temp$aroc, na.rm=TRUE)
#         # Order the variables to make sure the graphing doesn't get messed up
#         data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
#         title = paste("Annualized percent change in mortality due to", cause, "from 1990-2016 for Children Under 5,", 
#                       sex.name, sep=" ")
        
#         print(ggplot(data_temp) +
#                 geom_polygon(aes(x=long, y=lat, group=group, fill=aroc)) +
#                 scale_fill_gradientn(colours=colors, limits=ylim)  + 
#                 geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
#                 scale_x_continuous("", breaks=NULL) + 
#                 scale_y_continuous("", breaks=NULL) + 
#                 coord_fixed(ratio=1) +
#                 guides(fill=guide_colourbar(title="Annualized percent \n change in \n mortality rate \n (per 100,000 \n live births)", barheight=10)) + 
#                 theme_bw(base_size=10) + 
#                 theme(plot.title=element_text(hjust=0.5)) +
#                 labs(title=str_wrap(paste(title, sep=""), width=85)))
#     }
# }
# dev.off()


## Generate facet-wrapped maps for AROC
# Line break for long cause name
data_aroc$cause_name[data_aroc$cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma"] <- "Neonatal encephalopathy due to \n birth asphyxia and trauma"

## Set desired causes
causes <- unique(data_aroc$cause_name)

# Actually plot for both sexes
pdf(paste0(root, "temp/", user, "/mchs/china_cod_maps_actop5_aroc_bs9016.pdf",sep=""),width=9.6,height=6.4)

data_temp <- data_aroc[data_aroc$sex_id == 3 & !is.na(data_aroc$cause_name),]
ylim <- range(data_temp$aroc, na.rm=TRUE)
# Order the variables to make sure the graphing doesn't get messed up
data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
title = paste("Annualized percent change in mortality from 1990-2016 \n for Children Under 5, both sexes", sep=" ")

print(ggplot(data_temp) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=aroc)) +
        scale_fill_gradientn(colours=colors, limits=ylim)  + 
        geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
        scale_x_continuous("", breaks=NULL) + 
        scale_y_continuous("", breaks=NULL) + 
        coord_fixed(ratio=1) + 
        facet_wrap(~cause_name) +
        guides(fill=guide_colourbar(title="Annualized percent change \n in mortality rate \n (per 100,000 live births)",
            barheight=1.75, barwidth=18)) + 
        theme_bw(base_size=10) + 
        theme(plot.title=element_text(hjust=0.5),
            legend.position="bottom") +
        labs(title=str_wrap(paste(title, sep=""), width=100)))

dev.off()