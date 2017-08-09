######################################################################
## Maya Fraser, updated by Erika Eldrenkamp
## 4/7/16, updated 8/7/17
## Create maps of China mortality rates for 5q0 causes by province
######################################################################

##################
## Setup
##################

# Bring in libraries
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

# Set-up environment
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

# Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
    cause <- args[1]
} else {
    cause <- 302
}

# Source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))

# Read in shape files 
provinces <- readShapeSpatial(paste0(root, "/DATA/SHAPE_FILES/GBD_geographies/selected_subnationals/CHN/ADM1/GIS/CHN_adm1.shp"))
provinces <- fortify(provinces, region="location_i")
provinces$id <- as.integer(provinces$id)

# Get location metadata to merge onto data
loc.table <- get_location_metadata(location_set_id = 22)

# Generate list of relevant provinces
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

# Pull burden data
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
cod.mr <- get_outputs(topic="cause", location_id=prov.list, year_id=years, age_group_id=1, sex_id="all", measure_id=1,
    metric_id=3, cause_id="lvl4", version="latest")

cod.mr[, rate := val * 100000]

# Get cause_ids for selected causes
# cause_id_sub <- cod.mr[grepl("encephalopathy|Lower|complications|Congenital heart|Drowning|Other neonatal|
#     |Pedestrian|Unintentional suffocation|Digestive congenital|Other congenital|Neonatal sepsis|
#     |Diarrheal disease|Whooping|Neural tube|Protein-energy",
#     cod.mr$cause_name), ]

cause_id_sub <- cod.mr[grepl("encephalopathy|Lower", cod.mr$cause_name), ]

# Subset COD estimates to get selected causes
cod.mr <- cod.mr[which(cod.mr$cause_id %in% cause_id_sub$cause_id)]

# Merge onto location metadata
cod.mr <- merge(cod.mr, locations, by="location_id")
setnames(cod.mr, "location_id", "id")

# Merge onto shapefile
data <- merge(provinces, cod.mr, by=c("id"), all.x=T)

################################################
### Generate the maps
################################################

# Set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

# Set desired causes
causes <- unique(cod.mr$cause_name)

# Actually plot
pdf(paste0(root, "temp/", user, "/mchs/china_cod_maps.pdf",sep=""),width=7.5,height=5)

### Loop through years
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
            
            print(ggplot(data_temp) +
                    geom_polygon(aes(x=long, y=lat, group=group, fill=val)) +
                    scale_fill_gradientn(colours=colors, limits=ylim)  + 
                    geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
                    scale_x_continuous("", breaks=NULL) + 
                    scale_y_continuous("", breaks=NULL) + 
                    coord_fixed(ratio=1) + 
                    guides(fill=guide_colourbar(title="Mortality Rate", barheight=10)) + 
                    theme_bw(base_size=10) +  
                    labs(title=paste(title, sep="")))   
        }
    }
}
dev.off()