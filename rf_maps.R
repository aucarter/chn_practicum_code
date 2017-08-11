############################################
## Maya Fraser
## 4/7/16
## create maps of china province risk factors for all provinces
############################################


##################
## Set up R
##################

rm(list=ls())
library(foreign); library(plyr); library(data.table); library(lattice); library(latticeExtra); library(RColorBrewer);library(ggplot2)
library(maptools) ; library(RMySQL)
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

#source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))

# read in shape files 
provinces <- readShapeSpatial(paste0(root, "/DATA/SHAPE_FILES/GBD_geographies/selected_subnationals/CHN/ADM1/GIS/CHN_adm1.shp"))
provinces <- fortify(provinces, region="location_i")
provinces$id <- as.integer(provinces$id)

# Get location metadata to merge onto data
locations <- get_location_metadata(location_set_id = 22)
locations <- locations[,c("ihme_loc_id", "location_id", "parent_id")]

#Generate list of relevant provinces
prov.list <- setdiff(c(locations[parent_id == 6, location_id], locations[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)

#Pull burden data
years <- c(1990, 1995, 2000, 2005, 2010, 2016)
all.mr <- get_outputs(topic = "rei", location_id = prov.list, year_id = years, measure_id = 1, metric_id = 3, 
    age_group_id = 1, sex_id = 3, version = "latest")
all.mr[, rate := val * 100000]

# merge onto location metadata
all.mr <- merge(all.mr, locations, by="location_id")
setnames(all.mr, "location_id", "id")

#Merge onto shapefile
data <- merge(provinces, all.mr, by=c("id"), all.x=T)

################################################
### print the graphs
################################################


# set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

# actually plot
pdf(paste0(root, "temp/", user, "/chn/China_maps_rate_abrev.pdf",sep=""),width=12,height=8)

### loop through the years
for(year in c(1990, 2000, 2016)) {
    #for(sex in 1:2){
        data_temp <- data[data$year_id == year,]
       #sex.name <- ifelse(sex == 1, "Males", "Females")
        ylim <- range(data_temp$rate, na.rm=TRUE)
        ylim <- c(floor(ylim[1]), ceiling(ylim[2]))
        # order the variables to make sure the graphing doesn't get messed up
        data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
        title = paste("Attributable Mortality rate due to all risk factors in", year,"for Children Under 5,", sep=" ")
        
        print(ggplot(data_temp) +
                geom_polygon(aes(x=long, y=lat, group=group, fill=rate)) +
                scale_fill_gradientn(colours=colors, limits=ylim)  + 
                geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
                scale_x_continuous("", breaks=NULL) + 
                scale_y_continuous("", breaks=NULL) + 
                coord_fixed(ratio=1) + 
                guides(fill=guide_colourbar(title="Death/100,000", barheight=10)) + 
                theme_bw(base_size=10) +  
                labs(title=paste(title, sep="")))   
  #}
}
dev.off()
