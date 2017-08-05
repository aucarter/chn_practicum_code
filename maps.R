############################################
## Maya Fraser
## 4/7/16
## create maps of china province life expectancy for all provinces
############################################


##################
## Set up R
##################

rm(list=ls())
library(foreign); library(plyr); library(data.table); library(lattice); library(latticeExtra); library(RColorBrewer);library(ggplot2)
library(maptools) ; library(RMySQL)
root <- ifelse(Sys.info()[1]=="Windows","J:","/home/j")

# read in shape files 
provinces <- readShapeSpatial(paste0(root, "/DATA/SHAPE_FILES/GBD_geographies/selected_subnationals/CHN/ADM1/GIS/CHN_adm1.shp"))
provinces <- fortify(provinces, region="location_i")
provinces$id <- as.integer(provinces$id)

# read in location file to map id's to LE data
source(paste0(root, "/Project/Mortality/shared/functions/get_locations.r"))
locations <- get_locations()
locations <- locations[,c("ihme_loc_id", "location_name", "location_id", "local_id_2013")]
locations <- rename(locations, c("local_id_2013" = "iso3"))

data <- fread(paste0(root, "/temp/aucarter/le_decomp/le_diff.csv"))

# merge locations and data
data <- merge(data, locations, by=c("location_id"))
data <- rename(data, c("location_id" = "id"))
province_list <- unique(provinces$id)
data <- data[data$id %in% province_list,]

# merge provinces and data
data <- merge(provinces, data, by=c("id"), all.x=T)


################################################
### print the graphs
################################################


# set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

# actually plot
pdf(paste0(root, "/temp/aucarter/le_decomp/China_LE_diff_maps.pdf",sep=""),width=12,height=8)

### loop through the years

for(sex in 1:2){
    data_temp <- data[data$sex_id==sex,]
    sex.name <- ifelse(sex == 1, "Males", "Females")
    ylim <- range(data_temp$diff, na.rm=TRUE)
    ylim <- c(floor(ylim[1]), ceiling(ylim[2]))
    # order the variables to make sure the graphing doesn't get messed up
    data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
    
    title = paste("Change in life expectancy between 1990 and 2016 in the Chinese provinces,", 
                  sex.name, sep=" ")  
    
    print(ggplot(data_temp) +
            geom_polygon(aes(x=long, y=lat, group=group, fill=diff)) +
            scale_fill_gradientn(colours=colors, limits=ylim)  + 
            geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
            scale_x_continuous("", breaks=NULL) + 
            scale_y_continuous("", breaks=NULL) + 
            coord_fixed(ratio=1) + 
            guides(fill=guide_colourbar(title="Deaths/100,000 Live Births", barheight=10)) + 
            theme_bw(base_size=10) +  
            labs(title=paste(title, sep="")))
    
  }
dev.off()



