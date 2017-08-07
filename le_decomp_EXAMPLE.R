# Austin Schumacher
## 4/17/2014
## Decompose differences in life expectancy by cause using demographic techniques from 
## Beltran-Sanchez, Preston, and Canudas-Romo (2008).

## outputs both files with all locations, an smaller files with all combinations for a starting year (e.g. all decomps beginning in 1990)
##################
## Set up R
##################

rm(list=ls())
library(foreign); library(plyr); library(data.table); library(lattice); library(latticeExtra); library(RColorBrewer);
root <- ifelse(Sys.info()[1]=="Windows","J:","/home/j")

###################
## set directories
###################

dir <- paste(root, "/Project/Mortality/Requests/", sep="")

######################
## DEFINE FUNCTIONS
######################

## ax graduation function
ax_fun <- function(d,nx) {
  shift1 <- c(d[-1],0)
  shift2 <- c(0,d[-length(d)])
  ax <- (((-nx/24)*shift2) + (nx/2*d) + ((nx/24)*shift1))/d
  return(ax)
} ## end ax_fun()


## Life table creation and decomposition of difference in e0 between two year_ids
##  INPUT:
##    A dataframe of mx by ihme_loc_id, year_id, sex_id, age, and cause with the following variables:
##      ihme_loc_id, year_id, sex_id, age, acause (mutually exclusive and collectively exhaustive causes of death including all_cause), mx
##  OUTPUT:
##    A dataframe of decomposition of change in life expectancy by ihme_loc_id, sex_id, age, and cause with the following variables:
##      ihme_loc_id, year_id1, year_id2, sex_id, age, acause (same as input), dcomp (decomposition of the change in e0 between year_id1 and year_id2)
## This takes about 10 minutes, currently
lifetable_decomp <- function(data) {
   
  
  cat(paste("All-cause lifetable\n\n")); flush.console()
  
  # bring in ax from thelife table 
  
    ax<- as.data.frame(fread("/share/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/compiled_summary_lt.csv"))
    ax <- ax[,c("ihme_loc_id", "sex_id", "year", "age_group_id", "ax")]
    ax <- ax[ax$year %in% c(starty, endy),]
    locs <- unique(data$ihme_loc_id)
    ax <- ax[ax$ihme_loc_id %in% locs,]
    # ax <- ax[ax$sex_id != 3,]
    ax <- setnames(ax, old="year", new="year_id")
    #age_crosswalk <- cs[,c("age", "age_group_id")]
    #age_crosswalk <- unique(age_crosswalk)
    #data <- merge(ax, age_crosswalk, all.x=T, by="age_group_id")
  data <- merge(data,ax, by=c("ihme_loc_id", "sex_id", "year_id", "age_group_id") )
  
  ## order data just in case
  data <- data[order(data$ihme_loc_id,data$sex_id,data$year_id,data$acause,data$age),]
  
  ## get length of intervals (n is set to 5 for the terminal age group)
  data$n <- unlist(tapply(data$age, list(data$ihme_loc_id,data$sex_id,data$year_id,data$acause), function(x) c(x[-1],max(x)+5) - x ))
  
  ## save cause-specific estimates
  cs <- data[data$acause != "all_cause",]
 
  ## format allcause data
  
  ################
  ## get all-cause lifetable
  ###############
  
  data <- data[data$acause == "all_cause",c("ihme_loc_id","sex_id","year_id","acause","age","n","mx", "ax")]
  
  ## iterate ax graduations until convergence
  cat(paste("\tIterating ax graduations\n")); flush.console()
  
  data <- data[order(data$ihme_loc_id,data$sex_id,data$year_id,data$age),]

  ## fix negative ax values
  data$ax[data$ax < 0] <- 1.1
  
  ## qx
  data$qx <- (data$n*data$mx)/(1+((data$n-data$ax)*data$mx))
  data$qx[data$age == max(data$age)] <- 1
  
  ## px
  data$px <- 1- data$qx
  data$px[data$px < 0] <- 0.01
  
  ## lx
  data$lx <- 0
  data$lx[data$age==0] <- 1
  for (i in 1:length(unique(data$age))) {
    temp <- NULL
    temp <- data$lx*(data$px)
    temp <- c(0,temp[-length(temp)])
    data$lx <- 0
    data$lx <- data$lx + temp
    data$lx[data$age==0] <- 1
  }
  temp <- NULL
  
  ## dx 
  dx <- data.table(data)
  setkey(dx,ihme_loc_id,sex_id,year_id)
  dx <- as.data.frame(dx[,c(diff(lx),-1*lx[length(lx)]),by=key(dx)])
  dx <- dx$V1*(-1)
  data <- cbind(data,dx=dx)  
  
  ## nLx
  lx_shift <- data.table(data)
  setkey(lx_shift,ihme_loc_id,sex_id,year_id)
  lx_shift <- as.data.frame(lx_shift[,c(lx[-1],0),by=key(lx_shift)])
  lx_shift <- lx_shift$V1
  data <- cbind(data,lx_shift)
  data$nLx <- (data$n * data$lx_shift) + (data$ax * data$dx)
  data$nLx[data$age == max(data$age)] <- data$lx[data$age == max(data$age)]/data$mx[data$age == max(data$age)]
  data$lx_shift <- NULL
  
  ## Tx
  Tx <- data.table(data)
  setkey(Tx,ihme_loc_id,sex_id,year_id)
  Tx <- as.data.frame(Tx[,list(Tx=rev(cumsum(rev(nLx)))),key(Tx)])
  data$Tx <- Tx$Tx
  Tx <- NULL
  
  ## ex
  data$ex <- data$Tx/data$lx
  
  ## format allcause data
  names(data)[names(data) %in% c("mx","ax","px","nLx","ex")] <- c("mort.all_cause","ax.all_cause","px.all_cause","nLx.all_cause","ex.all_cause")
  data <- data[,c("ihme_loc_id","sex_id","year_id","age","mort.all_cause","ax.all_cause","px.all_cause","nLx.all_cause","ex.all_cause")]
  
  #####################
  ## Get cause deleted px, lx, dx, ax, and nLx values
  #####################

  cat(paste("\nCause deleted lifetable\n\n")); flush.console()
  
  cs$ax <- NULL
  ## add cause specific data back in
  data <- merge(cs,data,by=c("ihme_loc_id","year_id","sex_id","age"))
  if (nrow(data) != nrow(cs)) stop("All-cause data didn't merge correctly")
  cs <- NULL
  
  ## get px cause deleted
  data$pxdel <- data$px.all_cause^(1-(data$mx/data$mort.all_cause))
 
  ## order data
  data <- data[order(data$ihme_loc_id,data$sex_id,data$year_id,data$acause,data$age),]

  ## get lx cause deleted
  data$lxdel <- 0
  data$lxdel[data$age==0] <- 1
  for (i in 1:length(unique(data$age))) {
    temp <- NULL
    temp <- data$lxdel*data$pxdel
    temp <- c(0,temp[-length(temp)])
    data$lxdel <- 0
    data$lxdel <- data$lxdel + temp
    data$lxdel[data$age==0] <- 1
  }
  temp <- NULL
  
  ## get dx cause deleted
  dx <- data.table(data[,c("ihme_loc_id","sex_id","year_id","age","acause","lxdel")])
  setkey(dx,ihme_loc_id,sex_id,year_id,acause)
  dx <- as.data.frame(dx[,list(dx=c(rev(diff(rev(lxdel))),lxdel[length(lxdel)])),by=key(dx)])
  data$dxdel <- dx$dx
  dx <- NULL
#   
#   ## get ax cause deleted
#   ax <- data.table(data)
#   setkey(ax,ihme_loc_id,sex_id,year_id,acause)
#   ax <- as.data.frame(ax[,list(axdel=ax_fun(dxdel,n)),key(ax)])
#   data$axdel <- ax$axdel
#   ax <- NULL

  data$axdel <- data$ax.all_cause
  
  # fix young and old ages
  ss <- data$age %in% c(0,1,5,max(sort(unique(data$age))[-length(sort(unique(data$age)))]))
  data$axdel[ss] <- data$n[ss] + ((data$mort.all_cause[ss] - data$mx[ss])/data$mort.all_cause[ss]) * ((1-data$px.all_cause[ss])/(1-data$pxdel[ss])) * (data$ax.all_cause[ss] - data$n[ss])
  data$axdel[data$axdel < 0] <- 0.01
  ss <- NULL
  
  ## get nLx cause deleted
  lx_shift <- data.table(data)
  setkey(lx_shift,ihme_loc_id,sex_id,year_id,acause)
  lx_shift <- as.data.frame(lx_shift[,c(lxdel[-1],0),by=key(lx_shift)])
  lx_shift <- lx_shift$V1
  data <- cbind(data,lx_shiftdel=lx_shift)
  data$nLxdel <- (data$n * data$lx_shiftdel) + (data$axdel * data$dxdel)
  
  ## fix terminal age group
  ss <- data$age == max(data$age)
  data$nLxdel[ss] <- (data$ex.all_cause[ss]/(1-(data$mx[ss]/data$mort.all_cause[ss]))) * data$lxdel[ss]
  data$lx_shiftdel <- NULL
  ss <- NULL
  
  ##only keep necessary variables
  for (v in c("pxdel","lxdel","dxdel","axdel")) data[,v] <- NULL

  #######################
  ## Do decomposition
  #######################
  
  cat(paste("\nDecomposition\n\n")); flush.console()
  
  ## order data
  data <- data[order(data$year_id,data$ihme_loc_id,data$sex_id,data$acause,data$age),]
  
  ## calculate decomposition
  y1 <- min(data$year_id)
  y2 <- max(data$year_id)
  ss_y1 <- (data$year_id == y1)
  ss_y2 <- (data$year_id == y2)
  data$nLxcause <- NA
  data$nLxcause[data$nLxdel==0] <- data$nLx.all_cause[data$nLxdel==0]
  data$nLxcause[data$nLxdel != 0] <- (data$nLx.all_cause[data$nLxdel != 0]/ data$nLxdel[data$nLxdel != 0])*data$n[data$nLxdel != 0]
  data$dcomp[ss_y1] <- (data$nLxcause[ss_y2] - data$nLxcause[ss_y1]) * ((data$nLxdel[ss_y2] + data$nLxdel[ss_y1])/(2*data$n[ss_y1]))
  data$year_id1 <- y1
  data$year_id2 <- y2
  data <- data[!is.na(data$dcomp),c("ihme_loc_id","year_id1","year_id2","sex_id","age","acause","dcomp","ex.all_cause")]
  names(data)[names(data) == "ex.all_cause"] <- "ex_all_start" 
  ss_y1 <- NULL
  ss_y2 <- NULL
  
  return(data)

} ## END lifetable_decomp()



#############################
#############################
## START ANALYSIS
#############################
#############################

################
## Set directory
################

setwd(dir)

####################
## Read in and format data
####################

for (y1 in c(1990, 1995, 2000, 2005, 2010)) {
  for (y2 in c(1995, 2000, 2005, 2010, 2015)) {
  
    starty <- y1
    
    if (y1 >= y2) {
    	next
    } else {
    	endy <- y2
    }
    
    print(paste0("calculating ", starty, " - ", endy))
    
    dat <- as.data.table(read.dta(paste0("data/le_decomp_viz/intermediate/input_data_", starty, "_", endy, ".dta")))
    dat <- dat[!is.na(ihme_loc_id),]
    
    
    ## get all cause mortality
    setkey(dat,ihme_loc_id,location_id,sex_id,year_id,age, age_group_id)
    all <- as.data.frame(dat[,
                             list(acause="all_cause",
                                  cause="all_cause",
    							  cause_id=294,
                                  mean_rate_death=sum(mean_rate_death)),
                             key(dat)])
    dat <- rbind(as.data.frame(dat),all)
    all <- NULL
    
    
    ## TEMPORARY 2/24/15
    # filter out locations that we do not have a mean life table for and thus have an all_cause rate of 0
    exclude <- unique(dat[dat$acause=="all_cause" & dat$mean_rate_death==0,]$ihme_loc_id)
    dat <- dat[!(dat$ihme_loc_id %in% exclude),]
    dat <- dat[dat$acause != "_none",]
    
    ######################
    ## Get broad cause crosswalk
    ######################
    
    cw <- dat[!duplicated(dat[,c("acause","cause")]),c("acause","cause")]
    cw$cause <- ifelse(grepl("B\\.10\\.",cw$cause),"B.10",substr(cw$cause,1,3))
    cw <- cw[order(cw$cause,cw$acause),]
    write.csv(cw,"data/le_decomp_viz/cause_name_crosswalk.csv",row.names=F)
    
    ####################
    ## Get life expectancy decomposition
    ####################
    
    names(dat)[names(dat) == "mean_rate_death"] <- "mx"
    dat$mx[dat$mx > 1] <- 0.999
    dat <- dat[!is.na(dat$age),]
    
    # do all year combinations
    ## do separate decompositions for subnational and national
 
    
    dc <- lifetable_decomp(dat)
     
    
 
    # do scaling
    correct <- fread("J:/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/result/compiled_summary_lt.csv")
    correct <- correct[age_group_id==28 & ihme_loc_id %in% unique(dc$ihme_loc_id),]
    correct <- correct[year %in% c(starty,endy), c("ihme_loc_id", "sex_id", "year", "ex"), with=F]
    
    correct$year[correct$year == starty] <- 1
    correct$year[correct$year == endy] <- 2
    correct <- as.data.table(reshape(correct, direction = "wide", v.names=c("ex"), idvar = c("ihme_loc_id", "sex_id"), timevar="year"))
    correct$diff <- correct$ex.2-correct$ex.1
    
    
    # correct_iso3s <- unique(correct$iso3)
    # dc_iso3s <- unique(dc$iso3)
    # correct_iso3s %in% dc_iso3s
    
    ## aggregate dcomp over causes
    
    #decomp_all <- aggregate(list(total_dcomp=dc$dcomp), by=list(ihme_loc_id=dc$ihme_loc_id), sex=dc$sex),FUN=sum)
    dc <- as.data.table(dc)
    decomp_all <- dc[,list(total_dcomp=sum(dcomp)), by=list(ihme_loc_id, sex_id)]
    
    final <- merge(correct, decomp_all, by=c("ihme_loc_id", "sex_id"))
    
    final$ratio <- final$diff/final$total_dcomp
    final[final$ratio > 2,]$ratio <- 1
    final <- final[,c("ihme_loc_id", "sex_id", "ratio", "ex.1", "ex.2"), with=F]
    
    final2 <- merge(final, dc, by=c("ihme_loc_id", "sex_id") )
    
    final2$dcomp <- final2$dcomp*final2$ratio
    final2$ex_all_start <- final2$ex.1
    
    # keep only the variables that we are interested in
    final2<- final2[,c("ihme_loc_id", "year_id1",  "year_id2", "sex_id", "age", "acause", "dcomp", "ex_all_start"), with = F]
    dc <- final2
    #####################
    ## Save data
    #####################
    
    ## life expectancies
    ## for graphing
    # save(dc,file="data/decomposition_data.Rdata")
    
    ## for number plugging
    write.dta(dc,file=paste0("data/le_decomp_viz/intermediate/debug/decomposition_data_", starty, "_", endy, ".dta"))
  
  }
}