##########################################################
# Author: Kelly Cercy
# Adapted by: Will Godwin- 08/22/17
# Date: 3 June 2016
# Description: Das Gupta decomp figure for China Under 5 RF paper
##########################################################

##########################################################
# LIBRARIES ---------------------------------------------
##########################################################

rm(list=ls())
library(data.table);library(scales);library(ggplot2);library(dplyr);library(lattice);library(latticeExtra);library(RColorBrewer);library(foreign);library(plyr)

##########################################################
# PARAMETERS ---------------------------------------------
##########################################################

#Windows/Unix
if (Sys.info()["sysname"] == "Linux") j <- "/home/j"
if (Sys.info()["sysname"] == "Windows") j <- "J:"

#Set title, output path
plot_title <- "Global decomposition of changes in all-cause deaths attributable to top ten level 3 risk factors from 1990 to 2016 due to population growth, population ageing, risk exposure and the risk-deleted mortality rate.\nRisks are reported in order of percent change in the number of attributable deaths from 1990 to 2016"
file_path <- paste0(j,"/temp/wgodwin/chn/decomp/decomp_1990_2016.pdf")

##########################################################
# PULL DATA AND RESHAPE -----------------------------------------
##########################################################

dt <- fread(paste0(j,"/temp/wgodwin/chn/decomp/data/death_decomp_compile.csv")) #example data from RF
#setnames(dt, paste0("death_count_", years), paste0("count_", 1:2))
dt <- dt[, .(location_name, rei_name, sex, age_group_id, count_1, count_2, age_structure_effect, population_effect,
              under_rate_effect, risk_effect)]
dt <- dt[,lapply(.SD, sum), by = .(sex, rei_name), .SDcols = c("count_1", "count_2", "age_structure_effect", 
            "population_effect", "risk_effect", "under_rate_effect")]

#Generate pct effects for each factor
dt[, pct_population := ifelse(count_1 == 0, 0, population_effect/count_1)]
dt[, pct_age_structure := ifelse(count_1 == 0, 0, age_structure_effect/count_1)]
dt[, pct_under_rate := ifelse(count_1 == 0, 0, under_rate_effect/count_1)]
dt[, pct_risk := ifelse(count_1 == 0, 0, risk_effect/count_1)]
dt[, pct_total := ifelse(count_1 == 0, 0, (count_2 - count_1)/count_1)]

dt <- dt[, .(sex, rei_name, pct_population, pct_age_structure, pct_under_rate, pct_risk, pct_total)]
dt <- melt(dt, id = c("rei_name", "sex"), variable.factor = F)


##########################################################
# PLOT IT ---------------------------------------------
##########################################################

#create var for total percent change and merge back on 
total_dt <- dt[variable=="pct_total",.(rei_name, value, sex)]
setnames(total_dt,"value","total")
dt <- merge(dt[variable != 'pct_total',],total_dt,by=c("rei_name", "sex"))

#set factor order for stacking
factors <- c("Change due to population ageing","Change due to population growth",
             "Change due to risk exposure","Change due to risk-deleted mortality rate")
dt[variable == "pct_population", variable := "Change due to population growth"]
dt[variable == "pct_risk", variable := "Change due to risk exposure"]
dt[variable == "pct_age_structure", variable := "Change due to population ageing"]
dt[variable == "pct_under_rate", variable := "Change due to risk-deleted mortality rate"]

dt[,factor_format := factor(variable, levels=factors)]

# order risks by total percent change
dt <- dt[with(dt, order(total,rei_name)), ]
dt[, rei_name := reorder(rei_name,-total)]

# Generate percentages
dt[, c("value", "total") := .(value * 100, total * 100)]

#order risks by total dalys in 2015
# dt <- dt[with(dt, order(number_2015,rei_name)), ]
# dt[, rei_name := reorder(rei_name,-number_2015)]

#open up pdf
pdf(file=file_path,height=8.5, width=11)

#plot negative and positive values separately so ggplot shows them properly
plot <- ggplot() +
  geom_point(data=dt,aes(x=rei_name,y=total,order=factor_format),size=0,na.rm=T)+ #not plotting anything, just keeping axis in order bc subsets don't necessarily contain all risks
  geom_bar(data=arrange(dt[value<0,],factor_format),aes(x=rei_name,y=value,fill=factor_format,order=factor_format),stat="identity",width=.75,na.rm=T) +
  geom_bar(data=arrange(dt[value>=0,],factor_format),aes(x=rei_name,y=value,fill=factor_format,order=factor_format),stat="identity",width=.75,na.rm=T) +
  geom_point(data=dt,aes(x=rei_name,y=total,color="Total percent change"),size=2.5,na.rm=T)+ # may need to make size bigger number of risks
  scale_fill_manual(values=c("Change due to population ageing"="#A2C851","Change due to population growth"="#218380",
                             "Change due to risk-deleted mortality rate"="#ffbc42","Change due to risk exposure"="orangered3"),drop=F) + #color of factors (stacked bars)
  scale_color_manual(values=c("Total percent change"="black"))+ #color of total percent change (point)
  #scale_y_continuous(labels=percent,limits=c(-1.5,2),breaks = c(-1,-.5,0,.5,1,1.5),expand=c(0,0))  +
  coord_flip() +  
  labs(title=plot_title,y="", x="",fill="") +
  theme_bw() +
  #geom_hline(yintercept=0, linetype=1,size=.75) +
  theme(plot.title=element_text(vjust=2,hjust=0,size=10), 
        legend.key.size=unit(0.5, "cm"), 
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        legend.box="horizontal") +
  guides(fill=guide_legend(nrow=2,reverse=F))

#print plot to page
print(plot)

#close pdf
dev.off()