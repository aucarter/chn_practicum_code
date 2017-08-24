rm(list=ls())
library(reshape2)
library(ggplot2)
library(tidyr)
library(gdata)

setwd("C:/Users/mreitsma/Downloads")

df<-read.csv("J:/WORK/01_covariates/02_inputs/smoking_prevalence/Bloomberg_Tobacco/02_collapse/03_crosswalk/crosswalked_dataset.csv")

isos<-unique(df$ihme_loc_id)

df$sex[df$sex_id==1]<-"male"
df$sex[df$sex_id==2]<-"female"

pdf(file="ggplot_fiveyearage_tovet.pdf")
  for (iso3 in isos) {
      for (sex in c("male", "female")) {
        plot<-ggplot(data=df[which(df$iso3==ihme_loc_id & df$sex==sex),], aes(x=year_id)) +
          geom_ribbon(aes(ymin=gpr_lower, ymax=gpr_upper), fill="#66CD00", alpha=1/3) +
          geom_line(aes(y=gpr_mean), colour="#006400", size=1.2) + 
          geom_point(aes(y=data), colour="#CC0000") +
          #geom_point(aes(y=prev_2013), size=1.2) +
          labs(title=paste("Country: ", iso3, ", Sex: ", sex, sep="")) +
          xlab("Year") +
          ylab("Prevalence") +
          facet_wrap(~age_start, ncol=4) +
          theme(axis.text.x = element_text(size = 8, angle=45))
        print(plot)
      }
    }
dev.off()

