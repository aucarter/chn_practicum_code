##############################################
## Das Gupta decomposition test (risk factors)
## 26 April 2017
##############################################
## Set up environment
library(data.table);library(plyr);library(ggplot2);library(zoo)
library(scales);library(ini);library(RMySQL);library(magrittr)

## Define functions
source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")
source("/home/j/temp/central_comp/libraries/current/r/get_rei_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_cause_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_population.R")
source("/home/j/temp/central_comp/libraries/current/r/get_demographics.R")

## Define global variables
args = commandArgs(trailingOnly=TRUE)
gbdrid = as.numeric(args[1])
measid = as.numeric(args[2])
yids = c(as.numeric(args[3]),as.numeric(args[4]))
file_path = args[5]
lid = args[6]
demo <- get_demographics(gbd_team="epi",gbd_round_id = gbdrid)
#agids <- demo$age_group_ids
agids <- c(2,3,4,5)
sids <- demo$sex_ids

loadData <- function() {

  # pull most detailed cause
  # tb, liver not expanded for alcohol
  cause_ids <- get_cause_metadata(cause_set_id=3,gbd_round_id=gbdrid)[most_detailed==1,]
  cause_ids <- c(cause_ids$cause_id,297,417)

  # Pull risk (all and most-detailed) PAFs
  pafdf <- get_outputs(topic = "rei", rei_id = "most_detailed", cause_id = cause_ids, 
                         measure_id = measid, metric_id = 2, location_id = lid, year_id = yids, 
                         age_group_id = agids, sex_id = sids, gbd_round_id = gbdrid, compare_version_id = 202,
                         rei_set_id = 1)
  df_169 <- get_outputs(topic = "rei", rei_id = 169, cause_id = cause_ids, 
                        measure_id = measid, metric_id = 2, location_id = lid, year_id = yids,
                        age_group_id = agids, sex_id = sids, gbd_round_id = gbdrid, compare_version_id = 202)
  pafdf <- rbind(pafdf, df_169)
  pafdf <- rename(pafdf, c("val" = "met_2"))
  rm(df_169)

  # Pull all risk attributable burden (counts)
  numdf <- get_outputs(topic = "rei", rei_id = "most_detailed", cause_id = cause_ids, 
                       measure_id = measid, metric_id = 1, location_id = lid, year_id = yids, 
                       age_group_id = agids, sex_id = sids, gbd_round_id = gbdrid, compare_version_id = 202,
                       rei_set_id = 1)
  df_169 <- get_outputs(topic = "rei", rei_id = 169, cause_id = cause_ids, 
                        measure_id = measid, metric_id = 1, location_id = lid, year_id = yids, 
                        age_group_id = agids, sex_id = sids, gbd_round_id = gbdrid, compare_version_id = 202,
                        rei_set_id = 1)
  numdf <- rbind(numdf, df_169)
  numdf <- rename(numdf, c("val" = "met_1"))
  rm(df_169)
  
  # Pull CSMR 
  cdf <- get_outputs(topic = "cause", cause_id = cause_ids, measure_id = measid, metric_id = 3,
                     location_id = lid, year_id = yids, age_group_id = agids, sex_id = sids,
                     gbd_round_id = gbdrid, compare_version_id = 202)
  cdf <- rename(cdf, c("val" = "met_3"))
  
  # Pull SEVs for SBP, FPG, IKF PAFs of 1
  map <- data.table(rei_id = c(105,105,107,107,341,341,341,341),cause_id=c(587,590,498,591,590,591,592,593))
  sevdf <- get_outputs(topic = "rei", rei_id = c(105,341,107),
                       measure_id = 29, metric_id = 3, location_id = lid, year_id = yids, 
                       age_group_id = agids, sex_id = sids, gbd_round_id = gbdrid, compare_version_id = 202,
                       rei_set_id = 1)
  sevdf <- merge(sevdf,map,by="rei_id",allow.cartesian=T)
  sevdf[, rei_id := NULL]
  sevdf <- rename(sevdf, c("val" = "sev"))
  sevdf <- sevdf[, list(sev=mean(sev,na.rm=T)), by=c("location_id", "year_id", "age_group_id", "sex_id", "cause_id")]
  sevdf <- sevdf[order(age_group_id)][,sev:=na.locf(sev,fromLast=T,na.rm=F),by=c("location_id","year_id","sex_id","cause_id")]
  sevdf <- sevdf[order(-age_group_id)][,sev:=na.locf(sev,fromLast=T,na.rm=F),by=c("location_id","year_id","sex_id","cause_id")]
  
  # Pull prev for unsafe sex/cervical cancer
  prevdf <- get_outputs(topic = "cause", cause_id = c(432), measure_id = 5, metric_id =1,
                     location_id = lid, year_id = yids, age_group_id = agids, sex_id = sids,
                     gbd_round_id = gbdrid, compare_version_id = 202)
  prevdf <- rename(prevdf, c("val" = "prev"))
  
  # Make requisite transformations
  rfdf <- merge(pafdf[, c("location_id", "year_id", "age_group_id", "sex_id", "rei_id", "cause_id", "met_2"), with = FALSE],
    numdf[, c("location_id", "year_id", "age_group_id", "sex_id","cause_id", "rei_id", "met_1"), with = FALSE],
    by = c("location_id", "year_id", "age_group_id","sex_id","cause_id","rei_id"),all.x=T)
  rfdf <- merge(rfdf, cdf[, c("location_id", "year_id", "age_group_id", "sex_id", "cause_id", "met_3"), with = FALSE],
    by = c("location_id", "year_id", "age_group_id","sex_id", "cause_id"),all.x=TRUE)
  rfdf <- merge(rfdf, sevdf[, c("location_id", "year_id", "age_group_id", "sex_id", "cause_id", "sev"), with = FALSE],
    by = c("location_id", "year_id", "age_group_id","sex_id", "cause_id"),all.x=TRUE)
  rfdf <- merge(rfdf, prevdf[, c("location_id", "year_id", "age_group_id", "sex_id", "cause_id", "prev"), with = FALSE],
    by = c("location_id", "year_id", "age_group_id","sex_id", "cause_id"),all.x=TRUE)
  rm(pafdf,numdf,cdf,sevdf,prevdf)
  rfdf[is.na(met_1), met_1 := 0]
  rfdf[is.na(met_2), met_2 := 0]
  rfdf[is.na(met_3), met_3 := 0]
  rfdf[is.na(sev), sev := 0]
  rfdf[is.na(prev), prev := 0]

  # tb, liver not expanded for alcohol
  rfdf <- rfdf[!(cause_id %in% c(418,419,420,421,934,946,947,954)),]
  
  # flag 3 factor
  rfdf[, paf1 := "no"]
  rfdf[((rei_id == 94 & cause_id == 387) |
        (rei_id == 95 & cause_id == 390) |
        (rei_id == 96 & cause_id == 389) |
        (rei_id == 102 & cause_id %in% c(420,524,560,938)) |
        (rei_id == 103 & cause_id %in% c(562,563,564,565,566)) |
        (rei_id == 129 & cause_id == 514) |
        (rei_id == 150 & cause_id == 512) |
        (rei_id == 161 & cause_id == 511) |
        (rei_id == 240 & cause_id == 387) |
        (rei_id == 334 & cause_id == 381) |
        (rei_id == 335 & cause_id == 381) |
        (rei_id == 169 & cause_id %in% c(387,390,389,420,524,560,938,562,563,564,565,566,514,512,511,381))), paf1 := "three_risk"]
  rfdf[(rei_id == 170 & cause_id %in% c(394,395,396,397,398)) |
       (rei_id == 169 & cause_id %in% c(394,395,396,397,398)), paf1 := "three_under"]
  rfdf[((rei_id == 105 & cause_id %in% c(587,590)) |
       (rei_id == 107 & cause_id %in% c(498,591)) |
       (rei_id == 341 & cause_id %in% c(590,591,592,593)) |
       (rei_id == 170 & cause_id == 432) |
       (rei_id == 169 & cause_id %in% c(498,587,590,591,592,593,432))), paf1 := "cfr_sev"]
  rfdf[(rei_id == 170 & cause_id == 432) |
       (rei_id == 169 & cause_id ==432), paf1 := "cfr_prev"]

  # Calculate burden factors (exposure and risk-deleted)
  rfdf[, under_rate := met_3 * (1 - met_2)][, risk := met_2 / (1 - met_2)][, number := met_1]
  rfdf[paf1 == "three_risk" | paf1 == "three_under", risk := met_3 * met_2]
  rfdf[paf1 == "cfr_sev" | paf1 == "cfr_prev", under_rate := met_3 ]
  rfdf <- rfdf[!is.na(under_rate) & !is.na(risk)]

  # Load pop data
  popdf <- get_population(location_id = lid, year_id = yids, age_group_id = agids, sex_id = sids, 
    gbd_round_id = gbdrid, location_set_id=35)
  # Make requisite transformations for age and pop factors
  popdf <- popdf[, age_structure := population / sum(population), 
        by = c("location_id", "year_id", "sex_id")][, population := sum(population), 
                                                    by = c("location_id", "year_id", "sex_id")]

  # Combine to one data frame
  df <- merge(
    rfdf[, c("location_id", "year_id", "age_group_id", "sex_id", "rei_id", "cause_id", "paf1", "under_rate", "risk", "number", "prev", "sev"), with = FALSE], 
    popdf[, c("location_id", "year_id", "age_group_id", "sex_id", "age_structure", "population"), with = FALSE], 
    by = c("location_id", "year_id", "age_group_id", "sex_id"))
  rm(rfdf,popdf)
  df[paf1 == "cfr_sev", risk := sev]
  df[paf1 == "cfr_sev", under_rate := under_rate/sev]
  df[paf1 == "cfr_prev", risk := prev/(age_structure*population)]
  df[paf1 == "cfr_prev", under_rate := under_rate/(prev/(age_structure*population))]
  df[is.na(risk), risk := 0]
  df[is.na(under_rate), under_rate := 0]

  # Make wide
  df[, year_id := as.numeric(factor(year_id))]
  df <- dcast(df, location_id + age_group_id + sex_id + rei_id + cause_id + paf1 ~ year_id, 
              value.var = c("age_structure", "population", "under_rate", "risk", "number"))

  # Return data frame
  return(df)
}

decompFactors <- function(df) {

  # Calculate 4-factor effects
  df[, population_effect := ((age_structure_1 * under_rate_1 * risk_1 + age_structure_2 * under_rate_2 * risk_2) / 4 +
                               (age_structure_1 * under_rate_1 * risk_2 + age_structure_1 * under_rate_2 * risk_1 +
                                  age_structure_2 * under_rate_1 * risk_1 + age_structure_2 * under_rate_2 * risk_1 +
                                  age_structure_2 * under_rate_1 * risk_2 + age_structure_1 * under_rate_2 * risk_2) / 12) * (population_2 - population_1)]
  df[, age_structure_effect := ((under_rate_1 * population_1 * risk_1 + under_rate_2 * population_2 * risk_2) / 4 +
                                  (under_rate_1 * population_1 * risk_2 + under_rate_1 * population_2 * risk_1 +
                                     under_rate_2 * population_1 * risk_1 + under_rate_2 * population_2 * risk_1 +
                                     under_rate_2 * population_1 * risk_2 + under_rate_1 * population_2 * risk_2) / 12) * (age_structure_2 - age_structure_1)]
  df[, under_rate_effect := ((age_structure_1 * population_1 * risk_1 + age_structure_2 * population_2 * risk_2) / 4 +
                               (age_structure_1 * population_1 * risk_2 + age_structure_1 * population_2 * risk_1 +
                                  age_structure_2 * population_1 * risk_1 + age_structure_2 * population_2 * risk_1 +
                                  age_structure_2 * population_1 * risk_2 + age_structure_1 * population_2 * risk_2) / 12) * (under_rate_2 - under_rate_1)]
  df[, risk_effect := ((age_structure_1 * population_1 * under_rate_1 + age_structure_2 * population_2 * under_rate_2) / 4 +
                         (age_structure_1 * population_1 * under_rate_2 + age_structure_1 * population_2 * under_rate_1 +
                            age_structure_2 * population_1 * under_rate_1 + age_structure_2 * population_2 * under_rate_1 +
                            age_structure_2 * population_1 * under_rate_2 + age_structure_1 * population_2 * under_rate_2) / 12) * (risk_2 - risk_1)]
  
  # Calculate 3-factor effects for PAFs of 1
  df[paf1 == "three_risk" | paf1 == "three_under",
     population_effect := ((age_structure_1 * risk_1 + age_structure_2 * risk_2) / 3 +
                             (age_structure_1 * risk_2 + age_structure_2 * risk_1) / 6) * (population_2 - population_1)]
  df[paf1 == "three_risk" | paf1 == "three_under",
     age_structure_effect := ((risk_1 * population_1 + risk_2 * population_2) / 3 +
                                (risk_1 * population_2 + risk_2 * population_1) / 6) * (age_structure_2 - age_structure_1)]
  df[paf1 == "three_risk" | paf1 == "three_under",
     risk_effect := ((age_structure_1 * population_1 + age_structure_2 * population_2) / 3 +
                       (age_structure_1 * population_2 + age_structure_2 * population_1) / 6) * (risk_2 - risk_1)]

  # set things to 0 as needed
  df[paf1 == "three_risk", under_rate_effect := 0]
  df[paf1 == "three_under", under_rate_effect := risk_effect]
  df[paf1 == "three_under", risk_effect := 0]
  df[is.na(risk_effect), risk_effect := 0]
  df[is.na(population_effect), population_effect := 0]
  df[is.na(age_structure_effect), age_structure_effect := 0]
  df[is.na(under_rate_effect), under_rate_effect := 0]

  # Sum across age and sex
  df <- df[, lapply(.SD, sum), by = c("location_id", "rei_id", "cause_id"), 
     .SDcols = c("number_1", "number_2", "population_effect", "age_structure_effect", "under_rate_effect", "risk_effect")]
  
  # Return data frame
  return(df)
  
}

meatiator <- function(df, reimetadf) {

  # Get mediation data
  meddf <- fread("/home/j/WORK/05_risk/mediation/mediation_matrix_draw.csv")
  meddf <- meddf[, grep("draw", names(meddf), value = TRUE) := .(rep(NULL, 1000))][!is.na(mean),]
  
  # Mediate change factor
  overlapdf <- merge(df,
                     meddf[, c("rei_id", "cause_id", "mean", "med_"), with = FALSE],
                     by = c("rei_id", "cause_id"))
  overlapdf[, risk_effect_mediation := risk_effect * mean]
  overlapdf <- merge(overlapdf[, c("location_id", "cause_id", "med_", "risk_effect_mediation"), with = FALSE],
                     reimetadf[, c("rei_id", "rei"), with = FALSE],
                     by.x = "med_",
                     by.y = "rei")
  overlapdf <- overlapdf[, .(risk_effect_mediation = sum(risk_effect_mediation)), by = .(location_id, rei_id, cause_id)]
  
  # Apply mediation
  df <- merge(df,
              overlapdf,
              by = c("location_id", "cause_id", "rei_id"),
              all.x = TRUE)
  df[(risk_effect < 0 & risk_effect_mediation > 0) | (risk_effect > 0 & risk_effect_mediation < 0), risk_effect_mediation := 0]
  df[abs(risk_effect) < abs(risk_effect_mediation), risk_effect_mediation := risk_effect]
  df[!is.na(risk_effect_mediation), risk_effect := risk_effect - risk_effect_mediation][, risk_effect_mediation := NULL]
  rm(meddf,overlapdf)
  
  # Return data frame
  return(df)
}

riskRaker <- function(df) {
  # Mediate before scaling
  reimetadf <- get_rei_metadata(rei_set_id = 1, gbd_round_id = gbdrid)
  df <- meatiator(df, reimetadf)
  
  # Scale mediated most detailed risks to all-risk aggregate
  scalardf <- merge(df,
                    reimetadf[, c("rei_id", "most_detailed"), with = FALSE],
                    by = "rei_id")
  scalardf <- scalardf[, .(risk_effect = sum(risk_effect)), by = .(location_id, cause_id, most_detailed)]
  scalardf <- dcast(scalardf,
                    location_id + cause_id ~ paste0("agg_", most_detailed),
                    value.var = "risk_effect")
  scalardf <- scalardf[!is.na(agg_1),]
  scalardf[, factor_scalar := agg_0 / agg_1]
  scalardf[agg_0 == 0 & agg_1 == 0, factor_scalar := 1]
  
  # Apply scalar
  df <- merge(df,
              scalardf[, c("location_id", "cause_id", "factor_scalar"), with = FALSE],
              by = c("location_id", "cause_id"),all.x=T)
  rm(scalardf)
  df[rei_id %in% reimetadf$rei_id[reimetadf$most_detailed == 1], risk_effect := risk_effect * factor_scalar][, factor_scalar := NULL]
  
  # Return data frame
  return(df)
}

df <- loadData()
df <- decompFactors(df)
df <- riskRaker(df)
if (!file.exists(paste0(file_path,"/",yids[1],"_",yids[2]))) {
    dir.create(paste0(file_path,"/",yids[1],"_",yids[2]))
}
write.csv(df, file=paste0(file_path,"/",yids[1],"_",yids[2],"/decomp_",lid,"_",measid,".csv"), row.names=F)
# END OF R