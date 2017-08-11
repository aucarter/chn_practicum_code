##############################################
## Das Gupta decomposition 
##############################################

## Set up environment
rm(list = ls())
library(data.table);library(plyr);library(ggplot2)
library(scales);library(ini);library(RMySQL);library(magrittr)

## Define functions
source("/home/j/temp/central_comp/libraries/current/r/get_demographics.R")
source("/home/j/temp/central_comp/libraries/current/r/get_population.R")
source("/home/j/temp/central_comp/libraries/current/r/get_location_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_rei_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_cause_metadata.R")
source("/home/j/WORK/01_covariates/common/ubcov_central/functions/cluster_tools.r")
source("/home/j/WORK/01_covariates/common/ubcov_central/functions/ubcov_tools.r")
source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")

####################################################################################################
# - HELPERS ----------------------------------------------------------------------------------------
####################################################################################################

main <- function(gbd_round_id,measure_id,year_start_id,year_end_id,cause_lvl,data_dir,code_dir,out_dir,log_dir) {

  # # submit job for each most detailed loc
  # locations <- get_location_metadata(location_set_id=35,gbd_round_id=gbd_round_id)[most_detailed == 1,]
  # file_list <- NULL
  # setwd("/home/j/WORK/01_covariates/common/ubcov_central")
  # for (loc in locations$location_id) {
  #   output_path <- paste0(data_dir,"/",year_start_id,"_",year_end_id,"/decomp_", loc, "_",measure_id,".csv")
  #   file_list <- c(file_list, output_path)
  #   qsub = paste0('qsub -P proj_rfprep -N ', gsub("!", loc, 'decomp_!'),
  #    ' -e /dev/null -o ',log_dir,' -pe multi_slot 4 ',code_dir,'/r_shell.sh ',code_dir,'/decomposition_01.R ',
  #    gbd_round_id, " ", measure_id, " ", year_start_id, " ", year_end_id, " ", data_dir, " ", loc)
  #   system(qsub)
  # }

  #set objects for qsub
  project <- "-P proj_custom_models " 
  sge.output.dir <- "-o /share/temp/sgeoutput/wgodwin/output -e /share/temp/sgeoutput/wgodwin/errors"
  cores.provided <- 4
  rscript <- "/snfs2/HOME/wgodwin/chn/decomposition_01.R"
  rshell <- "/snfs2/HOME/wgodwin/chn/R_shell.sh"

  # submit job for each most detailed loc
  locations <- get_location_metadata(location_set_id=35,gbd_round_id=gbd_round_id)[most_detailed == 1 & parent_id == 6,]
  locs <- locations$location_id
  remove <- c(361, 354) # HK and Macao
  locs <- setdiff(locs, remove)
  file_list <- NULL
  setwd("/home/j/WORK/01_covariates/common/ubcov_central")
  for (loc in locs) {
    output_path <- paste0(data_dir,"/",year_start_id,"_",year_end_id,"/decomp_", loc, "_",measure_id,".csv")
    file_list <- c(file_list, output_path)
    
    args <- paste(gbd_round_id, measure_id, year_start_id, year_end_id, data_dir, loc) 
    jname.arg <- paste0("-N decomp_", loc)
    slot.arg <- paste0("-pe multi_slot ", cores.provided)
    sys.sub <- paste("qsub", project, sge.output.dir, jname.arg, slot.arg)  

    system(paste(sys.sub, rshell, rscript, args))
  }
  # Wait until all jobs are done
  job_hold("decomp", file_list = file_list)
  # append all the files
  df <- rbindlist(lapply(file_list,fread),use.names=T)

  # aggregate cause and location
  df <- aggCause(df,gbd_round_id)
  df <- aggLoc(df,gbd_round_id)

  ## add correct death counts
  numdf <- get_outputs(topic = "rei", rei_id = 169, cause_id = "all", 
                       measure_id = measure_id, metric_id = 1,
                       location_id = "all",
                       year_id = c(year_start_id,year_end_id),
                       age_group_id = 22, sex_id = 3, gbd_round_id = 4, compare_version_id = 202)
  numdf <- numdf[,c("location_id","year_id","cause_id","val"),with=F]
  numdf[, year_id := as.numeric(factor(year_id))]
  numdf <- dcast(numdf, location_id + cause_id ~ year_id, 
                 value.var = c("val"))
  setnames(numdf,c("1","2"),c("number_1","number_2"))
  df <- merge(df[, c("number_1", "number_2") := .(NULL, NULL)],numdf,by=c("location_id","cause_id"))

  # Plot data
  reimetadf <- get_rei_metadata(rei_set_id = 1, gbd_round_id = gbd_round_id)
  causemetadf <- get_cause_metadata(cause_set_id = 3, gbd_round_id = gbd_round_id)
  yids <- c(year_start_id,year_end_id)
  #allloc <- get_location_metadata(location_set_id=35,gbd_round_id=gbd_round_id)[order(sort_order),]
  pdf(file = paste0(out_dir, "/", yids[1], "_", yids[2], "/", "decomp_mort_causelvl1and2.pdf"),
    height = 8.5, width =11)
  for(l in unique(df$location_id)){  ########Uncommented
    causePlotter(df, loc_set = l, gbd_round_id, measure_id, reimetadf, causemetadf,yids,cause_lvl = 1)
    causePlotter(df, loc_set = l, gbd_round_id, measure_id, reimetadf, causemetadf,yids,cause_lvl = 2)
  } ####
  dev.off()

}

causePlotter <- function(df, loc_set, gbd_round_id, measure_id, reimetadf, causemetadf,yids,cause_lvl) {
  df <- df[location_id == loc_set,][,location_id := NULL]
  df[, total_pct := (number_2 - number_1) / number_1]
  df[, c("age_structure_effect", "population_effect", "under_rate_effect", "risk_effect") :=
       .(age_structure_effect / number_1, population_effect / number_1,
         under_rate_effect / number_1, risk_effect / number_1)]
  df[, c("number_1", "number_2") := .(NULL, NULL)]
  df <- melt(df,
             id.vars = c("cause_id", "rei_id"),
             value.vars = c(grep("effect", names(df), value = TRUE), "total_pct"),
             variable.name = "factor_format",
             value.name = "change")
  #create var for total percent change and merge back on
  total_df <- df[factor_format=="total_pct",list(cause_id,rei_id,change)]
  setnames(total_df,"change","total")
  df <- merge(df[factor_format!="total_pct",],total_df,by=c("cause_id","rei_id"))
  meas_name <- ifelse(measure_id ==2,"DALY",ifelse(measure_id == 1,"mortality",ifelse(measure_id==2,"YLD","YLL")))
  df[, factor_format := factor(factor_format,
                               level = c("age_structure_effect", "population_effect", "under_rate_effect", "risk_effect"),
                               label = c("Change due to population ageing",
                                         "Change due to population growth",
                                         paste0("Change due to risk-deleted ",meas_name," rate"),
                                         "Change due to risk exposure"))]
  df <- merge(df,causemetadf[, c("cause_id", "cause_name", "level", "most_detailed"), with = FALSE],by = "cause_id")
  # subset to needed cause level
  df <- df[level == cause_lvl | (level < cause_lvl & most_detailed == 1) | cause_id == 294,]
  df[, c("level", "most_detailed") := .(NULL, NULL)]
  df[,cause_name := paste(strwrap(cause_name, width = 45),collapse="\n"),by=1:nrow(df)]
  df <- merge(df,reimetadf[, c("rei_id", "rei_name"), with = FALSE],by = "rei_id")
  df[, location_id := loc_set]
  df[, location_name := unique(get_location_metadata(location_set_id=35,gbd_round_id=gbd_round_id)[level > 3,location_name := paste0(location_name," (", substr(ihme_loc_id, 1, 3),")")][location_id==loc_set,]$location_name)]
  
  # set colors
  colors <- c("Change due to population ageing" = "#A2C851",
              "Change due to population growth" = "#218380",
              "Change due to risk-deleted XX rate" = "#ffbc42",
              "Change due to risk exposure" = "orangered3")
  names(colors)[[3]] <- paste0("Change due to risk-deleted ",meas_name," rate")
  
  # order causes by total percent change with all cause at the top
  df <- df[rei_id == 169,]
  df[, sort := total]
  sortmin <- min(df$sort)
  df[cause_id==294,sort := sortmin-.0001]
  df <- df[with(df, order(sort,cause_name)), ]
  df[, cause_name := reorder(cause_name,-sort)]
 # write.csv(df,paste0(out_dir,"/cause_decomp_", yids[1], "_", yids[2], "_measure",measure_id,"_causelvl",cause_lvl,".csv"),row.names=F)
  plot <- ggplot() +
    geom_point(data=df,aes(x=cause_name,y=sort),
               size=-1,na.rm=T,color="white")+ #not plotting anything, just keeping axis in order bc subsets don't necessarily contain all risks
    geom_bar(data = df[change < 0,],
             aes(x = cause_name, y = change, fill = factor_format), stat = "identity", width = .75, na.rm = TRUE) +
    geom_bar(data = df[change >= 0,],
             aes(x = cause_name, y = change, fill = factor_format), stat = "identity", width = .75, na.rm = TRUE) +
    geom_point(data = df,
               aes(x = cause_name, y = total, color = "Total percent change"),
               size = 3, na.rm = TRUE) +
    geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") +
    scale_fill_manual(values = colors, drop = FALSE) +
    scale_color_manual(values = c("Total percent change" = "black")) +
    scale_y_continuous(labels = percent)  +
    coord_flip() +
    labs(title = paste0(unique(df$location_name)," decomposition of changes all cause and Level ",cause_lvl," cause ","death",
                        "s attributable to all risk factors from ",yids[1]," to ",yids[2], " in children under 5", "."),
         y = "Percent Change (%)", x = "", fill = "") +
    theme_bw() +
    theme(plot.title = element_text(vjust = 2, hjust = 0, size = 9),
          axis.text.x = element_text(size=7),
          legend.key.size = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          plot.margin = unit(c(.5, .5, .5, .5), "inch")) +
    guides(fill = guide_legend(nrow = 2, reverse = FALSE))
  # Print plot to page
  print(plot)
  
}

aggLoc <- function(df, gbd_round_id) {
  
  lsvid <- query(paste("SELECT shared.active_location_set_version(35, ",gbd_round_id,") as lsvid"),"epi")
  locdf <- query(paste("SELECT lhh.level, lhh.location_id,lhh.location_name,lhh.parent_id,lhh.most_detailed,
                        (SELECT GROUP_CONCAT(location_id) 
                        FROM shared.location_hierarchy_history 
                        WHERE location_set_version_id = ",lsvid," and parent_id = lhh.location_id) AS child_ 
                        FROM shared.location_hierarchy_history lhh 
                        WHERE lhh.location_set_version_id = ",lsvid,"
                        GROUP BY lhh.location_id, lhh.location_name, lhh.parent_id, lhh.most_detailed 
                        ORDER BY lhh.sort_order"),"epi")
  locdf <- locdf[child_ != "",]
  lmax <- max(locdf$level)
  
  # agg up the loc hierarchy
  for (i in lmax:0) {
    ltmp <- locdf[level == i,]
    if (nrow(ltmp)!=0) {
      for (j in 1:nrow(ltmp)) {
        keep <- unique(ltmp[j,]$child_)
        keep <- apply(do.call("rbind", strsplit(keep, ",")), 2, as.numeric)
        parent <- unique(ltmp[j,]$location_id)
        tmp <- df[location_id %in% keep,]
        if (nrow(tmp)!=0) {
          tmp <- tmp[, lapply(.SD, sum), by = c("cause_id", "rei_id"), 
              .SDcols = c("number_1", "number_2", "population_effect", "age_structure_effect", 
                "under_rate_effect", "risk_effect")]
          tmp[, location_id := parent]
          df <- rbind(tmp,df)
        }
      }
    }
  }
  
  # Return data frame
  return(df)
}

aggCause <- function(df, gbd_round_id) {
  
  csvid <- query(paste("SELECT shared.active_cause_set_version(3, ",gbd_round_id,") as csvid"),"epi")
  causedf <- query(paste("SELECT chh.level,chh.cause_id,chh.cause_name,chh.parent_id,chh.most_detailed,
                         (SELECT GROUP_CONCAT(cause_id) 
                         FROM shared.cause_hierarchy_history 
                         WHERE cause_set_version_id = ",csvid," and parent_id = chh.cause_id) AS child_ 
                         FROM shared.cause_hierarchy_history chh 
                         WHERE chh.cause_set_version_id = ",csvid,"
                         GROUP BY chh.cause_id, chh.cause_name, chh.parent_id, chh.most_detailed 
                         ORDER BY chh.sort_order"),"epi")
  causedf <- causedf[child_ != "",]
  cmax <- max(causedf$level)
  
  # agg up the cause hierarchy
  for (i in cmax:0) {
    ctmp <- causedf[level == i,]
    if (nrow(ctmp)!=0) {
      for (j in 1:nrow(ctmp)) {
        keep <- unique(ctmp[j,]$child_)
        keep <- apply(do.call("rbind", strsplit(keep, ",")), 2, as.numeric)
        parent <- unique(ctmp[j,]$cause_id)
        tmp <- df[cause_id %in% keep,]
        if (nrow(tmp)!=0) {
          tmp <- tmp[, lapply(.SD, sum), by = c("location_id", "rei_id"), 
              .SDcols = c("number_1", "number_2", "population_effect", "age_structure_effect", 
                "under_rate_effect", "risk_effect")]
          tmp[, cause_id := parent]
          df <- rbind(tmp,df)
        }
      }
    }
  }
  
  # Return data frame
  return(df)
}

query <- function(query,conn_def) {
  odbc <- read.ini("/home/j/WORK/05_risk/central/code/diagnostics/helpers/.odbc.ini")
  conn <- dbConnect(RMySQL::MySQL(), 
                    host = odbc[[conn_def]]$server, 
                    username = odbc[[conn_def]]$user, 
                    password = odbc[[conn_def]]$password)
  dt <- dbGetQuery(conn,query) %>% data.table
  dbDisconnect(conn)
  return(dt)
}

####################################################################################################
# - MAKE THE PLOTS ---------------------------------------------------------------------------------
####################################################################################################

main(gbd_round_id=4,
    measure_id = 1,
    year_start_id = 1990,
    year_end_id = 2016,
    cause_lvl = 2,
    data_dir = "/home/j/temp/wgodwin/chn/decomp2",
  code_dir = "/homes/wgodwin/chn",
  out_dir = "/home/j/temp/wgodwin/chn/decomp2",
  log_dir= "/share/temp/sgeoutput/wgodwin/output")