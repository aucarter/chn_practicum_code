################################################################################
## Purpose: Launch all birth defects analysis code
## Date created: 08/08/2017
## Date modified:
## Author: Austin Carter, aucarter@uw.edu
## Run instructions: 
## Notes: Created during practicum at MCHS in Chengdu, China
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
hiv.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/HIV/")
shell.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/chn_practicum_code/")
code.dir <- paste0(shell.dir, "birth_defects/")

## Packages
library(data.table)

## Arguments
# args <- commandArgs(trailingOnly = TRUE)
# if(length(args) > 0) {

# } else {

# }
region.lt <- T
le.decomp <- T
le.age <- T
daly.decomp <- T
bd.prop <- T
combine <- T

### Paths

### Functions
source(paste0(hiv.dir, "shared_functions/get_locations.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))

### Tables
loc.table <- get_locations()
regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)

### Code
prov.list <- loc.table[parent_id == 44533, ihme_loc_id]
region.list <- regions[region == 1, ihme_loc_id]
loc.list <- rev(c(prov.list, region.list, "CHN_44533"))
cause.list <- meta[level %in% 1:3, cause_id]

## Regional Life Tables
if(region.lt) {
	for(region in region.list) {
		region.string <- paste0("qsub -pe multi_slot 10 ",
							"-e /share/temp/sgeoutput/", user, "/errors ",
							"-o /share/temp/sgeoutput/", user, "/output ",
							"-N ", region, "_region_lt ", 
							shell.dir, "shell_R.sh ", 
							code.dir, "region_lt.R ", 
							region)
		print(region.string)
		system(region.string)
	}
}

## Life Expectancy Decomposition
if(le.decomp) {
	for (loc in loc.list) {
		for(cause in cause.list) {
			if(loc %in% c(region.list, "CHN_44533")) {
				n.cores <- 2
			} else {
				n.cores <- 2
			}
			le.string <- paste0("qsub -pe multi_slot ", n.cores, " ",
								"-e /share/temp/sgeoutput/", user, "/errors ",
								"-o /share/temp/sgeoutput/", user, "/output ",
								"-N ", loc, "_", cause, "_le_decomp ", 
								shell.dir, "shell_R.sh ", 
								code.dir, "le_decomp_loc.R ", 
								loc, " ", cause, " ", n.cores)
			print(le.string)
			system(le.string)
		}
	}
}

## Life Expectancy Decomposition with only 1 age group cause-deleted
if(le.age) {
	for (loc in loc.list) {
		for(del.age in c(28, 5, 6, 7)) {
			le.age.string <- paste0("qsub -pe multi_slot 10",
								"-e /share/temp/sgeoutput/", user, "/errors ",
								"-o /share/temp/sgeoutput/", user, "/output ",
								"-N ", loc, "_", del.age, "_le_age ", 
								shell.dir, "shell_R.sh ", 
								code.dir, "le_decomp_age.R ", 
								loc, " ", del.age)
			print(le.age.string)
			system(le.age.string)
		}
	}
}

## DALY Decomposition
if(daly.decomp) {
	for (loc in loc.list) {
		daly.string <- paste0("qsub -pe multi_slot 10 ",
							"-e /share/temp/sgeoutput/", user, "/errors ",
							"-o /share/temp/sgeoutput/", user, "/output ",
							"-N ", loc, "_daly_decomp ", 
							shell.dir, "shell_R.sh ", 
							code.dir, "daly_decomp_loc.R ", 
							loc)
		print(daly.string)
		system(daly.string)
	}
}

## Birth Defect Proportions
if(bd.prop) {
	bd.string <- paste0("qsub -pe multi_slot 10 ",
						"-e /share/temp/sgeoutput/", user, "/errors ",
						"-o /share/temp/sgeoutput/", user, "/output ",
						"-N bd_prop ", 
						shell.dir, "shell_R.sh ", 
						code.dir, "bd_prop.R")
	print(bd.string)
	system(bd.string)	
}

## Combine Files
if(combine) {
	combine.string <- paste0("qsub -pe multi_slot 10 ",
						"-e /share/temp/sgeoutput/", user, "/errors ",
						"-o /share/temp/sgeoutput/", user, "/output ",
						"-N combine ", 
						shell.dir, "shell_R.sh ", 
						code.dir, "combine_files.R")
	print(combine.string)
	system(combine.string)
}

### End