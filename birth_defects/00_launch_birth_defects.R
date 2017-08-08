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
bd.prop <- T
combine <- T

### Paths

### Functions
source(paste0(code.dir, "shared_functions/get_locations.R"))

### Tables
loc.table <- get_locations()
regions <- fread(paste0(root, "temp/aucarter/le_decomp/chn_region_table.csv"))

### Code
prov.list <- loc.table[parent_id == 44533, ihme_loc_id]
region.list <- regions[region == 1, ihme_loc_id]
loc.list <- c(prov.list, region.list, "CHN")

## Regional Life Tables
if(region.lt)
	for(region in region.list) {
		region.string <- paste0("qsub -pe multi_slot 10 ",
							"-e /share/temp/sgeoutput/", user, "/errors ",
							"-o /share/temp/sgeoutput/", user, "/output ",
							"-N prep_data ", 
							shell.dir, "shell_R.sh ", 
							code.dir, "region_lt.R ", 
							region)
		print(region.string)
		system(region.string)
	}
}

## Life Expectancy Decomposition
if(le.decomp)
	for (loc in prov.list) {
		le.string <- paste0("qsub -pe multi_slot 10 ",
							"-e /share/temp/sgeoutput/", user, "/errors ",
							"-o /share/temp/sgeoutput/", user, "/output ",
							"-N prep_data ", 
							shell.dir, "shell_R.sh ", 
							code.dir, "le_decomp_loc.R ", 
							loc)
		print(le.string)
		system(le.string)
	}
}

## Birth Defect Proportions
if(bd.prop) {
	bd.string <- paste0("qsub -pe multi_slot 10 ",
						"-e /share/temp/sgeoutput/", user, "/errors ",
						"-o /share/temp/sgeoutput/", user, "/output ",
						"-N prep_data ", 
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
						"-N prep_data ", 
						shell.dir, "shell_R.sh ", 
						code.dir, "le_decomp.R")
	print(combine.string)
	system(combine.string)
}

### End