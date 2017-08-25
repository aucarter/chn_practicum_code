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
code.dir <- paste0(shell.dir, "risk_factors/")

## Packages
library(data.table)

# Args
risk.decomp <- T
combine <- F

### Paths

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)

### Code
# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- loc.table[parent_id == 44533, ihme_loc_id]
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)
loc.list <- prov.list
cause.list <- meta[level %in% 1:3, cause_id]
risk.list <- c(86, 87, 339, 239, 100, 93, 83, 84, 238, 341)

## DALY Decomposition
if(risk.decomp) {
	for (risk in risk.list) {
		for (loc in prov.list) {
			risk.string <- paste0("qsub -pe multi_slot 10 ",
								"-e /share/temp/sgeoutput/", user, "/errors ",
								"-o /share/temp/sgeoutput/", user, "/output ",
								"-N ", loc, "_", risk, "_daly_decomp ", 
								shell.dir, "shell_R.sh ", 
								code.dir, "rf_decomp_loc.R ", 
								loc, " ", risk)
			print(risk.string)
			system(risk.string)
		}
	}
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