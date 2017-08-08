library(data.table)
user <- Sys.info()["user"]
code.dir <- paste0("/homes/", user, "/chn_practicum_code/")
n.slots <- 5

loc.list <- c("GHA", "AUT")
n.jobs <- 0
for (loc in loc.list) {
	code.string <- paste0("qsub -pe multi_slot ", n.slots, " ",
						"-e /share/temp/sgeoutput/", user, "/errors ",
						"-o /share/temp/sgeoutput/", user, "/output ",
						"-N prep_data ", 
						code.dir, "shell_R.sh ", 
						code.dir, "/le_decomp.R ", 
						loc)
	print(code.string)
	system(code.string)
	n.jobs <- n.jobs + 1
}
sprintf("Launched %i jobs", n.jobs)
