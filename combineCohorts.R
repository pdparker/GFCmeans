							### Combine cohorts for Analysis ###

## Packages
library(Amelia)

## Read in Rdata files
	# List files
	dataFiles <- list.files(pattern = "Cohort.+RData")
	# Load R data files
	cohort <- sapply(dataFiles, function(x) mget(load(x)), simplify = TRUE) 
	cohorts <- c("yr95", "yr98", "yr03", "yr06")
	for (i in seq_along(cohort) ){
		# Provide common names
		names(cohort[[i]])[13:22] <- c("WT", "school", "state", "sex", "indig", "id", "mining",
										"ses", "egp", "trend")
		# Add cohort variable
		cohort[[i]]$cohort <- cohorts[i]
		# Ensure ids are unique per cohort
		cohort[[i]]$id <- paste0(cohorts[i], cohort[[i]]$id)
		# Ensure school ids are unique per cohort
		cohort[[i]]$school <- paste0(cohorts[i], cohort[[i]]$school)
	}
	# Combine the data
	totalData <- do.call(rbind.data.frame, cohort)
	# Clear the ram
	rm(list = c("cohorts", "cohort", "dataFiles", "i") )
	
	
# Impute missing data
	MI <- amelia(totalData, idvars = c("school", "id", "state","WT","mining"),
				 noms = c("egp", "cohort") )
	
	total <- MI$imputations
	save(total, file="combinedData.RData")
	