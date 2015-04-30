							### Combine cohorts for Analysis ###

## Packages
library(survey)
library(mitools)
## Read in Rdata files
	# List files
	dataFiles <- list.files(pattern = "Cohort.+RData")
	dataFiles <- dataFiles[c(3,4,1,2)]
	# Load R data files
	cohort <- sapply(dataFiles, function(x) mget(load(x)), simplify = TRUE) 
	cohort <- lapply(cohort, function(x) x$imputations)
	#reorder 03 cohort
	for (i in 1:5){cohort[[1]][[i]] <- cohort[[1]][[i]][,c(1:17, 20, 18,19,21:26)]}
	#reorder 06 cohort
	for (i in 1:5){cohort[[2]][[i]] <- cohort[[2]][[i]][,c(1:17, 20, 18,19,21:26)]}
	#names of cohorts
	cohorts <- c("yr95", "yr98", "yr03", "yr06")
	for (i in seq_along(cohort) ){
		# Provide common names
		for (j in 1:5){
			names(cohort[[i]][[j]])[13:21] <- c("WT", "school", "state", "sex", "indig",
												"ses", "id", "mining","egp")
			# Ensure ids are unique per cohort
			#cohort[[i]][[j]]$id <- paste0(cohorts[i], cohort[[i]]$id)
			# Ensure school ids are unique per cohort
			cohort[[i]][[j]]$school <- paste0(cohorts[i], cohort[[i]][[j]]$school)
		}
	}
	# Combine the data
	totalData <- list()
	for (i in 1:5){
		temp <- list()
		print(i)
		for (j in 1:4){
			print(j)
			temp[[j]] <- cohort[[j]][[i]]
			print(names(cohort[[j]][[i]]))
		}
		totalData[[i]] <- do.call(rbind.data.frame, temp)
	}
	# Clear the ram
	rm(list = c("cohorts", "cohort", "dataFiles", "i") )
	
	
	total <- imputationList(totalData)
	
	by(total[["imputations"]][[1]]$WT, total[["imputations"]][[1]]$cohort, summary)
	
	dclust <- svydesign(ids = ~school+id, weights = ~WT, data = total, nest=TRUE)
	
	save(dclust, file="combinedData.RData")


								### Combine cohorts for Analysis ###

## Packages
library(Amelia)
library(mitools)
library(reshape2)
library(snowfall)
library(MatchIt)
library(survey)
library(parallel)
## Read in Rdata files
	# List files
	dataFiles <- list.files(pattern = "psm[0-9]{4}.+RData")
	# Load R data files
	cohort <- sapply(dataFiles, function(x) mget(load(x)), simplify = TRUE) 
	#Cofigure names for merging
	for (i in 1:4){names(cohort[[i]]) <- tolower(names(cohort[[i]]) )}
	for (i in 1:4){names(cohort[[i]]) <- gsub("[0-9]+", "", names(cohort[[i]]) ) }
	for (i in 1:4){names(cohort[[i]]) <- gsub("(.+)(ool|oolid|oolno|id|no)", "\\1", names(cohort[[i]]) ) }
	
	for (i in 1:4){names(cohort[[i]]) <- gsub("(.+)(ool|oolid|oolno|id|no)", "\\1", names(cohort[[i]]) ) }
	
#----------------------------------------------------------------------------
#Compare 03 and 98 cohort
#----------------------------------------------------------------------------
	#Merge the data
	psm98.03 <- rbind.data.frame(cohort[[2]], cohort[[3]])
	#Add treatment Variable
	psm98.03$treatment <- c(rep(0, nrow(cohort[[2]])), rep(1, nrow(cohort[[3]])))
	#Add common unique variable names
	names(psm98.03) <- make.unique(names(psm98.03), sep='')
	#We only need the first 6 time waves. Dropping waves 7 to 10
	psm98.03 <- psm98.03[, -grep(".+[7-9]$", names(psm98.03))]
	#Will use wt6 as the weight for all analysis
	psm98.03 <- psm98.03[!is.na(psm98.03$wt6),]
	psm98.03[which(psm98.03$indig==8), "indig"] <- NA
	# Imputing data
	sfInit(parallel=TRUE, cpus=8)
	sfLibrary(Amelia)
	sfExport("psm98.03")
	MI <- sfLapply(1:8, function(i) {
		amelia(psm98.03, m = 1, idvars = c(85:93,96,97,99), noms = c(94,95,100,101))
		}
	)
	sfStop()
	#remove unsuccessful imputations
	#MI<- MI[-which(sapply(MI, function(x) is.null(x$imputations$imp1))==TRUE)]
	#Turn into Amelia Object aking only first 5 successful imputations
	MIimp <- ameliabind(MI[[1]], MI[[2]],MI[[3]],MI[[4]],MI[[5]])
	#Wrap in an imputation list
	MIimp <- imputationList(MIimp$imputations)
	d98.03 <- svydesign(data = MIimp, id = ~sch)


	save(d98.03, MIimp, file="psm98.03.RData")

	#----------------------------------------------------------------------------
	#Compare 06 and 03 cohort
	#----------------------------------------------------------------------------
	#Merge the data
	d03 <-cohort[[3]][,-c(49:121,126:130)]
	d03 <- d03[, sort(names(d03))]
	d06 <-cohort[[4]][,-c(49:84,90:91)]
	d06 <- d06[,sort(names(d03))]
	psm03.06 <- rbind.data.frame(d03,d06)
	#Add treatment Variable
	psm03.06$treatment <- c(rep(0, nrow(cohort[[3]])), rep(1, nrow(cohort[[4]])))
	#Will use wt6 as the weight for all analysis
	psm03.06 <- psm03.06[!is.na(psm03.06$wt.3),]
	# Imputing data
	sfInit(parallel=TRUE, cpus=8)
	sfLibrary(Amelia)
	sfExport("psm03.06")
	MI <- sfLapply(1:8, function(i) {
		amelia(psm03.06, m = 1, idvars = c(20,47,53,15,38,58:61),
			   noms = c(48,6,25))
		}
	)
	sfStop()
	
	
	
	#remove unsuccessful imputations
	#MI<- MI[-which(sapply(MI, function(x) is.null(x$imputations$imp1))==TRUE)]
	#Turn into Amelia Object aking only first 5 successful imputations
	MIimp <- ameliabind(MI[[1]], MI[[2]],MI[[3]],MI[[4]],MI[[5]])
	#Wrap in an imputation list
	MIimp <- imputationList(MIimp$imputations)
	d03.06 <- svydesign(data = MIimp, id = ~sch)

	save(d03.06, MIimp, file="psm03.06.RData")

	