								### Data Prep 1995 ###

## Package Load
library(RSQLite)
library(magrittr)
library(survey)
library(Amelia)
library(mitools)
library(parallel)
#-------------------------------------------------

## Extract data from SQL database
db <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/SQLdb/LSAY2015_update.db")
#-------------------------------------------------

## Extract 1995 well-being data
	# Meta File extraction of target variables
	m1995 <- dbGetQuery(db, "SELECT * FROM LSAYMETA where cohort = 'Y95'")
	lsTags <- m1995[grep("Life aspects", m1995$Minor.topic.area),]

	# Exclusion of ecopnomy and politics questions as not consistently present
	lsTags <- lsTags[-c(grep("country|economy", lsTags$Data.element)),]

	# Extract Weight  variables
	weightTags <- m1995[grep("WT[0-9]{4}$| $", m1995$Variable),]
	# Remove weight for 1996
	weightTags %<>% "["(-1,)

	# Extraction of major data from database
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","), ",", 
				   paste(weightTags$Variable, collapse=","), " FROM LSAY1995")
	d1995 <- dbGetQuery(db, query)
	# Add id
	d1995$id <- paste0("C95.",row.names(d1995))
#-------------------------------------------------

## Run edit rules
source("./library/editRules.R")
	# Check data
	names(d1995)[1:120] <- lsTags$smallName 
	if(length(rangeFun(d1995))!=0) {cat("Mistakes in Range Found")}
	#NOT RUN!! rangeFun(d1995)
	d1995[,1:120] <- recodeFun(d1995[,1:120])
	# Run frequencies
	F1995 <- frequencyFun(d1995[,1:120])
	if(dim(F1995)==c(120,4)) {write.csv(F1995, file = "f1995.csv")}
	# Get means
	M1995 <- apply(d1995[,1:120],2, mean, na.rm=TRUE)
	write.csv(M1995, file = "m1995.csv")
#-------------------------------------------------

## Survey package - Imputation and Replicant Weights
	# Reorder
	d1995 <- d1995[,c(order(names(d1995)[1:120]), 121:131)]
	names(d1995) <- gsub("(|\\.)[0-9]+", "", names(d1995))
	# Rename weight variables
	# Stack data
	dLong <- rbind.data.frame(d1995[,c(1,11,21,31,41,51,61,71,81,91,101,111,121,131)], 
							  d1995[,c(2,12,22,32,42,52,62,72,82,92,102,112,122,131)],
							  d1995[,c(3,12,23,33,43,53,63,73,83,93,103,113,123,131)], 
							  d1995[,c(4,14,24,34,44,54,64,74,84,94,104,114,124,131)],
							  d1995[,c(5,15,25,35,45,55,65,75,85,95,105,115,125,131)],
							  d1995[,c(6,16,26,36,46,56,66,76,86,96,106,116,126,131)],
							  d1995[,c(7,17,27,37,47,57,67,77,87,97,107,117,127,131)],
							  d1995[,c(8,18,28,38,48,58,68,78,88,98,108,118,128,131)],
							  d1995[,c(9,19,29,39,49,59,69,79,89,99,109,119,129,131)],
							  d1995[,c(10,20,30,40,50,60,70,80,90,100,110,120,130,131)])
	dLong$trend <- rep(1:10, each=(nrow(d1995)))
	# List wise delet by attrition
	dLong <- dLong[!is.na(dLong$WT),]
	# Check number of complete cases
	sum(complete.cases(dLong))
	#Multiple Imputations of data holes
	MI <- amelia(dLong,m = 5, p2s = 1, idvars = c('WT', 'id'))
	a <- imputationList(MI$imputations)
	rm(list=c("dLong", "d1995", "lsTags", "weightTags", "F1995", "m1995"))
	# Wrap in srvey package
	dclust <- svydesign(ids = ~id, weights = ~WT, data = a)
	rm(a)

## Analysis of weighted data
	# Set parallel options
	options(mc.cores=detectCores())
	# Run Means
	trendMeans <- MIcombine(with (dclust, 
							 	 svyby(~general+living+home+future+career+work+
							  	  	money+lesiure+location+social+people+
							  	  	independence, 
							  		  ~trend, svymean, na.rm=TRUE, multicore=TRUE
							 	 	   )
								  )
							)
	# Wrap means and CIs into table and write to output
	S1995 <- cbind(trendMeans$coefficients, confint(trendMeans))
	colnames(S1995)[1] <- "means"
	write.csv(S1995, file = "s1995.csv")
