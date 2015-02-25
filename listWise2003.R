							### Data Prep 2003 ###

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

## Extract 2003 well-being data
	# Meta File extraction of target variables
	m2003 <- dbGetQuery(db, "SELECT * FROM LSAYMETA where cohort = 'Y03'")
	lsTags <- m2003[grep("Life satisfaction", m2003$Minor.topic.area),]

	# Exclusion of ecopnomy and politics questions as not consistently present
	lsTags <- lsTags[-c(grep("country|economy", lsTags$Data.element)),]

	# Extract Weight  variables
	weightTags <- m2003[grep("BRR|Attrition weight 20(04|05|06|07|08|09|10|11|12|13) .PISA population.",
							 m2003$Label),]

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),",", 
				   paste(weightTags$Variable, collapse=","),
				   " FROM LSAY2003")

	d2003 <- dbGetQuery(db, query)
	d2003$id <- paste0("C03.",row.names(d2003))
#-------------------------------------------------

## Run edit rules
source("./library/editRules.R")
	# Check data
	names(d2003)[1:120] <- lsTags$smallName 
	if(length(rangeFun(d2003))!=0) {cat("Mistakes in Range Found")}
	#NOT RUN!! rangeFun(d2003)
	# Run frequencies
	d2003[,1:120] <- recodeFun(d2003[,1:120])
	F2003 <- frequencyFun(d2003[,1:120])
	if(dim(F2003)==c(120,4)) {write.csv(F2003, file = "f2003.csv")}

## Get means
M2003 <-apply(d2003[,1:120],2, mean, na.rm=TRUE)
write.csv(M2003, file = "m2003.csv")
#-------------------------------------------------
## Survey package - Imputation and Replicant Weights
# Reorder
d2003 <- d2003[,c(order(names(d2003)[1:120]), 121:211)]
names(d2003) <- gsub("(|\\.)[0-9]+", "", names(d2003))
# Rename weight variables
# Stack data
dLong <- rbind.data.frame(d2003[,c(1,11,21,31,41,51,61,71,81,91,101,111,121:200,201,211)], 
						  d2003[,c(2,12,22,32,42,52,62,72,82,92,102,112,121:200,202,211)],
						  d2003[,c(3,12,23,33,43,53,63,73,83,93,103,113,121:200,203,211)], 
						  d2003[,c(4,14,24,34,44,54,64,74,84,94,104,114,121:200,204,211)],
						  d2003[,c(5,15,25,35,45,55,65,75,85,95,105,115,121:200,205,211)],
						  d2003[,c(6,16,26,36,46,56,66,76,86,96,106,116,121:200,206,211)],
						  d2003[,c(7,17,27,37,47,57,67,77,87,97,107,117,121:200,207,211)],
						  d2003[,c(8,18,28,38,48,58,68,78,88,98,108,118,121:200,208,211)],
						  d2003[,c(9,19,29,39,49,59,69,79,89,99,109,119,121:200,209,211)],
						  d2003[,c(10,20,30,40,50,60,70,80,90,100,110,120,121:200,210,211)])
dLong$trend <- rep(1:10, each=(nrow(d2003)))
# List wise delete by attrition
dLong <- dLong[!is.na(dLong$ACHWTP),]
# Check number of complete cases
sum(complete.cases(dLong))
#Multiple Imputations of data holes
MI <- amelia(dLong,m = 5, p2s = 1, idvars = c('W_FSTR','W_FSTR.1','W_FSTR.2','W_FSTR.3','W_FSTR.4',
			'W_FSTR.5','W_FSTR.6','W_FSTR.7','W_FSTR.8','W_FSTR.9','W_FSTR.10','W_FSTR.11','W_FSTR.12',
			'W_FSTR.13','W_FSTR.14','W_FSTR.15','W_FSTR.16','W_FSTR.17','W_FSTR.18','W_FSTR.19',
			'W_FSTR.20','W_FSTR.21','W_FSTR.22','W_FSTR.23','W_FSTR.24','W_FSTR.25','W_FSTR.26',
			'W_FSTR.27','W_FSTR.28','W_FSTR.29','W_FSTR.30','W_FSTR.31','W_FSTR.32','W_FSTR.33',
			'W_FSTR.34','W_FSTR.35','W_FSTR.36','W_FSTR.37','W_FSTR.38','W_FSTR.39','W_FSTR.40',
			'W_FSTR.41','W_FSTR.42','W_FSTR.43','W_FSTR.44','W_FSTR.45','W_FSTR.46','W_FSTR.47',
			'W_FSTR.48','W_FSTR.49','W_FSTR.50','W_FSTR.51','W_FSTR.52','W_FSTR.53','W_FSTR.54',
			'W_FSTR.55','W_FSTR.56','W_FSTR.57','W_FSTR.58','W_FSTR.59','W_FSTR.60','W_FSTR.61',
			'W_FSTR.62','W_FSTR.63','W_FSTR.64','W_FSTR.65','W_FSTR.66','W_FSTR.67','W_FSTR.68',
			'W_FSTR.69','W_FSTR.70','W_FSTR.71','W_FSTR.72','W_FSTR.73','W_FSTR.74','W_FSTR.75',
			'W_FSTR.76','W_FSTR.77','W_FSTR.78','W_FSTR.79', 'id', 'ACHWTP'))
a <- imputationList(MI$imputations)
rm(list=c("dLong", "d2003", "lsTags", "weightTags", "F2003", "m2003"))
# Wrap in srvey package
dclust <- svrepdesign(ids = ~id, weights = ~ACHWTP, repweights = "W_FSTR(|.[0-9])+", 
					  type = "Fay", data = a, rho = 0.5, combined.weights=TRUE)
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
S2003 <- cbind(trendMeans$coefficients, confint(trendMeans))
colnames(S2003)[1] <- "means"
write.csv(S2003, file = "s2003.csv")

