### Data Prep 2006 ###

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

## Extract 2006 well-being data
	# Meta File extraction of target variables
	m2006 <- dbGetQuery(db, "SELECT * FROM LSAYMETA where cohort = 'Y06'")
	lsTags <- m2006[grep("Life satisfaction", m2006$Minor.topic.area),]

	# Exclusion of ecopnomy and politics questions as not consistently present
	lsTags <- lsTags[-c(grep("country|economy", lsTags$Data.element)),]

	# Extract Weight  variables
	weightTags <- m2006[grep("BRR-FAY|Attrition weight 20(07|08|09|10|11|12|13) .PISA population.",
							 m2006$Label),]

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),",", 
				   paste(weightTags$Variable, collapse=","),
				   " FROM LSAY2006")
	d2006 <- dbGetQuery(db, query)
	d2006$id <- paste0("C06.",row.names(d2006))
#-------------------------------------------------

## Run edit rules
source("./library/editRules.R")
	# Check data
	names(d2006)[1:84] <- lsTags$smallName 
	if(length(rangeFun(d2006))!=0) {cat("Mistakes in Range Found")}
	#NOT RUN!! rangeFun(d2006)
	# Run frequencies
	d2006[,1:84] <- recodeFun(d2006[,1:84])
	F2006 <- frequencyFun(d2006[,1:84])
	if(dim(F2006)==c(84,4)) {write.csv(F2006, file = "f2006.csv")}

## Get means
M2006 <-apply(d2006[,1:84],2, mean, na.rm=TRUE)
write.csv(M2006, file = "m2006.csv")
#-------------------------------------------------
## Survey package - Imputation and Replicant Weights
# Reorder
d2006 <- d2006[,c(order(names(d2006)[1:84]), 85:172)]
names(d2006) <- gsub("(|\\.)[0-9]+", "", names(d2006))
# Rename weight variables
# Stack data
dLong <- rbind.data.frame(d2006[,c( 1,8,15,22,29,36,43,50,57,64,71,78,85:164,165,172)], 
						  d2006[,c( 2,9,16,23,30,37,44,51,58,65,72,79,85:164,166,172)],
						  d2006[,c(3,10,17,24,31,38,45,52,59,66,73,80,85:164,167,172)], 
						  d2006[,c(4,11,18,25,32,39,46,53,60,67,74,81,85:164,168,172)],
						  d2006[,c(5,12,19,26,33,40,47,54,61,68,75,82,85:164,169,172)],
						  d2006[,c(6,13,20,27,34,41,48,55,62,69,76,83,85:164,170,172)],
						  d2006[,c(7,14,21,28,35,42,49,56,63,70,77,84,85:164,171,172)])

dLong$trend <- rep(1:7, each=(nrow(d2006)))
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
rm(list=c("dLong", "d2006", "lsTags", "weightTags", "F2006", "m2006"))
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
S2006 <- cbind(trendMeans$coefficients, confint(trendMeans))
colnames(S2006)[1] <- "means"
write.csv(S2006, file = "s2006.csv")

