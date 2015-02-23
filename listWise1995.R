### Data Prep 1995 ###

## Package Load
library(RSQLite)
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

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),
				   " FROM LSAY1995")
	d1995 <- dbGetQuery(db, query)
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
## Get means
M1995 <- apply(d1995[,1:120],2, mean, na.rm=TRUE)
write.csv(M1995, file = "m1995.csv")
