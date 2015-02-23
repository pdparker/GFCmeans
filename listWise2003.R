### Data Prep 2003 ###

## Package Load
library(RSQLite)
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

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),
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