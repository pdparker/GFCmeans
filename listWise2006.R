### Data Prep 2006 ###

## Package Load
library(RSQLite)
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

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),
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