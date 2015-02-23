### Data Prep 1998 ###

## Package Load
library(RSQLite)
#-------------------------------------------------

## Extract data from SQL database
db <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/SQLdb/LSAY2015_update.db")
#-------------------------------------------------

## Extract 1998 well-being data
	# Meta File extraction of target variables
	m1998 <- dbGetQuery(db, "SELECT * FROM LSAYMETA where cohort = 'Y98'")
	lsTags <- m1998[grep("Life satisfaction", m1998$Minor.topic.area),]

	# Exclusion of ecopnomy and politics questions as not consistently present
	lsTags <- lsTags[-c(grep("country|economy", lsTags$Data.element)),]

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),
				   " FROM LSAY1998")
	d1998 <- dbGetQuery(db, query)
	d1998$id <- paste0("C98.",row.names(d1998))
#-------------------------------------------------

## Run edit rules
source("./library/editRules.R")
	# Check data
	names(d1998)[1:120] <- lsTags$smallName 
	if(length(rangeFun(d1998))!=0) {cat("Mistakes in Range Found")}
	#NOT RUN!! rangeFun(d1998)
	# Run frequencies
	d1998[,1:120] <- recodeFun(d1998[,1:120])
	F1998 <- frequencyFun(d1998[,1:120])
	if(dim(F1998)==c(120,4)) {write.csv(F1998, file = "f1998.csv")}

## Get means
M1998 <- apply(d1998[,1:120],2, mean, na.rm=TRUE)
write.csv(M1998, file = "m1998.csv")