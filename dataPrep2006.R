									### Data Prep 2006 ###

## Package Load
library(RSQLite)
library(magrittr)
library(survey)
library(Amelia)
library(mitools)
library(parallel)
library(car)
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
	weightTags <- m2006[grep("Final weight 20(07|08|09|10|11|12|13)$",
							 m2006$Label),]

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),",", 
				   paste(weightTags$Variable, collapse=","),
				   ",SCHOOLID, STATE,ST04Q01 as SEX, INDIG, HISEI,
				   INTDAT07, DOB FROM LSAY2006")
	d2006 <- dbGetQuery(db, query)
	d2006$id <- paste0("C06.",row.names(d2006))
	# Add mining state
	d2006$mining <- recode(d2006$STATE, "c(4,6,8)=1; c(1,2,3,5,7)=0")
	#Specify age in days at first interview point
	d2006$age <- as.Date(d2006$INTDAT07, "%d/%m/%y") - as.Date(d2006$DOB, "%d/%m/%y")
	# translate to EGP
	source("./lib/translationFun.R")
	translator <- dbGetQuery(db, "SELECT ISEI, EGP FROM SESconversion")
	#d2006$EGP <- sapply(d2006$HISEI, function(x) translator[match(x, translator$ISEI), "EGP"] )
	d2006$EGP <- sesConvert(d2006$HISEI, fromFormat = "ISEI", toFormat = "EGP", FUN = Mode)
	d2006$EGP <- recode(d2006$EGP, "c(1,2)='upper'; c(3,4,5,6,7,8)='middle'; c(9,10,11)='working'") %>% factor %>%
		relevel( 'upper')
	d2006 <- d2006[,-c(97:98)]
#-------------------------------------------------

## Run edit rules
source("./lib/editRules.R")
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
#write data for PSMatching
save(d2006, file = "psm2006.RData")
#-------------------------------------------------
## Survey package - Imputation and Replicant Weights
N <- length(d2006)
# Reorder
d2006 <- d2006[,c(order(names(d2006)[1:84]), 85:N)]
names(d2006) <- gsub("(|\\.)[0-9]+", "", names(d2006))
# Rename weight variables
# Stack data
dLong06 <- rbind.data.frame(d2006[,c( 1,8,15,22,29,36,43,50,57,64,71,78,85,92:N)], 
							d2006[,c( 2,9,16,23,30,37,44,51,58,65,72,79,86,92:N)],
							d2006[,c(3,10,17,24,31,38,45,52,59,66,73,80,87,92:N)], 
							d2006[,c(4,11,18,25,32,39,46,53,60,67,74,81,88,92:N)],
							d2006[,c(5,12,19,26,33,40,47,54,61,68,75,82,89,92:N)],
							d2006[,c(6,13,20,27,34,41,48,55,62,69,76,83,90,92:N)],
							d2006[,c(7,14,21,28,35,42,49,56,63,70,77,84,91,92:N)])

dLong06$trend <- rep(1:7, each=(nrow(d2006)))
#Devide weights by number of waves to bring back to population values
# Add treatment variable
dLong06$post06 <- ifelse(dLong06$trend > 3,1,0)
dLong06$post03 <- ifelse(dLong06$trend > 6,1,0)

#add cohort variable
dLong06$cohort <- 'y06'
# List wise delete by attrition
dLong06 <- dLong06[!is.na(dLong06$WT),]
# Check number of complete cases
sum(complete.cases(dLong06))
#Multiple Imputations of data holes
MI <- amelia(dLong06,m = 5, p2s = 1, idvars = c('id', 'WT', 'post03','post06' , 'cohort', 'STATE'),
			 noms = c("EGP", "SEX", "INDIG") )
a06 <- imputationList(MI$imputations)
#save to Rdata
save(a06, file = "Cohort06Data.RData")
rm(list=c("dLong06", "d2006", "lsTags", "weightTags", "F2006", "m2006"))
# Wrap in srvey package
dclust <- svydesign(ids = ~id+SCHOOLID, weights = ~WT, data = a06, nest=TRUE)
rm(a06)

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

