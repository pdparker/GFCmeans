							### Data Prep 1998 ###

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

## Extract 1998 well-being data
	# Meta File extraction of target variables
	m1998 <- dbGetQuery(db, "SELECT * FROM LSAYMETA where cohort = 'Y98'")
	lsTags <- m1998[grep("Life satisfaction", m1998$Minor.topic.area),]

	# Exclusion of ecopnomy and politics questions as not consistently present
	lsTags <- lsTags[-c(grep("country|economy", lsTags$Data.element)),]

	# Extract Weight  variables
	weightTags <- m1998[grep("WT20[0-9]{2}", m1998$Variable),]

	# Extraction of major data
	query <- paste("SELECT ", paste(lsTags$Variable, collapse=","),",", 
				   paste(weightTags$Variable, collapse=","), 
				   ",SCHOOLNO, STATE, SEX, INDIG, ANU3DAD, ANU3MUM FROM LSAY1998")
	d1998 <- dbGetQuery(db, query)
	d1998$id <- paste0("C98.",row.names(d1998))
	# Add mining state
	d1998$mining <- recode(d1998$state, "c(4,6,8)=1; c(1,2,3,5,7)=0")
	#Get highest occupational code
	d1998$HISEI <- apply(d1998[,c("ANU3DAD", "ANU3MUM")],1, max, na.rm = TRUE) %>% 
		recode("-Inf = NA") 
	# translate to EGP
	source("./library/translationFun.R")
	translator <- dbGetQuery(db, "SELECT ISEI, EGP FROM SESconversion")
	d1998$EGP <- sesConvert(d1998$HISEI, fromFormat = "ANU3", toFormat = "EGP", FUN = Mode)
	d1998$EGP <- recode(d1998$EGP, "c(1,2)='upper'; c(3,4,5,6,7,8)='middle'; c(9,10,11)='working'") %>% factor %>%
		relevel( 'upper')
	d1998 <- d1998[,-c(135,136)]
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
#---------------------------------------------------
## Survey package - Imputation and Replicant Weights
N <- length(d1998)
# Reorder
d1998 <- d1998[,c(order(names(d1998)[1:120]), 121:N)]
names(d1998) <- gsub("(|\\.)[0-9]+", "", names(d1998))
# Rename weight variables
# Stack data
dLong98 <- rbind.data.frame(d1998[,c(1,11,21,31,41,51,61,71,81,91,101,111,121,131:N)], 
							d1998[,c(2,12,22,32,42,52,62,72,82,92,102,112,122,131:N)],
							d1998[,c(3,12,23,33,43,53,63,73,83,93,103,113,123,131:N)], 
							d1998[,c(4,14,24,34,44,54,64,74,84,94,104,114,124,131:N)],
							d1998[,c(5,15,25,35,45,55,65,75,85,95,105,115,125,131:N)],
							d1998[,c(6,16,26,36,46,56,66,76,86,96,106,116,126,131:N)],
							d1998[,c(7,17,27,37,47,57,67,77,87,97,107,117,127,131:N)],
							d1998[,c(8,18,28,38,48,58,68,78,88,98,108,118,128,131:N)],
							d1998[,c(9,19,29,39,49,59,69,79,89,99,109,119,129,131:N)],
							d1998[,c(10,20,30,40,50,60,70,80,90,100,110,120,130,131:N)])
dLong98$trend <- rep(1:10, each=(nrow(d1998)))
# List wise delete by attrition
dLong98 <- dLong98[!is.na(dLong98$WT),]
# Check number of complete cases
sum(complete.cases(dLong98))
#save to Rdata
save(dLong98, file = "Cohort98Data.RData")
#Multiple Imputations of data holes
MI <- amelia(dLong98,m = 5, p2s = 1, idvars = c('WT', 'id', 'schoolno'), noms = c("EGP") )
a <- imputationList(MI$imputations)
rm(list=c("dLong98", "d1998", "lsTags", "weightTags", "F1998", "m1998"))
# Wrap in srvey package
dclust <- svydesign(ids = ~id+schoolno, weights = ~WT, data = a, nest=TRUE)
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
S1998 <- cbind(trendMeans$coefficients, confint(trendMeans))
colnames(S1998)[1] <- "means"
write.csv(S1998, file = "s1998.csv")

