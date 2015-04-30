						## Sanity Checks and Mean Combination##

sanityCheck <- function(year){
				##Call packages
				library(assertthat)
				## Year files
				freq <- read.csv(paste0("~/Dropbox/Projects_Research/GFCmeans/f", year,".csv") )
				means <- read.csv(paste0("~/Dropbox/Projects_Research/GFCmeans/m", year, ".csv") )
				surveyM <- read.csv(paste0("~/Dropbox/Projects_Research/GFCmeans/s", year, ".csv") )
				## Check same length
				assert_that(nrow(freq) == nrow(means) & nrow(freq) == nrow(surveyM))
				## Read in Frequencies, Means, and Survey Means
					# Frequencies
					freq <- freq[order(freq$X),]
					freq$X <- gsub("\\.[0-9]", "", freq$X)
					# Liswise deleted means
					means <- means[order(means$X),]
					means$X <- gsub("\\.[0-9]", "", means$X)
					# Imputed and weighted means
					surveyM$X <- gsub("([0-9]+)(:)(.+)", "\\3", surveyM$X)
					surveyM <- surveyM[order(surveyM$X),]

					## Check means within tolerence
					tol <- 0.05
					for (i in 1:nrow(freq)){
						assert_that(freq[i,1] == means[i,1] & freq[i,1] == surveyM[i,1])
						temp <- c(rep(1, freq[i,2]), rep(2, freq[i,3]),rep(3, freq[i,4]),rep(4, freq[i,5]))
						tempMean <- mean(temp)
						if(abs(tempMean - means[i,2]) > tol) {cat("case", i, "descrepency between freq and mean")}
						if(abs(surveyM[i,2] - means[i,2]) > tol) {cat("case", i, "descrepency between mean and SurveyM: ",
													" descrepency was", surveyM[i,2] - means[i,2], "\n",
													file = "./library/log.txt", append =TRUE)}
						}
}

capture.output(print(Sys.time() ), file = "./library/log.txt", append = TRUE)

lapply(c("1998", "2003", "2006"), sanityCheck)

							### Merge Files ###
## Read target csv files
sMeansFiles <- list.files(pattern = "^s[0-9]{4}\\.csv")
sMeans <- lapply(sMeansFiles, read.csv)
# Re-orientate data to required structure
for (i in 1:length(sMeans) ){sMeans[[i]]$X <- gsub("[0-9]+:", "", sMeans[[i]]$X) }
sMeans <- lapply(sMeans, function(x) do.call(cbind,split(x, x$X) ) )
sMeans <- do.call(rbind, sMeans)

sMeans$wave <- c(1:9, rep(1:10,2),1:7 ) 
sMeans$age <- c(17:25,rep(17:26,2), 17:23 )
sMeans$year <- c(1997:2005,2000:2009,2004:2013,2007:2013)
sMeans$cohort <- c(rep("C81",9),rep("C84",10),rep("C87",10),rep("C90",7))
write.csv(sMeans, "allMeans.csv")
