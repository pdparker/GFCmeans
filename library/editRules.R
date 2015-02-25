## Get names
lsTags$smallName <- NA
lsTags[grep("work you do", lsTags$Data.element),"smallName"] <-  "work"
lsTags[grep("spare time", lsTags$Data.element),"smallName"] <- "lesiure"
lsTags[grep("people", lsTags$Data.element),"smallName"] <- "people"
lsTags[grep("money", lsTags$Data.element),"smallName"] <- "money"
lsTags[grep("social life", lsTags$Data.element),"smallName"] <- "social"
lsTags[grep("independence", lsTags$Data.element),"smallName"] <- "independence"
lsTags[grep("career prospects", lsTags$Data.element),"smallName"] <- "career"
lsTags[grep("future", lsTags$Data.element),"smallName"] <- "future" 
lsTags[grep("life at home", lsTags$Data.element),"smallName"] <- "home"
lsTags[grep("standard of living", lsTags$Data.element),"smallName"] <- "living"
lsTags[grep("Where you live", lsTags$Data.element),"smallName"] <- "location"
lsTags[grep("life as a whole", lsTags$Data.element),"smallName"] <- "general"

## Data check
rangeFun <- function(z) {apply(z, 2, function(x) which(x < 1 & x !=NA & x > 5) ) }

recodeFun <- function(z) {
	library(car)
	apply(z, 2, function(x) recode(x, "5=NA; 1=4; 2=3; 3=2; 4=1") )
	}

frequencyFun <- function(z) {
	temp <- rbind(apply(z, 2, function(x){
						t <- as.factor(x)
						levels(t) <- list(VUH = 1,UH = 2,H = 3,VH = 4)
						table(t)
						}
					)
				)
	t(temp)
	}

