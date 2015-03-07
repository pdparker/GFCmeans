sesConvert <- function(x, fromFormat, toFormat, FUN){
	db <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/SQLdb/LSAY2015_update.db")
	FUN <- match.fun(FUN)
	returnList <- c()
	for (i in seq_along(x)){
		if(is.na(x[i])){returnList[i] <-NA}
		else{
			query <- paste0("SELECT ", toFormat, " FROM sesConversion2 where ", fromFormat, " = ", x[i])
			temp <- dbGetQuery(db, query)
			returnList[i] <- FUN(temp[[1]])
		}
	}
	returnList
	}



Mode <- function(z) {
	ux <- unique(z)
	tab <- tabulate(match(z, ux))
	ux[tab == max(tab)]
}


#sesConvert(d1998$HISEI, fromFormat = "ANU3", toFormat = "EGP", FUN = Mode)
