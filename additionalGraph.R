library(RSQLite)
library(RSvgDevice)
library(car)

db <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/SQLdb/LSAY2015_update.db")

#Age 95
age95 <- dbGetQuery(db, "SELECT INTDAT97, DOB FROM LSAY1995")
age95 <- as.data.frame(apply(age95,2, as.Date, format = "%d/%m/%y"))
age95$age <- (age95[,1] - age95[,2])
describe(age95)


age98 <- dbGetQuery(db, "SELECT INTDAT00, DOB FROM LSAY1998")
age98 <- as.data.frame(apply(age98,2, as.Date, format = "%d/%m/%y"))
age98$age <- (age98[,1] - age98[,2])
describe(age98)

age03 <- dbGetQuery(db, "SELECT INTDAT04, DOB FROM LSAY2003")
age03 <- as.data.frame(apply(age03,2, as.Date, format = "%d/%m/%y"))
age03$age <- (age03[,1] - age03[,2])
describe(age03)

age06 <- dbGetQuery(db, "SELECT INTDAT07, DOB FROM LSAY2006")
age06 <- as.data.frame(apply(age06,2, as.Date, format = "%d/%m/%y"))
age06$age <- (age06[,1] - age06[,2])
describe(age06)

grade95 <- dbGetQuery(db, "SELECT AA003 FROM LSAY1995")
table(grade95)
grade98 <- dbGetQuery(db, "SELECT AA002 FROM LSAY1998")
table(grade98)
grade03 <- dbGetQuery(db, "SELECT ST01Q01 FROM LSAY2003")
table(grade03)
grade06 <- dbGetQuery(db, "SELECT ST01Q01 FROM LSAY2006")
table(grade06)
#----------------------------------------------------------
library(RSQLite)
library(RSvgDevice)
library(car)

db <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/SQLdb/LSAY2015_update.db")

devSVG(file = "~/Dropbox/Projects_Research/GFCmeans/figure/MoneyByMonth.svg")
m <- matrix(c(1,2,3,4), nrow =2, byrow = TRUE)
layout(m)
#2006 Cohort
month <- dbGetQuery(db, "SELECT INTDAT09, LDJ005D FROM LSAY2006")
month$INTDAT09 <- as.Date(month$INTDAT09, "%d/%m/%y")
month$MONTH <- as.numeric(format(month$INTDAT09, "%m"))
month$YEAR <- as.numeric(format(month$INTDAT09, "%Y"))
month$LDJ005D <- recode(month$LDJ005D, "5=NA; 1=4; 2=3;3=2;4=1")

wellBeing <- with(month, tapply(LDJ005D, list(YEAR, MONTH), mean, na.rm=TRUE) )

wellBeingCI.L <- tapply(month[month$YEAR == 2009, "LDJ005D"],
						month[month$YEAR == 2009, "MONTH"],
						function(x){t.test(x)$conf.int[1]} )

wellBeingCI.H <- tapply(month[month$YEAR == 2009, "LDJ005D"],
						month[month$YEAR == 2009, "MONTH"],
						function(x){t.test(x)$conf.int[2]} )

plot(c(wellBeing[1,]), type="l", xaxt="n", xlab = "Month", bty = 'n',
	 ylab = "Money", main="Cohort 91:\nSatisfaction with Money Over 2009",
	 ylim = c(min(wellBeingCI.L), max(wellBeingCI.H)))
axis(1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J",
							  "J", "A", "S", "O", "N", "D"))

sapply(1:12, function(z) segments(x0 = z,y0 = wellBeingCI.L[z],
								 y1 = wellBeingCI.H[z]) )

segments(x0 = 3.9, y0 = 2.80, y1 = 3.15, 
		 col = "red", lty = 2, lwd = 3)

#-------
#2006 Cohort
month <- dbGetQuery(db, "SELECT INTDAT08, LCJ002D FROM LSAY2006")
month$INTDAT08 <- as.Date(month$INTDAT08, "%d/%m/%y")
month$MONTH <- as.numeric(format(month$INTDAT08, "%m"))
month$YEAR <- as.numeric(format(month$INTDAT08, "%Y"))
month$LCJ002D <- recode(month$LCJ002D, "5=NA; 1=4; 2=3;3=2;4=1")


wellBeing <- with(month, tapply(LCJ002D, list(YEAR, MONTH), mean, na.rm=TRUE) )

wellBeingCI.L <- tapply(month[month$YEAR == 2008, "LCJ002D"],
						month[month$YEAR == 2008, "MONTH"],
						function(x){t.test(x)$conf.int[1]} )

wellBeingCI.H <- tapply(month[month$YEAR == 2008, "LCJ002D"],
						month[month$YEAR == 2008, "MONTH"],
						function(x){t.test(x)$conf.int[2]} )

plot(c(wellBeing[1,]), type="l", xaxt="n", xlab = "Month", bty = 'n',
	 ylab = "Money", main="Cohort 91:\nSatisfaction with Money Over 2008",
	 ylim = c(min(wellBeingCI.L), max(wellBeingCI.H)))
axis(1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J",
							  "J", "A", "S", "O", "N", "D"))

sapply(1:12, function(z) segments(x0 = z,y0 = wellBeingCI.L[z],
								  y1 = wellBeingCI.H[z]) )

segments(x0 = 3.9, y0 = 2.85, y1 = 3.15, 
		 col = "red", lty = 2, lwd = 3)

#---------------------------------------------

#2003 Cohort
month <- dbGetQuery(db, "SELECT INTDAT09, LGJ010D FROM LSAY2003")
month$INTDAT09 <- as.Date(month$INTDAT09, "%d/%m/%y")
month$MONTH <- as.numeric(format(month$INTDAT09, "%m"))
month$YEAR <- as.numeric(format(month$INTDAT09, "%Y"))
month$LGJ010D <- recode(month$LGJ010D, "5=NA; 1=4; 2=3;3=2;4=1")

with(month, table(YEAR, MONTH) )

wellBeing <- with(month, tapply(LGJ010D, list(YEAR, MONTH), mean, na.rm=TRUE) )

wellBeingCI.L <- tapply(month[month$YEAR == 2009, "LGJ010D"],
						month[month$YEAR == 2009, "MONTH"],
						function(x){t.test(x)$conf.int[1]} )

wellBeingCI.H <- tapply(month[month$YEAR == 2009, "LGJ010D"],
						month[month$YEAR == 2009, "MONTH"],
						function(x){t.test(x)$conf.int[2]} )

plot(c(wellBeing[1,]), type="l", xaxt="n", xlab = "Month", bty = 'n',
	 ylab = "Money", main="Cohort 88:\nSatisfaction with Money Over 2009",
	 ylim = c(min(wellBeingCI.L), max(wellBeingCI.H)))
axis(1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J",
							  "J", "A", "S", "O", "N", "D"))

sapply(1:12, function(z) segments(x0 = z,y0 = wellBeingCI.L[z],
								  y1 = wellBeingCI.H[z]) )

segments(x0 = 3.9, y0 = 2.80, y1 = 3.15, 
		 col = "red", lty = 2, lwd = 3)

#2003 Cohort
month <- dbGetQuery(db, "SELECT INTDAT08, LFJ003D FROM LSAY2003")
month$INTDAT08 <- as.Date(month$INTDAT08, "%d/%m/%y")
month$MONTH <- as.numeric(format(month$INTDAT08, "%m"))
month$YEAR <- as.numeric(format(month$INTDAT08, "%Y"))
month$LFJ003D <- recode(month$LFJ003D, "5=NA; 1=4; 2=3;3=2;4=1")

with(month, table(YEAR, MONTH) )

wellBeing <- with(month, tapply(LFJ003D, list(YEAR, MONTH), mean, na.rm=TRUE) )

wellBeingCI.L <- tapply(month[month$YEAR == 2008, "LFJ003D"],
						month[month$YEAR == 2008, "MONTH"],
						function(x){t.test(x)$conf.int[1]} )

wellBeingCI.H <- tapply(month[month$YEAR == 2008, "LFJ003D"],
						month[month$YEAR == 2008, "MONTH"],
						function(x){t.test(x)$conf.int[2]} )

plot(c(wellBeing[1,]), type="l", xaxt="n", xlab = "Month", bty = 'n',
	 ylab = "Money", main="Cohort 88:\nSatisfaction with Money Over 2008",
	 ylim = c(min(wellBeingCI.L), max(wellBeingCI.H)))
axis(1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J",
							  "J", "A", "S", "O", "N", "D"))

sapply(1:12, function(z) segments(x0 = z,y0 = wellBeingCI.L[z],
								  y1 = wellBeingCI.H[z]) )

segments(x0 = 3.2, y0 = 2.90, y1 = 3.2, 
		 col = "red", lty = 2, lwd = 3)
#----------------------------------------------
dev.off()
