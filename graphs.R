				### Graphs for study 1###
## load packages
library(wesanderson)
library(RColorBrewer)
library(RSvgDevice)
## load data
sMeans <- read.csv("allMeans.csv")
## Graphs for paper
simpleCap <- function(x) {
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s, 1, 1)), substring(s, 2),
		  sep = "", collapse = " ")
}

satNames <- c("general", "work","living","money",
			  "people", "social", "home",
			  "career", "future","independence",
			  "lesiure","location")

for(i in seq_along(satNames)){
	devSVG(file = paste0("figures/", satNames[i], ".svg") )
	### Function to plot all factors by age ###
	## Plot layouts
	m <- matrix(c(1,1,1,1,1,1,
				  2,2,2,2,3,3,
				  2,2,2,2,3,3,
				  2,2,2,2,3,3,
				  2,2,2,2,4,4,
				  2,2,2,2,4,4,
				  2,2,2,2,4,4), nrow =7, byrow = TRUE)
	layout(m)
	#Plot Title
	par(mar = c(0,0,0,0))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.5, y = 0.5, paste0(simpleCap(satNames[i]), " Satisfaction"), 
		 cex = 1.6, col = "black")
	par(mar=c(5.1,4.1,4.1,2.1) )
		
	## Plot 1
	# Extract the mean and CIs
	reqNams <- grep(satNames[i], names(sMeans), value = TRUE)[-1]
	# Select the rows of the data that I need
	l95 <- which(sMeans$cohort == "C81")
	l98 <- which(sMeans$cohort == "C84")
	l03 <- which(sMeans$cohort == "C87")
	l06 <- which(sMeans$cohort == "C90")
	# Pick colours
	cols <- brewer.pal(n = 4, name = 'Dark2')
	# Plot cohort 95
	plot(1:9, sMeans[l95,reqNams[1]],
		col = cols[1], type = "l", bty= "n", lwd = 2,
		main = "All Cohorts",
		xlab = "Avg. Age", ylab = "Satisfaction",
		ylim = range(sMeans[,reqNams[1]]), xlim = c(1,12), xaxt="n")
	# Plot cohort 98
	lines(sMeans[l98,reqNams[1]], lwd = 2,col = cols[2])
	#Plot cohort 03
	lines(1:6, sMeans[l03[1:6],reqNams[1]], lwd = 2,col = cols[3])
	lines(6:10, sMeans[l03[-c(1:5)],reqNams[1]], lwd = 2,col = cols[3], lty=2)
	# plot cohort 06
	lines(1:3, sMeans[l06[1:3],reqNams[1]], lwd = 2,col = cols[4])
	lines(3:7,sMeans[l06[-c(1:2)],reqNams[1]], lwd = 2,col = cols[4], lty=2)
	# Add confidence intervals
	sapply(1:9, function(z) segments(x0 = z,y0 =sMeans[l95[z], reqNams[2]],
									 y1 = sMeans[l95[z], reqNams[3]], col = cols[1]) )
	sapply(1:10, function(z) segments(x0 = z,y0 =sMeans[l98[z], reqNams[2]],
									 y1 = sMeans[l98[z], reqNams[3]], col = cols[2]) )
	sapply(1:10, function(z) segments(x0 = z,y0 =sMeans[l03[z], reqNams[2]],
									 y1 = sMeans[l03[z], reqNams[3]], col = cols[3]) )
	sapply(1:7, function(z) segments(x0 = z,y0 =sMeans[l06[z], reqNams[2]],
									 y1 = sMeans[l06[z], reqNams[3]], col = cols[4]) )
	# Add x-axis
	axis(1, at = 1:10, labels = 17:26)
	
	#Add legend
	text(10, sMeans[l95[length(l95)],reqNams[1]], "Cohort 81", col = cols[1])
	text(11, sMeans[l98[length(l98)],reqNams[1]], "Cohort 84", col = cols[2])
	text(11, sMeans[l03[length(l03)],reqNams[1]], "Cohort 87", col = cols[3])
	text(8, sMeans[l06[length(l06)],reqNams[1]], "Cohort 90", col = cols[4])
	
	## Plot 2
	plot(1:5, sMeans[l03[1:5],reqNams[1]],
		 col = cols[3], type = "l", bty= "n", lwd = 2,
		 main = "C87 vs C90",
		 xlab = "Avg. Age", ylab = "Satisfaction",
		 ylim = range(sMeans[,reqNams[1]]), xlim = c(1,5), xaxt="n")
	# Plot cohort 06
	lines(1:3, sMeans[l06[1:3],reqNams[1]], lwd = 2,col = cols[4])
	lines(3:5, sMeans[l06[3:5],reqNams[1]], lwd = 2,col = cols[4], lty =2)
	# Add x-axis
	axis(1, at = 1:5, labels = 16:20)
		#Add CIS
	sapply(1:5, function(z) segments(x0 = z,y0 =sMeans[l03[z], reqNams[2]],
									  y1 = sMeans[l03[z], reqNams[3]], col = cols[3]) )
	sapply(1:5, function(z) segments(x0 = z,y0 =sMeans[l06[z], reqNams[2]],
									 y1 = sMeans[l06[z], reqNams[3]], col = cols[4]) )
	## Plot 3
	plot(1:5, sMeans[l98[4:8],reqNams[1]],
		 col = cols[2], type = "l", bty= "n", lwd = 2,
		 main = "C84 vs C87",
		 xlab = "Avg. Age", ylab = "Satisfaction",
		 ylim = range(sMeans[,reqNams[1]]), xlim = c(1,5), xaxt="n")
	# Plot cohort 03
	lines(1:3, sMeans[l03[4:6],reqNams[1]], lwd = 2,col = cols[3])
	lines(3:5, sMeans[l03[6:8],reqNams[1]], lwd = 2,col = cols[3], lty =2)
	# Add x-axis
	axis(1, at = 1:5, labels = 19:23)
	#Add CIS
	sapply(4:8, function(z) segments(x0 = z-3,y0 =sMeans[l98[z], reqNams[2]],
									 y1 = sMeans[l98[z], reqNams[3]], col = cols[2]) )
	sapply(4:8, function(z) segments(x0 = z-3,y0 =sMeans[l03[z], reqNams[2]],
									 y1 = sMeans[l03[z], reqNams[3]], col = cols[3]) )
	dev.off()
	}

#convert using parallel convert {} {.}.pdf ::: *.svg
# cite Tange 2011

library(googleVis)
library(car)
vis <- sMeans[, c(grep("\\.means$", names(sMeans) ), 50:53) ]
names(vis) <- gsub("\\.means$", "", names(vis))
#vis$col <- recode(vis$cohort, "'y95' = 'red'; 'y98' = 'blue'; 'y03' = 'green'; 'y06' = 'black'")
vis$GFC <- ifelse(vis$year < 2010, "pre", "post")
#op <- options(gvis.plot.tag='chart')
M <- gvisMotionChart(vis, 'cohort', 'year', colorvar='GFC',  xvar = 'age', yvar = 'general',
					 options=list(width=600, height=400))
#plot(M)
capture.output(print(M, 'chart'), file = "GFCweb/GFCchart.html")
