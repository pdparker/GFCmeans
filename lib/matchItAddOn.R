library(ggplot2); library(MatchIt)
meandifftable<- function (x){
  post<-data.frame(x$sum.matched[4])
  matchID <- as.vector (row.names (post) )
  names(post)[1]<-c("m_mean_diff")
  post$absolute<- abs(post[1])
  total2<-post[order (-post$absolute, na.last=NA) ,]
  meandiffover1<- subset(total2[1], total2[1]> .1 | total2[1]< -.1)
  meandiffover1
}

all_meandiffplot <- function (x) {
  adiff<-data.frame(x$sum.all)
  names(adiff)[4]<-c("all_mean_diff")
  diffplot<-ggplot(adiff, aes(all_mean_diff) )
  diffplot<- diffplot+ geom_histogram (fill="grey")
  diffplot<- diffplot+ geom_density (colour="red")
  diffplot<-diffplot+xlim(-.5, .5)
  diffplot
}

matched_meandiffplot <- function (x) {
  mdiff<-data.frame(x$sum.matched)
  names(mdiff)[4]<-c("matched_mean_diff")
  diffplot<-ggplot(mdiff, aes(matched_mean_diff) )
  diffplot<- diffplot+ geom_histogram (fill="grey")
  diffplot<- diffplot+ geom_density (colour="red")
  diffplot<-diffplot+xlim(-.5, .5)
  diffplot
}

all_meandiffcount<-function (x){
  all<-data.frame(x$sum.all[4])
  all$all_group[all[1] > .25]<- "Large"
  all$all_group[all[1] < -.25] <- "Large"
  all$all_group[all[1] > .20 & all[1] < .25 ] <- "Medium"
  all$all_group[all[1] < -.20 & all[1] > -.25] <- "Medium"
  all$all_group[all[1] < .20 & all[1] > .00]<- "Small"
  all$all_group[all[1] > -.20 & all[1] < .00] <- "Small"
  table(all$all_group)
}

matched_meandiffcount<-function (x){
  matched<-data.frame(x$sum.matched[4])
  matched$matched_group[matched[1] > .25]<- "Large"
  matched$matched_group[matched[1] < -.25] <- "Large"
  matched$matched_group[matched[1] > .20 & matched[1] < .25 ] <- "Medium"
  matched$matched_group[matched[1] < -.20 & matched[1] > -.25] <- "Medium"
  matched$matched_group[matched[1] < .20 & matched[1] > .00]<- "Small"
  matched$matched_group[matched[1] > -.20 & matched[1] < .00] <- "Small"
  table(matched$matched_group)
}