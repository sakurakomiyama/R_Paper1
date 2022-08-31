
#temporal <- vrtl

## Before running lines 1-70 run everything below line 70 to get access to the functions
## Some functions needed
logitcomm <- function(t, alpha, betta, D, sym=T){
  #browser()
  if (sym) t <- t - 2*(t-12)*(t>12) # reflected around 12
  tmp <- exp((t-betta)*alpha)
  tmp/(1+tmp)*D-D
}

time2hourMin <- function(x){
  hour <- floor(x)
  min <- floor((x-hour)*60)
  as.data.frame(cbind(hour,min))
}


## Start test ----
source('nls/divaSunrise.r', echo=TRUE)
## Use plankton data from Sakura
#load(file="vrtl.Rdata")
#x <- vrtl$EK_200.df
x <- data.table(data[vessel %in% c("SD1031", "SD1032") ]) #[vessel %in% c("SD1031", "SD1032") ] #& area_2 %in% () & month%in%c("06")
x <- mutate (x, area_id = case_when (area_2=="AlbjoernLing" ~ "1", area_2=="Engelish Klondyke" ~ "2",area_2=="Inner Shoal" ~ "3", area_2=="Nordgyden" ~ "4",area=="Ostbanken" ~ "5", area=="Outer Shoal" ~ "6",area=="Vestbanken" ~ "7", area=="Vikingbanken" ~ "8", TRUE ~ "0"))

#x<- subset(x, x$altitude_degree > -1)

## Inspect positions
#plot(x$Longitude, x$Latitude, col=x$area_id) #1032


## Please read help est.nls (see below). Need defined variables
## I am testing for x$weighted_PLANK_Depth_10m
l <- data.table(label=c("Weighted mean depth (m)", "Mean depth (m)", "Normalised mean depth", "Normalised minimum depth", "Normalised maximum depth", "school height (m)"),
                predictor=c("weighted_meanDepth", "meanDepth", "nor_Depth", "nor_DepthStart", "nor_DepthStop", "school_height"))

C <- "school_height" # weighted_meanDepth / nor_Depth / nor_DepthStart / nor_DepthStop / school_height / meanDepth
x$Catch <- x[[C]]#*100
x$stratum <- 1 # x$area_id / 1
x$row.id <- 1:nrow(x)
x$zero.exclude <- rep(FALSE,nrow(x))
##

ggplot() + theme_bw(base_size = 15) + theme(panel.grid = element_blank(), axis.text = element_text(size=12), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(), strip.text.y = element_text(size=8)) + 
  geom_point(aes(x=altitude_degree, y=Catch, colour=category), shape=1, size=1, alpha=0.8, data=x)+ 
  scale_y_reverse()+
  facet_grid(area_2~factor(month_name, levels = c("April", "May", "June", "July~")), scales="free_y") +
  xlab("Solar altitude (°)") + ylab(l[predictor%in%C]$label)
#

## Test by area 
# tmp <- x
# x <- tmp
# x <- x[x$area_id == 6,]


# Structure the data frame with correct variables
utc.tmp <- as.data.frame(cbind(lubridate::year(x$YMD_time),lubridate::month(x$YMD_time),lubridate::day(x$YMD_time),lubridate::hour(x$YMD_time),lubridate::minute(x$YMD_time),x$Longitude,(x$Latitude), x$Catch, x$stratum, x$zero.exclude, x$row.id ))
names(utc.tmp) <- c("year","month","day","hour","min","lon","lat","catch","stratum","zero.exclude","row.id")
utc.tmp$zero.exclude <- 0
utc.tmp <- mutate_all(utc.tmp, function(x) as.numeric(as.character(x)))


## Include altitude of Sun (I had a code available that I use (see below). You can use your solution
#utc.tmp$x <- as.vector(unlist(alt.of.sun(x=utc.tmp)[1]))
utc.tmp$catch <- utc.tmp$catch*-1
utc.tmp$x <- x$altitude_degree

# Inspect the data
boxplot(utc.tmp$catch ~ round(utc.tmp$x), xlab="Altitude of sun", ylab = "Center of depth gravity")
a1 <-tapply(utc.tmp$catch, round(utc.tmp$x),median)
#plot(as.numeric(names(a1)),a1) #, ylim=c(-45,-30)


## Model fit
## I have used this method in 
# Johnsen, E., and Iilende, T. 2007. Factors affecting the diel variation in commercial CPUE of Namibian hake - Can new information improve standard survey estimates? Fisheries Research, 88: 70â€“79.
# Johnsen, E., and Godo, O. R. 2007. Diel variations in acoustic recordings of blue whiting (Micromesistius poutassou). Ices Journal of Marine Science, 64: 1202â€“1209.

#z1#z2====est.nls==============================================================
# Function for estimating Alpha, Beta, Dd and mu in the function logit.nls 
# Input:
# Data: a data.frame that must contain the following columns:
# 	catch, stratum, x, zero.exclude, row.id
# a.fix, Alpha: ALpha=C where C is a number. If a.fix=TRUE, Alpha is not 
#	estimated, but set equal to C. If a.fix=FALSE, C is used as start value
#	for Alpha in the nls algorithm.
# b.fix, Beta: same as for a.fix, Alpha.
# timebased: TRUE: local time of day is used. FALSE: altitude of sun is used.
#

tt <- est.nls(utc.tmp, a.fix=F, b.fix=F, timebased=F, Alpha=0.5, Beta=15)
#nor_Depth all month alpha=0.7, beta=15
#utc.tmp_1 <- utc.tmp[utc.tmp$stratum==1,]
#tt1 <- est.nls(utc.tmp_1, a.fix=F, b.fix=F, timebased=F, Alpha=tt$Alpha, Beta=tt$Beta)

## Get the results
list(tt$abd.pars, tt$mu, tt$Rsq, tt$fixedpars)


## plot ##
library(grid)
Model <- data.frame(altitude=min(utc.tmp$x):max(utc.tmp$x), fit=logitcomm(t=min(utc.tmp$x):max(utc.tmp$x), alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F))
Model$fit2 <- Model$fit+tt$mu
Obs <- data.frame(altitude=tapply(round(utc.tmp$x), round(utc.tmp$x),max) , obs=tapply(utc.tmp$catch, round(utc.tmp$x),median))
coef <- grobTree(textGrob(paste(paste("\U03B1=",round(tt$Alpha,2)),
              paste("\U03B2=",round(tt$Beta,2)),
              paste("D =",round(tt$Dd,2)),
              paste("R2 =", round(tt$Rsq[1],2)), 
              paste("p =", round(as.numeric(tt$abd.pars[3,4]),3)),
              sep = "\n"),x=0.05,  y=0.15, hjust=0,
              gp=gpar(col="black", fontsize=10))) #
utc.tmp$vessel <- x$vessel

height <- 
ggplot() + theme_bw() + theme(panel.grid = element_blank(),  axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + 
  geom_point(aes(x=x, y=catch, colour=vessel), shape=1, size=1, alpha=0.5, data=utc.tmp)+ #, col="black"
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_point(aes(x=altitude, y=obs), data=Obs, alpha=1, shape=1, col="red", stroke=1.1) + 
  geom_path(aes(x=altitude, y=fit2), data=Model) +
  annotation_custom(coef)+
  #scale_y_continuous(limits = c(-100, 0))+
  xlab("Solar altitude (°)") + ylab(l[predictor%in%C]$label)
#
library(ggpubr)
ggarrange(mean, min, max, height, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom") #, norm + rremove("x.text")


## Plot models
xx <- -20:60
plot(xx,logitcomm(t=xx, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F),xlab="Solar altitude (°)", ylab = "Depth difference (m)", cex.lab=1.5, type="b", cex=0.8, lwd=2)
plot(tt$results$x, tt$results$fit, col=as.factor(tt$results$stratum))
a1 <-tapply(utc.tmp$catch, round(utc.tmp$x),median)
plot(as.numeric(names(a1)),a1, ylim=c(-60,-5))
plot(utc.tmp$catch ~ utc.tmp$x)
plot(aggregate(utc.tmp$catch ~ round(utc.tmp$x), data=utc.tmp, median))
lines(tt$results$x, tt$results$fit)



#
#
#




### TEsting (Only for Espen)
test <- function(){
  x <- read.table("../Data/FinalData98.dat",header=T)
  x <- x[x$strata=="year=1998month=11dyp=300lat=1",]
  x$Catch <- x$KgHour
  x$t <- x$localtime
  x$stratum <- paste0("year=",x$year,"month=",x$month,"day=",x$day,"Vessel=",x$VesselID)
  x$row.id <- 1:nrow(x)
  ###
  x$stratum <- paste(x$year,x$month,x$day,x$VesselID)
  tmp <- x
  
  ## I have called it tmp.dat
  #tmp <- read.table("tmp.dat",header=T)
  tmp$x <- tmp$loct
  tmp$catch <- log(tmp$CPUE)
  stratum.var <- function(x){
    y <- numeric(length(x))
    for(i in 1:length(unique(x))){
      n.i <- 1:length(unique(x))
      y[x==unique(as.character(x))[i]] <- n.i[i]
    }
    y
  }
  tmp$stratum <- stratum.var(tmp$stratum)
  tmp$zero.exclude <- rep(FALSE,nrow(tmp))
  utc.tmp <- cbind(tmp$year,tmp$month,tmp$day,time2hourMin(tmp$localtime)["hour"],time2hourMin(tmp$localtime)["min"],tmp$longitudeStart,-abs(tmp$latitudeStart),tmp$catch,tmp$stratum,tmp$zero.exclude, tmp$row.id )
  names(utc.tmp) <- c("year","month","day","hour","min","lon","lat","catch","stratum","zero.exclude","row.id")
  #head(utc.tmp) <- utc.local(utc.tmp,tolocal=F)
  #    browser()
  tmpTime <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]
  
  
  ## Altitude of Sun
  utc.tmp$x <- as.vector(unlist(alt.of.sun(x=utc.tmp)[1]))
  tmpSun <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]
  
  est.nls(tmpTime,a.fix=F,b.fix=F,timebased=T,Alpha=0.96,Beta=8)
  est.nls(utc.tmp,a.fix=T,b.fix=F,timebased=T,Alpha=0.96,Beta=8)
  
  ## Denne
  tt <- est.nls(utc.tmp,a.fix=T,b.fix=F,timebased=F,Alpha=0.08,Beta=3)
}



xx <- -60:80
tt$Alpha

plot(xx,logitcomm(t=xx, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F))
lines(xx,logitcomm(t=xx, alpha=ans$values[1], betta=ans$values[3], D=ans$values[3], sym=F),lwd=3)
lines(xx,logitcomm(t=xx, alpha=ans$values[1], betta=ans$values[3], D=ans$values[3], sym=F),lwd=3)




#### test (sakura) ###
plot(xx,logitcomm(t=xx, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F),xlab="Solar altitude (°)", ylab = "Depth difference (m)", cex.lab=1.5, type="b", cex=0.8, lwd=2)
#lines(xx, logitcomm(t=xx, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F), cex.lab=1.5, type="b", col="blue", cex=0.8, lwd=2)
#legend(12, 9.5, lwd=2:2, lty=1:1,legend = c(expression(paste("D = -9.8,  ",alpha," = 0.2,  ",beta," = 0" )),
#                                expression(paste("D = -8.1,  ",alpha," =  1  ,  ",beta," = 0" ))),
#                                bty = "n", col=c("black", "blue"),  cex=1.2)


