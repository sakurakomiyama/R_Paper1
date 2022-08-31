
temporal <- vrtl$Transect82_38.df #[vrtl$Transect82_200.df$area=="west",]
temporal <- vrtl$EK_200.df[vrtl$EK_200.df$weighted_sandeel_PDMEAN!=0,] # #[vrtl$EK_200.df$weighted_sandeel_PDMEAN!=0,]
temporal <- xx

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
x <- temporal   #[temporal$longitude<4.5,]

## Update Mission
#x$Mission <- 1
#x$Mission[x$YMD_time >= as.POSIXct('2019-05-10 02:24:00', tz="UTC")] <- 2
#x$Mission[x$YMD_time >= as.POSIXct('2019-05-15 00:08:00', tz="UTC")] <- 3
#x$Mission[x$YMD_time >= as.POSIXct('2019-05-18 00:17:00', tz="UTC")] <- 4
#x$Mission[x$YMD_time >= as.POSIXct('2019-05-26 09:08:00', tz="UTC")] <- 5
#x$Mission[x$YMD_time >= as.POSIXct('2019-05-28 16:57:00', tz="UTC")] <- 6
#Aberdeen
names(x)[names(x) == 'TRANSECT'] <- 'Mission'
x$Mission <- sapply(x$Mission, function(x) gsub("T", "", x))
x$Mission <- as.numeric(x$Mission)
#English Klondyke
#names(x)[names(x) == 'i.Mission'] <- 'Mission'
x$Mission <- sapply(x$Mission, function(x) gsub("May01-03", "1", x))
x$Mission <- sapply(x$Mission, function(x) gsub("May12-16", "2", x))
x$Mission <- sapply(x$Mission, function(x) gsub("June12-14", "3", x))
x$Mission <- sapply(x$Mission, function(x) gsub("June15-20", "4", x))
x$Mission <- as.numeric(x$Mission)
#1032#
x$Mission <- x$area_id


## Inspect positions
plot(x$LONGITUD, x$LATITUDE, col=x$Mission)
plot(x$Longitude, x$Latitude, col=x$Mission) #1032


## Please read help est.nls (see below). Need defined variables
## I am testing for x$weighted_PLANK_Depth_10m
x$Catch <- x$weighted_meanDepth #load R.Data : weighted_PLANK_Depth_10m, Aberdeen : weighted_PDMEAN_10m, EnglishKlondyke : weighted_PLANK_PDMEAN_10m
x$stratum <- x$Mission
x$row.id <- 1:nrow(x)
x$zero.exclude <- rep(FALSE,nrow(x))
##

## Test by mission 
# tmp <- x
# x <- tmp
# x <- x[x$Mission == 6,]


# Structure the data frame with correct variables
utc.tmp <- as.data.frame(cbind(lubridate::year(x$YMD_time),lubridate::month(x$YMD_time),lubridate::day(x$YMD_time),lubridate::hour(x$YMD_time),lubridate::minute(x$YMD_time),x$LONGITUD,(x$LATITUDE),x$Catch,x$stratum,x$zero.exclude, x$row.id ))
#1032
utc.tmp <- as.data.frame(cbind(lubridate::year(x$YMD_time),lubridate::month(x$YMD_time),lubridate::day(x$YMD_time),lubridate::hour(x$YMD_time),lubridate::minute(x$YMD_time),x$Longitude,(x$Latitude),x$Catch,x$stratum,x$zero.exclude, x$row.id ))
names(utc.tmp) <- c("year","month","day","hour","min","lon","lat","catch","stratum","zero.exclude","row.id")

## Include altitude of Sun (I had a code available that I use (see below). You can use your solution
#utc.tmp$x <- as.vector(unlist(alt.of.sun(x=utc.tmp)[1]))
utc.tmp$catch <- utc.tmp$catch*-1
utc.tmp$x <- x$altitude_degree

# Inspect the data
boxplot(utc.tmp$catch ~ round(utc.tmp$x), xlab="Altitude of sun", ylab = "Center of depth gravity")
a1 <-tapply(utc.tmp$catch, round(utc.tmp$x),median)
plot(as.numeric(names(a1)),a1) #, ylim=c(-45,-30)

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
utc.tmp_1 <- utc.tmp[utc.tmp$stratum==1,]
tt1 <- est.nls(utc.tmp_1, a.fix=F, b.fix=F, timebased=F, Alpha=tt$Alpha, Beta=tt$Beta)

## Get the results
#tt
list(tt$abd.pars, tt$mu, tt$Rsq, tt$fixedpars)


## Plot models
xx <- -20:60
plot(xx,logitcomm(t=xx, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F),xlab="Solar altitude (°)", ylab = "Depth difference (m)", cex.lab=1.5, type="b", cex=0.8, lwd=2)
plot(tt$results$x, tt$results$fit, col=as.factor(tt$results$stratum))
a1 <-tapply(utc.tmp$catch, round(utc.tmp$x),median)
plot(as.numeric(names(a1)),a1, ylim=c(-60,-5))
plot(utc.tmp$catch ~ utc.tmp$x)
plot(aggregate(utc.tmp$catch ~ round(utc.tmp$x), data=utc.tmp, median))
lines(tt$results$x, tt$results$fit)

## Hit

## plot ##
Model_10m.df <- data.frame(altitude=-20:50, fit=logitcomm(t=-20:50, alpha=tt$Alpha, betta=tt$Beta, D=tt$Dd, sym=F), stratum="all")
test <- data.frame(altitude=tt1$results$x, fit=tt1$results$fit, stratum="May01-03") #May01-03
test2 <- data.frame(altitude=tt2$results$x, fit=tt2$results$fit, stratum="May12-16") #May12-16
test3 <- data.frame(altitude=tt3$results$x, fit=tt3$results$fit, stratum="June12-14") #June12-14
test4 <- data.frame(altitude=tt4$results$x, fit=tt4$results$fit, stratum="June15-20") #June15-20
#test5 <- data.frame(altitude=tt5$results$x, fit=tt5$results$fit, stratum="T5")
#test6 <- data.frame(altitude=tt6$results$x, fit=tt6$results$fit, stratum="T6")
Model_10m.df <- rbind(Model_10m.df, test, test2, test3, test4) #, test5, test6

Obs_10m.df <- data.frame(altitude=tapply(round(utc.tmp$x), round(utc.tmp$x),max) , obs=tapply(utc.tmp$catch, round(utc.tmp$x),median), stratum="all")
test <- data.frame(altitude=tapply(round(utc.tmp_1$x), round(utc.tmp_1$x),max) , obs=tapply(utc.tmp_1$catch, round(utc.tmp_1$x),median), stratum="May01-03") #May01-03
test2 <- data.frame(altitude=tapply(round(utc.tmp_2$x), round(utc.tmp_2$x),max) , obs=tapply(utc.tmp_2$catch, round(utc.tmp_2$x),median), stratum="May12-16") #May12-16
test3 <- data.frame(altitude=tapply(round(utc.tmp_3$x), round(utc.tmp_3$x),max) , obs=tapply(utc.tmp_3$catch, round(utc.tmp_3$x),median), stratum="June12-14") #June12-14
test4 <- data.frame(altitude=tapply(round(utc.tmp_4$x), round(utc.tmp_4$x),max) , obs=tapply(utc.tmp_4$catch, round(utc.tmp_4$x),median), stratum="June15-20") #June15-20
#test5 <- data.frame(altitude=tapply(round(utc.tmp_5$x), round(utc.tmp_5$x),max) , obs=tapply(utc.tmp_5$catch, round(utc.tmp_5$x),median), stratum="T5")
#test6 <- data.frame(altitude=tapply(round(utc.tmp_6$x), round(utc.tmp_6$x),max) , obs=tapply(utc.tmp_6$catch, round(utc.tmp_6$x),median), stratum="T6")
Obs_10m.df <- rbind(Obs_10m.df, test, test2, test3, test4) #, test5, test6

coef_10m.df <- data.frame(a=round(tt$Alpha,2), b=round(tt$Beta,2), D=round(tt$Dd,2), stratum="all")
test <- data.frame(a=round(tt1$Alpha,2), b=round(tt1$Beta,2), D=round(tt1$Dd,2), stratum="May01-03") #May01-03
test2 <- data.frame(a=round(tt2$Alpha,2), b=round(tt2$Beta,2), D=round(tt2$Dd,2), stratum="May12-16") #May12-16
test3 <- data.frame(a=round(tt3$Alpha,2), b=round(tt3$Beta,2), D=round(tt3$Dd,2), stratum="June12-14") #June12-14
test4 <- data.frame(a=round(tt4$Alpha,2), b=round(tt4$Beta,2), D=round(tt4$Dd,2), stratum="June15-20") #June15-20
#test5 <- data.frame(a=round(tt5$Alpha,2), b=round(tt5$Beta,2), D=round(tt5$Dd,2), stratum="T5")
#test6 <- data.frame(a=round(tt6$Alpha,2), b=round(tt6$Beta,2), D=round(tt6$Dd,2), stratum="T6")
coef_10m.df <- rbind(coef_10m.df, test, test2, test3, test4) #, test5, test6

#== when use dataset with top10m layer ==#
Model.df <- Model_10m.df
Obs.df <- Obs_10m.df
coef.df <- coef_10m.df

#== y axis scaling (For merged data) ###
a <- Model_10m.df[Model_10m.df$stratum=="all",]$fit
axis<-data.frame(y1=c(-40-(tt$Dd/2), -40+(tt$Dd/2)), y2=c(max(a), min(a))) #max and min of Depth(y1) + Altitude(y2)
A2D_summary <- summary(lm(formula = y1 ~ y2, data = axis)) #Regression of (A)ltitude to (D)epth:
A2DInt<-A2D_summary$coefficients[1, 1] #retrieve intercept 
A2DSlope<-A2D_summary$coefficients[2, 1] #retrieve slope
D2A_summary <- summary(lm(formula = y2 ~ y1, data = axis)) #Regression of (D)epth to (A)ltitude:
D2AInt<-D2A_summary$coefficients[1, 1] #retrieve intercept
D2ASlope<-D2A_summary$coefficients[2, 1] #retrieve slope

tmp <- utc.tmp
#tmp$stratum <- sub("^", "T", tmp$stratum )
tmp <- mutate(tmp, stratum=case_when(stratum==1~"May01-03",stratum==2~"May12-16", stratum==3~"June12-14",TRUE~"June15-20"))
test <- utc.tmp
test$stratum <- "all"
tmp <- rbind(tmp, test)

ggplot() + theme_bw(base_size = 20) + theme(panel.grid = element_blank(), axis.text = element_text(size=12), axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank()) + 
#  geom_point(aes(x=altitude, y=obs, colour=stratum), data=Obs.df, shape=1) + geom_line(aes(x=altitude, y=fit, colour=stratum), data=Model.df) +
#== for data with top 10m ==#
#  geom_point(aes(x=altitude, y=obs), data=Obs.df[Obs.df$stratum!="all",], shape=1, col="grey") + #[Obs.df$stratum!="all",]
#  geom_line(aes(x=altitude, y=fit), data=Model.df[Model.df$stratum!="all",], col="grey", linetype="dashed") + #
#== for data without top 10m ==#
#  geom_point(aes(x=x, y=catch), shape=1,size=0.9, alpha=0.3, data=tmp[tmp$stratum!="all",])+
  geom_point(aes(x=altitude, y=obs), shape=1, data=Obs_10m.df[Obs_10m.df$stratum!="all",]) + #
#   facet_grid(~stratum) + 
  facet_grid(~factor(stratum, levels = c("all", "May01-03", "May12-16", "June12-14", "June15-20"))) + 
#  geom_path(aes(x=altitude, y=fit*A2DSlope+A2DInt), data=Model_10m.df[Model_10m.df$stratum=="all",]) + scale_y_continuous(sec.axis = sec_axis(~.*D2ASlope+D2AInt)) +
  geom_line(aes(x=altitude, y=fit), data=Model_10m.df[Model_10m.df$stratum!="all",]) +
  geom_text(aes(x=3, y=-76, label=paste("D==",D)), parse = TRUE, size=4,data = coef_10m.df[coef_10m.df$stratum!="all",]) + 
  geom_text(aes(x=0, y=-79, label=paste('alpha', "==", a)), parse = TRUE, size=4,data = coef_10m.df[coef_10m.df$stratum!="all",]) + 
  geom_text(aes(x=2, y=-82, label=paste('beta', "==", b)), parse = TRUE, size=4,data = coef_10m.df[coef_10m.df$stratum!="all",]) + 
#  annotate("text",x=55, y=-21, label =paste("R2=",round(tt$Rsq[1],2)), size=4) + 
#  annotate("text",x=55, y=-24, label=paste("p=",round(as.numeric(tt$abd.pars[3,4]),3)), size=4) + 
#  scale_y_continuous(limits = c(-50, -13))+
  scale_color_discrete("Transect")+ #scale_y_continuous(limits = c(-65, -20))+
  labs(x="Solar altitude (°)" , y="Weighted mean depth of sandeel (m)")
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


