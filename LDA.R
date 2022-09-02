rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)

#================================#
####         Load data        ####
#================================#


#setwd("C:/Users/a37907/Desktop/KnowSandeel15781/Data")
#setwd("E:/KnowSandeel15781/Data")


#====   school, trawl, bio data   ====#
lapply(c("Data/sandeel.Rdata", "Data/School_EROS_bio.df", "Data/spdf.Rdata", "Data/School_SD.Rdata"),load,.GlobalEnv)

load("Data/School_EROS.Rdata")
School_EROS.dt <- School.dt
rm(School.dt)




#=======================================#
####      find Sandeel school        ####
#=======================================#

#== frequency response by category / area (use DT "joined") ==#
data = School_EROS.dt[category %in% c("SAND", "OTHER")] # rbind(School_EROS.dt, School_SD.dt)
frequency = c(18,38,70,120,200)
category = c("SAND" , "OTHER") #"SAND" / "PSAND" / "OTHER" /"KORONA" / "manual" 
aggregate(data$rf, list(data$Frequency, data$category),  sd)

ggplot(data=subset(data, Frequency %in% frequency & category %in% category & rf < 20 ), aes(x=Frequency, y = rf, colour=vessel)) + # & rf < 20
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  stat_summary(fun = mean, geom="point") + stat_summary(fun = mean, geom = "line") + #, linetype="dashed"
  #stat_summary(fun = mean, 
  #             fun.min = function(x) mean(x) - sd(x), 
  #             fun.max = function(x) mean(x) + sd(x), 
  #             geom = "errorbar", width = 7) +
  #stat_summary(fun = mean, geom="point") + stat_summary(data = subset (data, Frequency %in% c(38,200) & category %in% category & rf < 20) , fun = mean, geom = "line", size=1) + #  
  scale_x_continuous(breaks = c(38, 200)) + scale_y_continuous(breaks=c(-1, 1, 3, 5, 10)) + #, breaks=c(0.5, 1.0, 1.5) +
  facet_wrap(~category)+ #, scale="free_y"
  #  facet_wrap(~area, scale="free_y")+
  labs(x="Frequency", y="Frequency response")
#

#== r(f) for one school (specify "id") ==#
data = School_1032.dt
frequency = c(38,200)

ggplot(data=subset(data, id %in% 3487 & Frequency %in% frequency), aes(x=Frequency, y = rf )) + #colour=id, group=id
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_jitter(col="gray", size=0.5, shape=1,stroke = 1 ) + 
  geom_point() + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  scale_x_continuous(breaks = frequency) + # scale_y_continuous(limits = c(0.5, 1.5), breaks=c(0.5, 1.0, 1.5)) +
  labs(x="Frequency", y="Frequency response")
#

#== histogram of r(f) ==#
data = School_1032.dt
frequency = c(200)

ggplot(data=subset(data, Frequency %in% frequency ), aes(x = rf, colour=category, fill=category)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=0.1) + scale_x_continuous(limits = c(0, 7.5)) + geom_vline(xintercept = 1, linetype="dashed", alpha=.5) +
  #facet_wrap(~area, scales = "free_y")+
  #facet_grid(category~.)+
  #facet_wrap(~area, scale="free_y")+
  #annotate("text", label =paste("n="), size=4, x = Inf, y = Inf, hjust = 3, vjust = 7.5) + 
  #annotate("text", label =paste("n=", unique(joined$id.1)), size=4, x = Inf, y = Inf, hjust = 3, vjust = 7.5) + 
  labs(x="200kHz Frequency response", y="Number of school")
#

#!! mada !!#
School_EROS.dt %>% group_by(category, area) %>% mutate(N=n()) %>%
  mutate(N=ifelse(rf==max(rf,na.rm=T),paste0('n=',N),NA)) %>%
  ggplot(aes(x=rf, y=N, fill=category, label=N)) + scale_x_continuous(limits = c(0, 7.5)) + 
  geom_point()+
  #geom_histogram(bins = 1, binwidth=0.1) +
  geom_text(fontface='bold')+
  facet_wrap(~ area, nrow=3, scales = "free")
#xlab("")+
#scale_fill_manual(values = c("coral1", "lightcyan1", "olivedrab1"))+
#theme(legend.position="none")
#!! mada !!#


#== sV distribution (All) ==#
data <- rbind(School_EROS.dt, School_1032.dt, School_1031.dt)
frequency = c(200)

ggplot(data=subset(data, Frequency %in% frequency), aes(x=category, y=sV_mean)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(sV_mean), colour=vessel), alpha=.9) + #geom_point(aes(y=mean(sV_mean)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10() + #limits=c(1e-10,1e-03)
  labs(y="200 kHz sV mean")
#== sV distribution (above -82dB) ==#
sV_threshold <-10^(-82/10)
ggplot(data=data[Frequency==frequency & sV_mean > sV_threshold], aes(x=category, y=sV_mean)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + #geom_boxplot(aes(middle = mean(sV_mean)), alpha=.9) +geom_point(aes(y=mean(sV_mean)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10(limits=c(1e-10,1e-03))+ 
  #  facet_grid(.~category)+
  labs(y="sV mean")
#


#== Aspect ratio of school morphology ==#
data <- rbind(School_EROS.dt, School_1032.dt)
data$ratio <- data$PingNo/data$SampleCount
ggplot(data=subset(data, Frequency %in% c(200)), aes(x=category, y=ratio)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(ratio)), alpha=.9) +
  geom_point(aes(y=mean(ratio)), shape=1, size=3, col="red",stroke = 2 ) +
  #  facet_grid(.~category)+
  labs(y="aspect ration of school morphology")
#== r(f) vs school morphology aspect ratio (ratio) ==#
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(aes(x=rf, y=ratio, colour=category), size=1, shape=1) + 
  scale_x_continuous(limits = c(0, 4)) + 
  #facet_grid(.~category) +
  labs(y="ping number/sample count ratio", x="frequency response")
#

#== pixel number by category ==#
data <- rbind(School_EROS.dt)#School_1032.dt
ggplot(data=subset(data, Frequency %in% c(200)), aes(x=category, y=pixelNo)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(pixelNo)), alpha=.9) +
  #geom_point(aes(y=mean(pixelNo)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10()+
  #  facet_grid(.~category)+
  labs(y="pixel number")

#== r(f) vs pixel number (or aspect ratio) ==#
ggplot(data=subset(data, Frequency %in% c(200) ), aes(x=rf, y=pixelNo, col=category, fill=category)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point( size=1) + 
  scale_x_continuous(limits = c(0, 5)) + scale_y_continuous(limits = c(0,1000))+
  #facet_wrap(~area)+
  labs(x="200kHz Frequency response", y="pixel number")
#

#== school size by vehicles ==#
data <- rbind(School_EROS.dt,School_1032.dt, School_1031.dt)# School_1032.dt
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=school_area, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="school size (m2)")
#


#== school length by vehicles ==#
data <- rbind(School_EROS.dt, School_1032.dt)
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=school_length, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="school distance (m)")
#

#== sA by vehicles ==#
data <- rbind(School_EROS.dt)
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=sA, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="NASC")
#

#== school size vs frequency response ==#
data <- rbind(School_EROS.dt, School_1032.dt)
summary(data$school_area)
data$school_area_range <- cut(data$school_area, breaks=seq(0, 30870, 10), labels=as.character(seq(10, 30870, by=10)))
setDT(data)[, mean_rf := mean(rf), by = c("school_area_range", "Frequency", "category")]

category = c("SAND" ,"PSAND") #"SAND" / "PSAND" / "OTHER" /"KORONA" / "manual" 
ggplot(data=subset(data, Frequency %in% c(333) & category %in% category & mean_rf < 20)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point(aes(x=school_area_range, y=mean_rf)) +# scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() +
  facet_wrap(~category) + #, scale="free_y"
  labs(x="school size", y="frequency response")
#

#== route ==#
data <- rbind(sandeel.dt, School_EROS.dt[category%in%"SAND"]) #rbind(School_EROS.dt, School_SD.dt)
ggplot(data=subset(data, Frequency %in% 200 & !area %in% "outside")) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(),  axis.title = element_blank(), panel.spacing=unit(0, "lines"))+
  geom_point(aes(x=YMD_time, y=area, col=vessel)) + #facet_grid(area~.) + 
  labs(x="time")
#

#== chronological change ==#
data <- rbind(School_EROS.dt, School_SD.dt)
ggplot() + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point(aes(y=sA, x=YMD_time, col=category),data=subset(data, Frequency %in% c(200) & category %in% c("SAND", "KORONA", "manual")), shape=1) + scale_y_log10() + 
  #geom_point(aes(y=sA, x=YMD_time), data=subset(data,  Frequency %in% c(200) & category %in% c("SAND","KORONA", "manual")
  #& school_area >=100
  #& 10*log10(sV_mean) >= -55 & 10*log10(sV_mean) <= -40 
  #& rf >= 0.8 & rf<= 3.5)
  #), col="red")  +
  scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() +
  #facet_wrap(~category) + #, scale="free_y"
  labs(y="school NASC", x="")
#


#
#




#== proportion of each categories ==#
data = data.table(School_EROS.dt)
max(data$rf)
#EROS : 662739.7 -> 662740
#1032 : 65.0 -> 65.0
#
data$range <- cut(data$rf, breaks=seq(0, 662740, 0.1), labels=as.character(seq(0.1, 662740, by=0.1)))
setDT(data)[, sum := .N, by = c("range", "Frequency")][, prop := .N, by = c("range", "Frequency", "category")][, prop := prop/sum][, sum := NULL]
max(data$school_area)
#EROS : 9107.837
#1032 : 110120 max(110117)
#
data$range_size <- cut(data$school_area, breaks=seq(0, 9110, 10), labels=as.character(seq(10, 9110, by=10)))
setDT(data)[, sum := .N, by = c("range_size", "Frequency")][, prop_size := .N, by = c("range_size", "Frequency", "category")][, prop_size := prop_size/sum][, sum := NULL]
summary(10*log10(data$sV_mean))
#EROS : min(-102.71)   max(-12.68)
#1032 : 
#
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-103, -12, 1), labels=as.character(seq(-102, -12, by=1)))]
setDT(data)[, sum := .N, by = c("range_SV", "Frequency")][, prop_SV := .N, by = c("range_SV", "Frequency", "category")][, prop_SV := prop_SV/sum][, sum := NULL]
max(data$sA)
#EROS : 13027787
#1032 : 
#
data$range_sA <- cut(data$sA, breaks=seq(0, 13027787, 10), labels=as.character(seq(10, 13027787, by=10)))
setDT(data)[, sum := .N, by = c("range_sA", "Frequency")][, prop_sA := .N, by = c("range_sA", "Frequency", "category")][, prop_sA := prop_sA/sum][, sum := NULL]



x <- data.table(category=data$category, Frequency=data$Frequency, area=data$area, 
                range = data$range, prop=data$prop, range_size = data$range_size, prop_size=data$prop_size,
                range_SV = data$range_SV, prop_SV=data$prop_SV, range_sA=data$range_sA, prop_sA=data$prop_sA)
#x <- unique(x)
x$range <- as.numeric(as.character(x$range))
x$range_size <- as.numeric(as.character(x$range_size))
x$range_SV <- as.numeric(as.character(x$range_SV))
x$range_sA <- as.numeric(as.character(x$range_sA))



#== frequency response (rf) ==#
ggplot(data=subset(x, Frequency %in% c(200) & !category %in% "PSAND"), aes(x=range, y=prop, col=category)) + geom_vline(xintercept = 1, linetype="dashed", alpha=.5) +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 7.5))+
  #facet_wrap(~area)+
  labs(x="200kHz Frequency response", y="Proportion")
#


#== School size ==#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = pixelNo, colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=10) + scale_x_continuous(limits = c(0, 1000))+
  labs(x="School size", y="Number of school")
#
ggplot(data=subset(x, Frequency %in% c(200) & category %in% c("OTHER","SAND","PSAND")), aes(x=range_size, y=prop_size, col=category)) +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 1000))+
  #facet_wrap(~area)+
  labs(x="School size", y="Proportion")
#

#== SV_mean ==#
ggplot(data=subset(x, Frequency %in% c(38) ), aes(x=range_SV, y=prop_SV, col=category))  +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + #scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(-80, -35))+
  #facet_wrap(~area)+
  labs(x="38kHz mean SV", y="Proportion")
#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = 10*log10(sV_mean), colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=1) + scale_x_continuous(limits = c(-80, -35))+
  labs(x="mean SV", y="Number of school")
#

#== sA ==#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = sA, colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=10) + scale_x_continuous(limits = c(0, 1000))+
  labs(x="school sA", y="Number of school")
#
ggplot(data=subset(x, Frequency %in% c(200) ), aes(x=range_sA, y=prop_sA, col=category))  +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 1000))+
  #facet_wrap(~area)+
  labs(x="200kHz sA", y="Proportion")
#



#=======================================#
#===       find Sandeel school       ===#
#### Discriminant analyses (LDA/DFA) ####
#=======================================#


#library("tidyverse")
library("caret")
library(MASS)
#== pre-processing ==#
cols <- c("Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "area", "YMD_time", "altitude", 
          "meanDepth", "DepthStart", "DepthStop","nor_DepthStart", "nor_DepthStop", "nor_Depth",  "altitude_degree")
#,"SE","sV_stdv","school_length","school_height", "DepthStart", "DepthStop", "meanDepth" ,"sA", "altitude_degree", "Perimeter", "DepthfromBottom","Elongation", "school_rect", "school_circ"
data <- subset(School_EROS.dt, Frequency %in% c(200) 
               & !category %in% "PSAND"
               # & school_area >= median(School_EROS.dt$school_area)
               )
data[,cols]=NULL
data[, Frequency:=NULL]
data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]


#== plot (*take long time) ==#
#library("psych")
#pairs.panels(data[, -c("id","category")],bg=c("pink","blue", "green")[as.factor(data$category)], pch=21, gap=0)

#== Split the data into training and test set ==#
#training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
#train.data <- data[training.samples, ]
#test.data <- data[-training.samples, ]
train.data <- data
#== Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
data <- predict(preproc.param, data)
train.data <- predict(preproc.param, train.data)
#test.data <- predict(preproc.param, test.data)
train.data$category <- as.factor(train.data$category)
#test.data$category <- as.factor(test.data$category)
ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")
featurePlot(train.data[, 3:ncol(train.data)],as.factor(train.data$category), plot="density", auto.key = list(columns = 2))


#== saildrone data ==#
test_SD.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% c("KORONA", "manual") & !area %in% "outside")
test_SD.data[,cols]=NULL
test_SD.data[, Frequency:=NULL]
test_SD.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+2)][,school_height:=log(school_height)]
ggplot(melt(test_SD.data[,-c("id", "category")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")
#preproc.param <- preProcess(test_SD.data[,-c("id")], method = c("center", "scale"))
test_SD.data <- predict(preproc.param, test_SD.data)


#== step-wise LDA * 10,  Saildrone data ==#
library(ROCR)
maxvar <-(ncol(train.data))-2
direction <-"backward"
test_SD.data <- data.frame(test_SD.data)
score <- 0
score.df <- data.frame(matrix(ncol = 4, nrow = 0))
coef.df <- data.frame(matrix(ncol = 3, nrow = 0))
confusion.matrix <- list()
slda.lst <- list()

for(i in 1:10) {
  score <- 0
  slda <- train(category ~ ., data = train.data[, -c("id")],
                method = "stepLDA", importance = TRUE,metric="ROC", tuneLength=10,
                trControl = trainControl(method = "repeatedcv",number=10, repeats = 3, savePredictions = "final", classProbs = TRUE), #"repeatedcv" / "cv"
                tuneGrid=data.frame(maxvar,direction),
  )
  temp <- data.frame(id = test_SD.data$id)
  temp2 <- data.table(slda$finalModel$fit$scaling, keep.rownames = TRUE)
  temp2$attempt <- i
  coef.df <- rbind(coef.df, temp2)
  predictions <- predict(slda, train.data)
  temp3 <- confusionMatrix(reference=as.factor(train.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"train")]] <- temp3
  #predictions <- predict(slda, test.data)
  #temp3 <- confusionMatrix(reference=as.factor(test.data$category) , data= predictions, mode = "everything", positive = "SAND")
  #confusion.matrix[[paste0(i,"test")]] <- temp3
  slda.lst[[i]] <- slda
  predictions <- predict(slda, train.data, type="prob")
  pred <- prediction(predictions[2], train.data$category)
  png(paste0("ROC",i,".png") , width = 800, height = 600)
  plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate
  dev.off()
  for(j in 1:nrow(slda$finalModel$fit$scaling)) {
    score <- score + (slda$finalModel$fit$scaling[j] * test_SD.data[, row.names(slda$finalModel$fit$scaling)[j]])
  }
  temp$score <- score
  temp$pred <- predict(slda, test_SD.data)
  temp$attempt <- i
  score.df <- rbind(score.df, temp) 
  
}
rm(temp, temp2, temp3)
save(score.df, file="score.Rdata")
save(coef.df, file="coef.Rdata")
save(confusion.matrix, file="confusion.matrix.Rdata")
save(slda.lst, file="slda.lst.Rdata")
#


#== determine ids "SAND" ==#
ids <- score.df
ids$pred <- ifelse(score.df$pred=="SAND", 1, 0)
ids <- with(ids, aggregate(ids[,c("score", "pred")], list(id), mean))
ids <- data.table(ids)
ids <- ids[pred%in%1]$Group.1
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
test_SD.data$predict <- predict(slda.lst[[10]], test_SD.data)
featurePlot(test_SD.data[, 3:(ncol(test_SD.data)-1)],as.factor(test_SD.data$predict), plot="density", auto.key = list(columns = 2))

save(sandeel.dt, file="sandeel.Rdata")



#== biological data ==#
bio_id <- School_EROS_bio.df[, c("id", "meanLength", "weighted_meanLength")]
bio_id <- unique(bio_id)
x <- data.table(matrix(ncol = 4, nrow = 0))
for(i in 1:10) {
  temp <- data.table(id=train.data$id, pred = predict(slda.lst[[i]], train.data), data="training", attempt = i)
  #temp2  <- data.table(id=test.data$id, pred = predict(slda.lst[[i]], test.data), data="test", attempt = i)
  x <- rbind(x, temp, use.names=F)
  colnames(x) <- c("id", "pred", "data", "attempt")
}
x$pred_num <- ifelse(x$pred=="SAND", 1, 0)
x <- data.table(with(x, aggregate(x[,c("pred_num")], list(id), sum)))
colnames(x) <- c("id", "pred_num")
x$pred <- ifelse(x$pred_num==10, "SAND", "OTHER")

bio_id <- x[bio_id, on = "id", roll = TRUE]
bio_id$pred <- gsub('OTHER', 'Incorrect', bio_id$pred)
bio_id$pred <- gsub('SAND', 'Correct', bio_id$pred)

boxplot(meanLength~pred, data=bio_id)
ggplot(bio_id) + theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.title = element_blank())+
  geom_histogram(aes(x=meanLength, y = stat(density), fill=pred), bins=30, colour="grey50", alpha=0.5, position="dodge") +
  geom_density(aes(x=meanLength, fill=pred), colour="grey50", alpha=0.5) +
  labs(x="mean body length of a trawl catch (cm)", y="Density")
nrow(bio_id[pred=='Correct'])/nrow(bio_id)

#==  stat: mean length correctly vs incorrectly classified   ==#
summary(lm(meanLength~pred, data=bio_id))
#

#==  calculate accuracy ==# (schools larger than 500m2)
eros.lda <- data.table(id=train.data$id, pred=predict(slda.lst[[1]], train.data))
eros.lda <- merge(x = School_EROS.dt, y = eros.lda, by = "id", all.x = TRUE)
eros.lda <- eros.lda[Frequency %in% 200 & !category%in%"PSAND" & !pred%in%NA]
eros.lda <- eros.lda[, c("id", "category","pred","YMD_time", "PingNo","school_area", "DepthStart", "DepthStop", "meanDepth", "area")]
nrow(eros.lda[category == pred & school_area >=500])/nrow(eros.lda[school_area >=500])





#== step-wise Discriminant analyses 1 time ==#
maxvar <-(ncol(train.data))-2
direction <-"backward"
slda1 <- train(category ~ ., data = train.data[, -c("id")],
               method = "stepLDA", importance = TRUE,metric="ROC", tuneLength=10,
               trControl = trainControl(method = "repeatedcv",number=10, repeats = 3, savePredictions = "final", classProbs = TRUE), #"repeatedcv" / "cv"
               tuneGrid=data.frame(maxvar,direction),
)
slda1$finalModel
varImp(slda1)
slda1
slda1$finalModel$fit
coef <- data.table(slda1$finalModel$fit$scaling, keep.rownames = TRUE)

predictions.test <- predict(slda1, test.data) #, type="prob"
mean(predictions.test==test.data$category)
confusionMatrix(reference=as.factor(test.data$category) , data= predictions.test, mode = "everything", positive = "SAND")

library(ROCR)
predictions.test <- predict(slda, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate



#==========================================#
#===         find Sandeel school        ===#
#### Discriminant analyses (with 18kHz) ####
#==========================================#

#== pre-processing ==#
data <- subset(School_EROS.dt, Frequency %in% c(200, 18) 
               & !category %in% "PSAND"
               # & school_area >= median(School_EROS.dt$school_area)
)
data[, rf_18 := rf[Frequency==18], by=id]
data <- subset(data, Frequency %in% c(200))
data[,cols]=NULL
data[, Frequency:=NULL]
data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
data[,rf_18:=log(rf_18)]

#== Split the data into training and test set ==#
training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]

#== Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
data <- predict(preproc.param, data)
train.data <- predict(preproc.param, train.data)
test.data <- predict(preproc.param, test.data)
train.data$category <- as.factor(train.data$category)
test.data$category <- as.factor(test.data$category)
ggplot(melt(data[,-c("id")]), aes(value)) + geom_histogram(bins=30) + facet_wrap(~variable, scales="free_x")
featurePlot(train.data[, 3:ncol(train.data)],as.factor(train.data$category), plot="density", auto.key = list(columns = 2))


#== step-wise Discriminant analyses 1 time ==#
maxvar <-(ncol(train.data))-2
direction <-"backward"
slda1 <- train(category ~ ., data = train.data[, -c("id")],
               method = "stepLDA", importance = TRUE,metric="ROC", tuneLength=10,
               trControl = trainControl(method = "repeatedcv",number=10, repeats = 3, savePredictions = "final", classProbs = TRUE), #"repeatedcv" / "cv"
               tuneGrid=data.frame(maxvar,direction),
)
slda1$finalModel
varImp(slda1)
slda1
slda1$finalModel$fit
coef <- data.table(slda1$finalModel$fit$scaling, keep.rownames = TRUE)
predictions.test <- predict(slda1, test.data) #, type="prob"
mean(predictions.test==test.data$category)
confusion.matrix <- confusionMatrix(reference=as.factor(test.data$category) , 
                                    data= predictions.test, 
                                    mode = "everything", 
                                    positive = "SAND")
score <- data.frame(id = test.data$id, score = NA, pred = predict(slda1, test.data))
for(j in 1:nrow(slda1$finalModel$fit$scaling)) {
  for(i in 1:nrow(test.data)) {
    temp <- slda1$finalModel$fit$scaling[j] * test.data[[i, row.names(slda1$finalModel$fit$scaling)[j]]]
    score[i, 2] <- as.numeric(temp)
  }
}

save(coef, file="Data/LDAresults_with18kHz/coef.Rdata")
save(slda1, file="Data/LDAresults_with18kHz/slda1.Rdata")
save(confusion.matrix, file="Data/LDAresults_with18kHz/confusion.matrix.Rdata")
save(score, file="Data/LDAresults_with18kHz/score.Rdata")

library(ROCR)
predictions.test <- predict(slda1, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE) #tpr:true prediction rate, fpr:false prediction rate


#===========================#

































































































































































































































































































#========================================================================================================================#
#### test code                                                                                                        ####
#========================================================================================================================#


#replace NaN by interpolating previous and next non-NaN observation values
#library("zoo")
#bottom.dt$BottomDepth <- zoo::na.approx(bottom.dt$BottomDepth) 




#temp_dt = data.table(matrix(nrow=1))                                
#temp_dt = data.table(matrix(rep(NA, 1), nrow=1))[numeric(0), ]   ## create empty data table with 61columns
#SvSchool.dt = data.table(matrix(rep(NA, 1), nrow=1))[numeric(0), ]   ## create empty data table with 61columns

#== create empty data table
temp_dt <- data.table()   
SvSchool.dt <- data.table()
#== This pattern will be used in loop when extracting school ID from file name
#== pattern : extract strings between "SvSchool" and "_T2019"
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"

#== Read file name and directory of text data
#files <- list.files('S2019847_PEROS_3317/EXPORT/Sv_SAND_2.11.0-rc1',pattern='txt$', full.names=TRUE)
files <- list.files('S2019847_PEROS_3317/EXPORT/test', pattern='txt$', full.names=TRUE)

#== read file(i) and process row by row(r)
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "SAND", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  
  for (r in 1:nrow(temp)){
    range_bin <- (temp$RangeStop[r]-temp$RangeStart[r])/temp$SampleCount[r]   ## calculate range bin
    depth_bin <- (temp$DepthStop[r]-temp$DepthStart[r])/temp$SampleCount[r]   ## calculate depth bin
    
    for (s in 1:temp$SampleCount[r]) {
      Sv <- as.character(paste0("Sv", s))                                     ## create char "Sv1","Sv2","Sv3"...depends on (r)
      temp_dt <- cbind (temp[r,1:13])                                         ## fetch column 1:13 of text data and insert to the empty datatable
      temp_dt$sampleNo <- Sv                                                  ## add column that tells "Sv1","Sv2","Sv3"...
      temp_dt$rangeStart <- temp$RangeStart[r] + (range_bin * (s-1))          ## calculate and input range start of "Sv1"
      temp_dt$rangeStop <- temp_dt$rangeStart + range_bin                     ## calculate and input range stop of "Sv1"
      temp_dt$depthStart <- temp$DepthStart[r] + (depth_bin * (s-1))          ## calculate and input depth start of "Sv1"
      temp_dt$depthStop <- temp_dt$depthStart + depth_bin                     ## calculate and input depth stop of "Sv1"
      temp_dt$Sv <- as.numeric(temp[r,Sv,with=FALSE])                         ## add column of sV values of row(r) and sampleNo(s)
      SvSchool.dt <- rbind(SvSchool.dt, temp_dt,fill=TRUE)                    ## combine to Svschool.dt
    }
    SvSchool.dt <- SvSchool.dt[!is.na(SvSchool.dt$Sv), ]                      ## remove rows if sV value = NA
    
  }
  
}

# check id number
for(i in 1:length(files)) {
  temp <- rbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp, fill=TRUE)
}
#
write.csv(temp, file="1031_noise.csv")


SvSchool.dt <- 
  SvSchool.dt %>%
  group_by(id, PingNumber, SampleNo) %>% 
  mutate(rf=Sv[Frequency == 38]/Sv)


SvMean.dt <- as.data.table(aggregate(SvSchool.dt$Sv, FUN=mean, by=list(id=SvSchool.dt$id,frequency=SvSchool.dt$Frequency)))
SvMean.dt <- SvMean.dt[order(id),]
SvMean.dt <- 
  SvMean.dt %>% 
  group_by(id, sampleNo) %>% 
  mutate(rf=x[frequency == 38]/x)


#== Distance between pings ==#
dist <- data.table(id= SvSchool_1032.dt$id, PingNumber=SvSchool_1032.dt$PingNumber, YMD_time=SvSchool_1032.dt$YMD_time, Latitude=SvSchool_1032.dt$Latitude, Longitude=SvSchool_1032.dt$Longitude)
dist <- unique(dist, by = c("PingNumber"))
dist <- dist[order(PingNumber),]
library(geosphere)
dist$distance[2:nrow(dist)] <- sapply(2:nrow(dist), function(x) distm(dist[x-1, c('Longitude', 'Latitude')], dist[x, c('Longitude', 'Latitude')], fun = distHaversine))
dist[, distance := replace(distance, 1, distance[2]), by = id]
colSums(is.na(dist))
dist[is.na(dist)] <- mean(dist$distance, na.rm = TRUE) #1 ping school will have mean(distance of all ping distance)
colSums(is.na(dist))
dist <- data.table(PingNumber=dist$PingNumber, distance = dist$distance)
#== merge distance with SvSchool.dt ==#
SvSchool_1032.dt <- merge(x = SvSchool_1032.dt, y = dist, by = "PingNumber", all.x = TRUE)
setDT(SvSchool_1032.dt)[, pixel_size := distance*depth_bin]
setDT(SvSchool_1032.dt)[, school_area := sum(pixel_size), by = c("id", "Frequency")]
rm(dist)
#== School horizontal length ==#
dist <- data.table(id= SvSchool_1032.dt$id, PingNumber=SvSchool_1032.dt$PingNumber, Latitude=SvSchool_1032.dt$Latitude, Longitude=SvSchool_1032.dt$Longitude)
dist <- unique(dist, by = c("PingNumber"))
dist_1 <- setDT(dist)[, head(.SD,1), by = id]
dist_2 <- setDT(dist)[, tail(.SD,1), by = id]
dist <- rbind(dist_1, dist_2)
dist <- dist[order(PingNumber),]
library(geosphere)
dist$distance[2:nrow(dist)] <- sapply(2:nrow(dist), function(x) distm(dist[x-1, c('Longitude', 'Latitude')], dist[x, c('Longitude', 'Latitude')], fun = distHaversine))
dist <- setDT(dist)[, tail(.SD,1), by = id]
dist <- data.table(id=dist$id, school_length = dist$distance)
SvSchool_1032.dt <- merge(x = SvSchool_1032.dt, y = dist, by = "id", all.x = TRUE)
rm(dist, dist_1, dist_2)
#

#== coverage number ==#
School.dt <- School.dt[order(vessel, area, Frequency, YMD_time),]
School.dt <- School.dt[order(vessel, area, YMD_time),]
setDT(School.dt)[, time_diff:=as.numeric(difftime(School.dt$YMD_time, lag(School.dt$YMD_time), units = "hours"))]
setDT(School.dt)[, coverage:=as.numeric(1)]
School.dt$coverage <- as.numeric(School.dt$coverage)

for (i in 2:nrow(School.dt)){
  if (School.dt$area[i]!="outside" && School.dt$time_diff[i]>=24)
    School.dt$coverage[i] <- School.dt$coverage[(i-1)] + 1
  
  else if (School.dt$area[i]!="outside" && School.dt$area[i]!=School.dt$area[(i-1)] && 
           nrow(filter(School.dt[i:(i+49)], area==School.dt$area[i]))>=40 && 
           School.dt$area[i]!= School.dt[coverage==coverage[i-1]]$area[1])
    
    School.dt$coverage[i] <- School.dt$coverage[(i-1)] + 1
  
  else if (School.dt$area[i]=="outside")
    School.dt$coverage[i] <- "F"
  else
    School.dt$coverage[i] <- School.dt$coverage[(i-1)]
  
}

School.dt[, .(cnt= sum(.N)), by= c("coverage", "area")]
ggplot() +theme_bw(base_size=15) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text = element_text(size=5,), axis.ticks.length  = unit(1, "mm"), strip.text = element_text(size = 7))+geom_path(data = School.dt[Frequency%in%200], aes(Longitude, Latitude, colour=as.factor(coverage)))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + facet_wrap(~coverage, scales="free")
setDT(School.dt)[, time_diff:=NULL]



ggplot(data=subset(SvSchool.dt, Frequency %in% c(18, 38, 70,120, 200)), aes(x=Frequency, y = rf)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_point(data=subset(SvSchool.dt, Frequency %in% c(38, 200)), aes(x=Frequency, y = rf )) + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  stat_summary(fun = mean, geom="point") + stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, fun.min = function(x) mean(x) - sd(x), fun.max = function(x) mean(x) + sd(x), geom = "errorbar", width = 7) +
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) + scale_y_continuous(limits = c(0, 2.5)) + #, breaks=c(0.5, 1.0, 1.5)
  facet_grid(category~area)+
  labs(x="Frequency", y="Frequency response")
#
ggplot(data=subset(SvSchool.dt, id %in% c(5681)), aes(x=Frequency, y = rf )) + #colour=id, group=id
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_jitter(col="gray", size=0.5, shape=1,stroke = 1 ) + 
  geom_point() + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  #  geom_point(data=subset(SvSchool.dt, Frequency %in% c(38, 200)), aes(x=Frequency, y = rf )) + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) + # scale_y_continuous(limits = c(0.5, 1.5), breaks=c(0.5, 1.0, 1.5)) +
  #  facet_wrap(~id, scale="free_y") +
  facet_grid(.~category)+
  labs(x="Frequency", y="Frequency response")
#



##== check Sv value and frequency response of one school ==##
temp.dt <- as.data.table(read.table('S2019_SAILDRONE_1032/EXPORT/notuse_PSAND/SvSchool1615_T20190430_00595792-20190819_18193333_01.txt', header = TRUE, sep = ","))
temp.dt <- melt(temp.dt, id = 1:11)
temp.dt <- temp.dt[!is.na(temp.dt$value), ]
setnames(temp.dt, c("variable", "value"), c("SampleNo","Sv"))
temp.dt$depth_bin <- (temp.dt$DepthStop- temp.dt$DepthStart)/temp.dt$SampleCount
temp.dt$depthStart <- temp.dt$DepthStart + (temp.dt$depth_bin * (temp.dt$SampleCount-1))
temp.dt$depthStop <- temp.dt$depthStart + temp.dt$depth_bin

setDT(temp.dt)[, sV := 10^(Sv/10)][, sV38 := sV[Frequency==38], by = c("PingNumber", "SampleNo")][, sV_mean := mean(sV), by = c("Frequency")][, rf := sV_mean/sV_mean[Frequency==38]]
setDT(temp.dt)[, PingNo := max(PingNumber)-min(PingNumber) + 1][, sV_stdv := sd(sV), by = c("Frequency")][, SE_f := sV_stdv/sqrt(PingNo*SampleCount), by = c("Frequency")][, SE := SE_f/mean(sV38), by = c("Frequency")]

#plot(data=temp.dt, rf~Frequency)
#aggregate(rf ~ Frequency, data=temp.dt, mean)

axis <- data.frame(y1=c(min(temp.dt$Sv), max(temp.dt$Sv)), y2=c(min(temp.dt$rf), max(temp.dt[temp.dt$Frequency!=333,]$rf)))
A2D_summary <- summary(lm(formula = y1 ~ y2, data = axis)) 
A2DInt <- A2D_summary$coefficients[1, 1] 
A2DSlope <- A2D_summary$coefficients[2, 1]
D2A_summary <- summary(lm(formula = y2 ~ y1, data = axis)) 
D2AInt <- D2A_summary$coefficients[1, 1]
D2ASlope <- D2A_summary$coefficients[2, 1]
ggplot(data=subset(temp.dt,Frequency%in%c(18,38,70,120,200)),aes(x=Frequency, y=Sv) ) + geom_jitter(shape=1, size=1) + 
  geom_point(aes(y=rf*A2DSlope+A2DInt), col="red") + geom_line(aes(y=rf*A2DSlope+A2DInt), col="red") + scale_y_continuous("SV", sec.axis = sec_axis(~.*D2ASlope+D2AInt, name="Frequency response"),)+
  scale_x_continuous(breaks = c(18, 38, 70, 120, 200, 333)) +  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
#
rm(axis, A2D_summary, A2DInt, A2DSlope, D2A_summary, D2AInt, D2ASlope)





#== check each polygon ==#
#library("sp")
#point.in.polygon(test$x,test$y,polygon_EK$longitude,polygon_EK$latitude)
file1 <- "StratumPolygon.geojson"
library(geojsonio)
spdf <- geojson_read(file1,  what = "sp")
#plot(spdf, col="grey")
#
library(broom)
spdf_fortified <- tidy(spdf)
spdf_fortified <- mutate (spdf_fortified, area = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                                            id=="3" ~ "Inner_Shoal_East_2016", id=="4" ~ "Inner_Shoal_North",
                                                            id=="5" ~ "Inner_Shoal_West_2018", id=="6" ~ "Inner_Shoal_test",
                                                            id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                                            id=="9" ~ "Outer_Shoal", id=="10" ~ "VestbankenSouthEast",
                                                            id=="11" ~ "VestbankenSouthWest", id=="12" ~ "Vestbanken_North",
                                                            TRUE ~ "Vikingbanken"))
spdf_fortified <- mutate (spdf_fortified, area2 = case_when (id=="1" ~ "AlbjoernLing", id=="2" ~ "Engelsk_Klondyke",
                                                             id=="3" ~ "Inner_Shoal", id=="4" ~ "Inner_Shoal",
                                                             id=="5" ~ "Inner_Shoal", id=="6" ~ "Inner_Shoal",
                                                             id=="7" ~ "Nordgyden", id=="8" ~ "Ostbanken",
                                                             id=="9" ~ "Outer_Shoal", id=="10" ~ "Vestbanken",
                                                             id=="11" ~ "Vestbanken", id=="12" ~ "Vestbanken",
                                                             TRUE ~ "Vikingbanken"))


#==biological data from English Klondyke ==#
setwd("C:/Users/komiy/Desktop/jugyo/BIO399_MSc/Data")
library(data.table)
load("Biological_data/EngKlondyke2019_biodata.Rdata")
# Load the attached file: conatin station, catch & individual info.
sta.ind <- merge(sta2,ind1)
sta.ind$week <- week(sta.ind$date)
# Some output
len.dist.week <- tapply(sta.ind$length, list(sta.ind$length, sta.ind$week), length)
boxplot(sta.ind$length ~sta.ind$week, xlab="Week number", ylab="Length (cm)")

#== calculate distance from lat lon ==#
gridDim <- acos(sin(pi*(min(test$LATITUDE))/180.0)*sin(pi*(max(test$LATITUDE))/180.0)+
                  cos(pi*(min(test$LATITUDE))/180.0)*cos(pi*(max(test$LATITUDE))/180.0)*
                  cos(pi*(min(test$LONGITUD))/180.0-pi*(max(test$LONGITUD))/180.0))*6378


#   error txt.file from 1032
notuse_school.dt <- data.table(longitude=c(3.911060,3.991163,3.987633,3.979058,4.013532,3.992762,3.992107,
                                           3.991143,3.991148,3.991693,3.991150,3.991385,3.907003,3.907187,3.907920,
                                           3.927997,3.967090,3.906043,3.902615,3.902937,3.921058,3.919362,3.912782,
                                           3.972690,3.918762,3.917150,3.916098,3.914552,3.911685,3.912162), 
                               latitude=c(56.770847,56.750652,56.755875,56.760690,56.668268,56.736295,56.739108,
                                          56.749097,56.748453,56.743565,56.750112,56.745763,56.736165,56.739842,56.739290,
                                          56.807508,56.811503,56.735167,56.731685,56.732008,56.787138,56.777103,56.773018,
                                          56.783767,56.765828,56.763973,56.762858,56.761250,56.757800,56.758347),
                               id=c(1615,3355,3358,3359,3360,3363,3364,3368,3369,3370,3371,3372,3382,3384,3385,3386,
                                    3391,3392,3393,3394,3400,3403,3404,3407,3408,3409,3410,3411,3412,3413))
pos <- data.frame(id=data$id, Latitude=data$Latitude, Longitude=data$Longitude)
library("ggthemes") # for theme_map
w <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data = w) + geom_sf() + theme_bw(base_size=15) +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  coord_sf(xlim = c(0, 8), ylim = c(56, 61), expand = FALSE)+ #, datum = NA
  annotate(geom = "text", x = 7, y = 60, label = "Norway", fontface = "bold", color = "grey80", size = 10)+
  geom_polygon(data = spdf, aes(long, lat, group=area, fill=area), colour="black", alpha =0.3) + 
  #  geom_point(data=fishstation.dt, aes(x=longitudestart, y=latitudestart), size=1, shape=1, col="blue")
  geom_point(data=notuse_school.dt, aes(x=longitude, y=latitude), size=.8, shape=1)+
  labs(x="Longitude", y="Latitude")



#============================#
####   biological data    ####
#============================#
#== read data + marge with station data  ==#
bio.dt <- as.data.table(read.table("S2019847_PEROS_3317/BIOLOGY/CATCH_MEASUREMENTS/BIOTIC/individual.txt", header = TRUE, sep = "\t"))
fishstation.dt <- as.data.table(read.table("S2019847_PEROS_3317/BIOLOGY/CATCH_MEASUREMENTS/BIOTIC/fishstation.txt", header = TRUE, sep = "\t"))
setnames(fishstation.dt, c("serialnumber"), c("f.serialnumber"))

bio.dt <- fishstation.dt[bio.dt, on = "f.serialnumber", roll = TRUE]
bio.dt <- Filter(function(x)!all(is.na(x)), bio.dt)
bio.dt <- bio.dt[, !duplicated(as.list(bio.dt)), with = FALSE]
setDT(bio.dt)[, Longitude := (longitudeend+longitudestart)/2][, Latitude := (latitudeend+latitudestart)/2]
setDT(bio.dt)[,length_cm := (ifelse(lengthresolution==2, length*100, length*10))]


#== time ==#
#library(stringr)
#setDT(sta.ind_1)[,mo:=str_sub(sta.ind_1$stationstartdate,6,7)][, da:=str_sub(sta.ind_1$stationstartdate,9,10)][,Time_start:=paste(startyear, mo, da, sep="-")]
#setDT(sta.ind_1)[,Time_start:=paste(Time_start, stationstarttime, sep=" ")]
#setDT(sta.ind_1)[,Time_start:=as.POSIXct(sta.ind_1$Time_start, "%Y-%m-%d %H:%M", tz= "UTC")]
#setDT(sta.ind_1)[,Time_end:=paste(startyear, mo, da, sep="-")][,Time_end:=paste(Time_end, stationstoptime, sep=" ")]
#setDT(sta.ind_1)[,Time_end:=as.POSIXct(sta.ind_1$Time_end, "%Y-%m-%d %H:%M", tz= "UTC")][,mo:=NULL][,da:=NULL]










#================================#
####   find Sandeel school    ####
#================================#


data <- subset(School_EROS.dt, Frequency %in% 200)
summary(10*log10(data$sV_mean))
#EROS : -83.98(Min). -17.60(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-84, -17, 1), labels=as.character(seq(-83, -17, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
table(data$category)


data_1032 <- subset(School_1032.dt, Frequency %in% 200)
summary(10*log10(data_1032$sV_mean))
#1032 : -75.02(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-76, -20, 1), labels=as.character(seq(-75, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
table(data_1032$category)


#== Is the school size reasonable? ==#
# EROS #
nrow(data[data$category=="SAND"])/length(unique(School_EROS.dt[category=="SAND"]$id)) # detection rate
# saildrone #
sum(data_1032$sA)/sum(School_1032.dt[Frequency==200]$sA)


#== frequency response(rf) ==#
# EROS #
#test <- data[rf>= 1]
test <- data[rf>= 0.8 & rf<= 3.5] #rf>= 0.85 & rf<= 1.6
nrow(test[category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[rf>= 0.8 & rf<= 1.6])/nrow(data_1032) # % of sandeel
table(data_1032[rf>= 0.8 & rf<= 1.6]$category)


#== mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -54 & SV_mean<= -43])/nrow(data_1032)
table(data_1032[SV_mean >= -54 & SV_mean<= -43]$category)


#== frequency response(rf) + mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40 &  rf >= 0.8 & rf<= 3.5]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[Frequency==200 & category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6])/nrow(data_1032)
table(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6]$category)

data <- data.table(School_EROS.dt)
summary(10*log10(data$sV_mean))
#EROS : -102.71(Min). -12.68(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-102, -12, 1), labels=as.character(seq(-101, -12, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
data <- subset(data, Frequency %in% 200)

data_1032 <- data.table(School_1032.dt)
summary(10*log10(data_1032$sV_mean))
#1032 : -82.78(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-82, -20, 1), labels=as.character(seq(-81, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
data_1032 <- subset(data_1032, Frequency %in% 200)

#== frequency response(rf) + school size ==#
#= EROS =#
test <- data[school_area > 100 & rf >= 0.8  & rf<= 1.7]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND" ])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= saildrone =#
nrow(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7])/nrow(data_1032)
sum(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7]$sA)/sum(data_1032$sA) # sum(sA)/sum(sA from all sandeel schools)

#== frequency response(rf) + school size + mean_SV ==#
#= EROS =#
test <- data[SV_mean >= -55 & SV_mean<= -40  &  rf >= 0.7 & rf<= 1.8]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND"])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= SAILDRONE =#
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  pixelNo > 324 &  rf >= 0.9 & rf<= 1.6])/nrow(data_1032)

#== mean_SV + school size ==#
test <- data[Frequency==200 & SV_mean >= -55 & SV_mean<= -45 &  school_area > 100 ]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
#table(test$category)
nrow(test[test$category=="SAND"])/length(unique(School_EROS.dt$id[School_EROS.dt$category=="SAND"])) # exclusion % of sandeel schools
#= SAILDRONE =#


test <- subset(School_EROS.dt[,-c("id","category","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "sV_stdv","area", "SE", "YMD_time", "altitude")])
test2 <-  melt(test)
ggplot(test2, aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")



cols <- as.character(slda$finalModel$model$name)
cols <- append(cols,c("id", "category"))
data <- subset(School_EROS.dt, Frequency %in% c(200)& !category %in% "PSAND")#& !category %in% "PSAND"
data[, setdiff(names(data), cols) := NULL][]
cols_log <- cols[!cols %in% c("DepthStop","meanDepth","DepthfromBottom", "altitude_degree", "id", "category")]
data[,(cols_log):=lapply(.SD, log),.SDcols=cols_log]
#Split the data into training and test set:
training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
#Estimate pre-processing parameters
preproc.param <- train.data[,-c("id")] %>% preProcess(method = c("center", "scale"))
#Transform the data using the estimated parameters
train.data <- predict(preproc.param, train.data)
test.data <- predict(preproc.param, test.data)




#==discriminant analysis==#
#== Fit the model ==#
formula <- as.character(slda$finalModel$formula[3])
formula <- paste0("category~", formula)
model <- lda(as.formula(formula), data = train.data[,-c("id")]) #qda, mda, fda
plot(model, dimen=1, type="both") #plot of dimension 1
#library(devtools)
# Make predictions
pred.train <- predict(model, train.data)
pred.test <- predict(model, test.data)
# Model accuracy
mean(pred.train$class==train.data$category)
table(train.data$category, pred.train$class)
#mean(pred.test$class==test.data$category)
#table(test.data$category, pred.test$class)
library(ROCR)
pred <- prediction(pred.test$posterior[,2], test.data$category) # 2 class->[,2], 3 class->[,3]
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate

#library(ggord)
#ggord(model, train.data$category, xlim=c(-7, 7), ylim=c(-7.5,5))

#== determine ids "SAND" ==#
apply_SD <-  predict(slda, test_SD.data)
predict(slda, test.data, type="prob")
test_SD.data$predict <- apply_SD
nrow(test_SD.data[predict=="SAND"])/nrow(test_SD.data)
table(test_SD.data$predict)
ids <- test_SD.data[predict %in% c("SAND")]$id
#write.csv(ids, "ids_9.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#featurePlot(test_SD.data[, 3:19],as.factor(test_SD.data$predict), plot="density", auto.key = list(columns = 2))

#== "KORONA" + "manual" ==#
test_SD_KM.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% c("KORONA","manual") & !area %in% "outside")
test_SD_KM.data[,cols]=NULL
test_SD_KM.data[, Frequency:=NULL]
test_SD_KM.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+2)][,school_height:=log(school_height)]
test_SD_KM.data <- predict(preproc.param, test_SD_KM.data)
test_SD_KM.data$predictions <- predict(slda.lst[[1]], test_SD_KM.data)
table(test_SD_KM.data$predictions, test_SD_KM.data$category)
ids <- test_SD_KM.data[predictions%in%"SAND"]$id
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)




#=========================#
#### Logistic analysis ####
#=========================#
cols <- c("id","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "area", "YMD_time", "altitude", "nor_Depth", 
          "weighted_meanDepth")
#"SE","sV_stdv","school_length","school_height", "DepthStart", "DepthStop", "meanDepth" ,"sA", "altitude_degree", "Perimeter", "DepthfromBottom", "Elongation", "school_rect", "school_circ",
data <- subset(School_EROS.dt, Frequency %in% 200 & !category %in% "PSAND" & school_area >= median(School_EROS.dt$school_area))#& !category %in% "PSAND"
data[,cols]=NULL
data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
#Split the data into training and test set:
training.samples <- data$category %>%
  createDataPartition(p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. Categorical variables are automatically ignored.
# SNAD -> 1, OTHER -> 0 
train.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
test.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")


#model fit
#train.data$category <- as.factor(train.data$category)
lr <- train(as.double(category)~., data = train.data, family=binomial(), importance = TRUE,
            trControl = trainControl(method = "repeatedcv",number=10, repeats = 5, savePredictions = "final", classProbs = TRUE),
            method = "glmStepAIC")
lr$results
lr$finalModel
varImp(lr$finalModel)
plot(lr, type=c("g", "o"))

predictions.test <- ifelse(predict(lr, test.data)> 0.5, 1, 0)
mean(predictions.test==test.data$category)
confusionMatrix(reference=as.factor(test.data$category) , data= as.factor(predictions.test), mode = "everything", positive = as.character("1"))

library(ROCR)
predictions.test <- predict(slda, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate



#model <- glm(as.numeric(category)~., data=train.data, family = binomial())
train.data[, predict := predict(model, type="response")][,predict := ifelse(predict> 0.5, 1, 0)]
sum(train.data$category == train.data$predict)/nrow(train.data)
table(train.data$category, train.data$predict)
#test data
test.data[, predict := stats::predict(model, type="response", newdata = test.data)][,predict := ifelse(predict> 0.5, 1, 0)]
sum(test.data$category == test.data$predict)/nrow(test.data)
table(test.data$category, test.data$predict)
#find cutoff value using ROC



#apply to saildron data
test_SD.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% "KORONA" & !area %in% "outside")
test_SD.data[,cols]=NULL
test_SD.data[, Frequency:=NULL]
test_SD.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
ggplot(melt(test_SD.data[,-c("id", "category")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")
test_SD.data[, predict := ifelse(predict(lr, test_SD.data)> 0.5, "SAND", "OTHER")]
table(test_SD.data$predict)

ids <- test_SD.data[predict %in% "SAND"]$id
write.csv(ids, "ids_LR_1.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#




ids <- School_EROS.dt[category %in% c("SAND") & Frequency %in% 200 & !area %in% "outside"
                      #& school_area >=100
                      #& 10*log10(sV_mean) >= -55 & 10*log10(sV_mean) <= -40 
                      #& rf >= 0.8 & rf<= 3.5
                      #& area %in% c("Engelsk_Klondyke") # "Engelsk_Klondyke" / "Inner_Shoal_East_2016","Inner_Shoal_North", "Inner_Shoal_West_2018", "Inner_Shoal_test"
]$id
sandeel.dt <-  subset(School_EROS.dt, id %in% ids & Frequency %in% 200)



### horizontal distribution plot ###
data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #sandeel.dt #& school_area >= median(School_EROS.dt$school_area)
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- mutate (data, month = case_when (month=="04"~"April", month=="05"~"May", month=="06"~"June", month=="07"~"July~", TRUE ~"July~"))
data <- mutate (data, area = case_when (area=="AlbjoernLing" ~ "AlbjoernLing", area=="VestbankenSouthEast" ~ "Vestbanken",area=="VestbankenSouthWest" ~ "Vestbanken", area=="Vestbanken_North" ~ "Vestbanken",area=="Vikingbanken" ~ "Vikingbanken", area=="Engelsk_Klondyke" ~ "Engelish Klondyke",area=="Inner_Shoal_East_2016" ~ "Inner Shoal", area=="Inner_Shoal_North" ~ "Inner Shoal",area=="Inner_Shoal_West_2018" ~ "Inner Shoal", area=="Inner_Shoal_test" ~ "Inner Shoal",area=="Nordgyden" ~ "Nordgyden", area=="Ostbanken" ~ "Ostbanken", area=="Outer_Shoal" ~ "Outer Shoal",TRUE ~ "NA"))
setDT(data)[, scale_lat:= scale(Latitude)[,1], by=area][, scale_lon:=scale(Longitude)[,1], by = area ]

ggplot(data=data) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  geom_point(aes(x=scale_lat, y=log(sA)),size=1, shape=1) + # + scale_y_continuous(trans = log_trans(), breaks = trans_breaks("log", function(x) exp(x)),labels = trans_format("log", math_format(e^.x))) + 
  geom_smooth(method="lm", formula=log(y) ~ x, se=F, aes(x=scale_lat, y=sA)) + #,colour="grey", linetype = "dashed"
  #facet_grid(area~factor(month, levels=c("April", "May", "June", "July~"))) + #, ncol=1, scales = "free_x", strip.position="right"
  coord_flip() + 
  labs(x="Latitude", y="mean NASC of Sandeel (200kHz)")
#

#== center of gravity ==#
center_of_mass <- function(x,y,w){
  c(crossprod(x,w)/sum(w), crossprod(y,w)/sum(w))}
center_of_mass(data$Latitude, data$Longitude, data$sA)



#acos(sin(pi*(Latitude)/180.0)*sin(pi*(Latitudestart)/180.0)+cos(pi*(Latitude)/180.0)*cos(pi*(Latitudestart)/180.0)*cos(pi*(Longitude)/180.0-pi*(Longitudestart)/180.0))*6378
