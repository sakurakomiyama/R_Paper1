setwd("C:/Users/a37907/Desktop/Sandeel/Data")
#setwd("E:/KnowSandeel15781/Data")
library(dplyr)
library(data.table)
library(ggplot2)


#=======================================#
#              read data                # 
#=======================================#

#== create empty data table
SvSchool.dt <- data.table()
#== This pattern will be used in loop when extracting school ID from file name
#== pattern : extract strings between "SvSchool" and "_T2019"
pattern <- "SvSchool\\s*(.*?)\\s*_T2019"

#== Read file name and directory of text data
files <- list.files('EXPORT/EROS',pattern='txt$', full.names=TRUE)
tmp <- list.files('EXPORT/SD1031',pattern='txt$', full.names=TRUE)
files <- c(files, tmp)
#files <- list.files('S2019847_PEROS_3317/EXPORT/test', pattern='txt$', full.names=TRUE)

#== read SAND data file(i) ==#
for(i in 1:length(files)) {
  temp <- as.data.table(read.table(header = TRUE, files[i], sep = ","))     ## read text data
  temp <- cbind(category = "SAND", temp)                                    ## add column "category" and put "SAND"
  temp <- cbind(id = as.numeric(regmatches(files[i], regexec(pattern, files[i]))[[1]][2]), temp) ## add column "id" and fetch school ID from file name
  temp <- melt(temp, id.vars = c(1:13))
  temp$vessel <- basename(dirname(files[i]))
  SvSchool.dt <- rbind(SvSchool.dt, temp, fill=TRUE)
  temp <- data.table()
}

SvSchool.dt <- SvSchool.dt[!is.na(SvSchool.dt$value), ]
#save(SvSchool.dt, file = "Svschool_SAND.Rdata")
rm(files, i, pattern, temp, tmp)
names(SvSchool.dt)[names(SvSchool.dt) == 'variable'] <- 'SampleNo'
names(SvSchool.dt)[names(SvSchool.dt) == 'value'] <- 'Sv'
SvSchool.dt$Sample.number <- as.numeric(gsub("Sv", "", SvSchool.dt$SampleNo))
SvSchool.dt <- subset(SvSchool.dt, Frequency==200)

#== bottom depth data ==#
library("rjson")
#bottom.json <- fromJSON(file="S2019_SAILDRONE_1032/EXPORT/bottomEchogramPlot_T20190430_00595792-20190819_18193333.json") #SD1032
#EROS
bottom.json <- fromJSON(file="bottomEchogramPlot_T20190423_16561743-20190512_19512565.json") #EROS
#== Distance between pings ==#
bottom.dt <- data.table(PingNumber = bottom.json$pingNumber,  PingDistance = as.numeric(bottom.json$vesselDistance),BottomDepth = as.numeric(bottom.json$lowerLayerBoundary))
setDT(bottom.dt)[, Diff:=c(0, diff(bottom.dt$PingDistance))][, distance:= Diff*1852][,Diff:=NULL][,PingDistance:=NULL][,vessel:="EROS"]
#SD1031
bottom.json <- fromJSON(file="bottomEchogramPlot_T20190424_10291908-20190820_12575243.json")  #SD1031
#== Distance between pings ==#
temp <- data.table(PingNumber = bottom.json$pingNumber,  PingDistance = as.numeric(bottom.json$vesselDistance),BottomDepth = as.numeric(bottom.json$lowerLayerBoundary))
setDT(temp)[, Diff:=c(0, diff(bottom.dt$PingDistance))][, distance:= Diff*1852][,Diff:=NULL][,PingDistance:=NULL][,vessel:="SD1031"]
bottom.dt <- rbind(bottom.dt, temp)
rm(temp, bottom.json)

#== merge bottom data and Sv data ==#
SvSchool.dt <- merge(x = SvSchool.dt, y = bottom.dt, by = c("PingNumber",'vessel'), all.x = TRUE)


#== depth_bin of pixel ==#
SvSchool.dt$depth_bin <- (SvSchool.dt$DepthStop- SvSchool.dt$DepthStart)/SvSchool.dt$SampleCount
SvSchool.dt$depthStart <- SvSchool.dt$DepthStart + (SvSchool.dt$depth_bin * (as.numeric(gsub("Sv", "", SvSchool.dt$SampleNo))-1))
SvSchool.dt$depthStop <- SvSchool.dt$depthStart + SvSchool.dt$depth_bin

#== School size(area), School length ==#
setDT(SvSchool.dt)[, pixel_size := distance*depth_bin]
setDT(SvSchool.dt)[, school_area := sum(pixel_size), by = c("id", "Frequency")][,school_length := distance/.N, by = c("id", "Frequency","PingNumber")][, school_length := sum(school_length), by = c("id", "Frequency")]

#== mean depth ==#
SvSchool.dt$sV <- 10^(SvSchool.dt$Sv/10)
setDT(SvSchool.dt)[, sV_nmi := sV*4*pi*(1852)^2][, sV_nmi := sV_nmi * depth_bin]
setDT(SvSchool.dt)[, sV_nmi_sample := mean(sV_nmi), by=c("id", "Frequency", "SampleNo")]
setDT(SvSchool.dt)[, meanDepth := mean(c(DepthStart, DepthStop)), by=c("id", "Frequency")]
setDT(SvSchool.dt)[, weighted_meanDepth := weighted.mean((depthStart+(depth_bin/2)),sV_nmi_sample), by=c("id", "Frequency", "PingNumber")]

#== depth from bottom ==#
setDT(SvSchool.dt)[, nor_Depth := weighted_meanDepth/BottomDepth][, DepthfromBottom := BottomDepth-DepthStop]
setDT(SvSchool.dt)[, nor_DepthStart := DepthStart/BottomDepth][, nor_DepthStop := ifelse(DepthStop/max(BottomDepth)>1 , 1 , DepthStop/max(BottomDepth)), by=c("id","Frequency")]


#=======================================#
#             make figure               # 
#=======================================#
# chose which school (id) to be plotted
unique(SvSchool.dt$id)
x <- 23117
dat <- subset(SvSchool.dt, id == x)

ggplot(dat, aes(x=PingNumber, y=depthStart)) + theme_bw(base_size=10) + theme(panel.grid = element_blank(), legend.key = element_blank()) +
  scale_y_reverse(limits=c(max(dat$BottomDepth), dat$DepthStart-10)) + geom_tile(colour="red", lwd=2) + geom_tile(aes(fill=Sv)) +
  geom_line(aes(x=PingNumber, y=BottomDepth)) + 
  geom_segment(aes(x=dat$PingNumber[max.col(t(dat$depthStart))], y= min(dat$BottomDepth), 
                   xend= dat$PingNumber[max.col(t(dat$depthStart))], yend= max(dat$depthStop)), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  geom_segment(aes(x=min(PingNumber)-1, y= DepthStart, 
                   xend=min(PingNumber)-1, yend= max(dat$depthStop)), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  geom_segment(aes(x=min(PingNumber)-1, y= min(depthStart), 
                   xend=dat[which.min(dat$depthStart),]$PingNumber, yend=min(depthStart)), 
               linetype="dashed") +
  geom_segment(aes(x=min(PingNumber)-1, y= max(depthStop), 
                   xend=dat[which.max(dat$depthStart),]$PingNumber, yend=max(depthStop)), 
               linetype="dashed") +
  
  geom_segment(aes(x=min(PingNumber), y= DepthStart-3, 
                   xend=max(PingNumber), yend= DepthStart-3), 
               arrow = arrow(length = unit(0.5, "cm"), ends = "both")) + 
  geom_segment(aes(x=min(PingNumber), y= dat[which.min(dat$PingNumber),]$depthStart, 
                   xend=min(PingNumber), yend=DepthStart-3), 
               linetype="dashed") +
  geom_segment(aes(x=max(PingNumber), y= dat[which.max(dat$PingNumber),]$depthStart, 
                   xend=max(PingNumber), yend=DepthStart-3), 
               linetype="dashed")
