##### Merge all files in directory ######
library(data.table)

#### list all files in folder; be to change the path
names <-list.files(path = "F:/Western Basin Forage/2016/WBfall2016/raw data/SeaBirdFiles",pattern = ".asc")
files <- paste0("F:/Western Basin Forage/2016/WBfall2016/raw data/SeaBirdFiles/", names)

#### Add file name and merge all together using a loop
DF <- NULL
for (f in files){
  dat <- read.csv(f, header=F, sep=",", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(DF,dat)
}
#### This code combines all files, but also includes all header rows.
#### To remove duplicate header rows, first rename variables/column names either using...
#### the first row
#Header<-DF[1, ]
#colnames(DF)<-c(Header) 
#colnames(DF)[12]<-"Site"  #This one needs to be changed or will otherwise me the same as first file name#

#### ...or assign your own names
colnames(DF)<-c("Conductivity","Depth","Fluorescence","DO_ppm","DO_percent","Temperature","PAR","BT","ElapsedTime","Date","Time","SpCond","serial")

#### And then remove all other header rows - 
#### this removes any row of data in which the the first variable (Conductivity) is == to the first cell of data
v1<-DF[1,1]
DF2<-DF[!DF$Conductivity==v1, ]

#### Save as csv ####
#### Change file name as needed ####
### write.csv(DF2,"WBfall2016_seabird.csv",row.names=FALSE)

#### Create a combined DateTime column
DF2$DateTime <-paste(DF2$Date,DF2$Time)
DF3<-DF2[,c("serial","DateTime","Temperature","Depth","SpCond","Conductivity","PAR","Fluorescence","BT","DO_percent","DO_ppm")]

#### Remove 'SER' from all serial Numbers
#### Change the substr 'first' and 'last' numbers to match the file path character length
DF3$serial<-substr(DF3$serial,66,68)

#### Create other null columns
DF3$Resistivity <-rep(NA,nrow(DF3))
DF3$Salinity <- rep(NA,nrow(DF3))
DF3$TDS <- rep(NA,nrow(DF3))
DF3$Density <- rep(NA,nrow(DF3))
DF3$pH <- rep(NA,nrow(DF3))
DF3$pH_mV <- rep(NA,nrow(DF3))
DF3$GPSQuality <- rep(NA,nrow(DF3))
DF3$Longitude <-rep(NA,nrow(DF3))
DF3$Latitude <-rep(NA,nrow(DF3))
#### Rename Flourescence >> Chlorophyll  ***make sure column number is correct***
colnames(DF3)[8] <- "Chlorophyll"
colnames(DF3)[9] <- "Turbidity"

#### Reorder Columns
DF4<-DF3[,c("serial","DateTime","Temperature","SpCond","Conductivity","Resistivity","Salinity","TDS","Density","Depth","pH","pH_mV","PAR","Turbidity","Chlorophyll","DO_percent","DO_ppm","GPSQuality","Longitude","Latitude")]

#### Save as csv ####
#### Change file name as needed ####
write.csv(DF4,"data_prep/WQ_raw/WB_2016_Fall_WQ.csv",row.names=FALSE)

