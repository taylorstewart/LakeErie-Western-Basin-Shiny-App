
### Raw files must be saved in ".xlsx" format

#############################################################################
## Load packages and set java parameters for XLConnect
options(java.parameters="-Xmx2g")
library(FSA)
library(XLConnect)
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)

## Enter the season and year you are summarizing...
season <- "spring"
year <- "2015"

## Raw data file
raw_file <- "data_prep/WBspr2015_WQ_raw.xlsx"

## Effort data file
effort_file <- "data_prep/WBspr2015_LimnoData.xlsx"

#############################################################################
#############################################################################
## Create function to read Raw data in and create a list including all sheets
importWorksheets <- function(WB) {
  workbook <- loadWorkbook(raw_file)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names,function(.sheet){
    readWorksheet(object=workbook, .sheet,startRow=3)})}

## Read Raw data in and modify the list into a data frame
sonde <- do.call(rbind,importWorksheets(WB))

## Create names for the variables in Raw DF
names(sonde) <- c("DateTime","Temp","spCond","Conductivity",
                   "Resistivity","Salinity","TDS","Density","Depth",
                   "pH","pH_mV","PAR","Turbitity","Chlorophyll","DO_1",
                   "DO_2","GPS Quality","Longitude","Latitude")

## Subset the Raw DF variables needed for analysis
sonde %<>% select(Depth,DateTime,Temp,spCond,pH,pH_mV,Turbitity,Chlorophyll,DO_1,DO_2,Longitude,Latitude) %>% 
  arrange(DateTime)

## Make DateTime variable POSIXct
sonde$DateTime <- as.POSIXct(sonde$DateTime,format="%Y-%m-%d %H:%M:%S")

## Read Effort data
wb2 <- loadWorkbook(effort_file)
effort <- readWorksheet(wb2,sheet="Sheet1") %>%
  select(serial,date,time_st,lat_st,long_st) %>%
  arrange(serial)

## Create a new df with Serial, Lat, and Longs to merge later.
latlong <- effort %>% select(serial,lat_st,long_st)

## Add DateTime variable and make it POSIXct
effort$DateTime <- as.POSIXct(paste(effort$date,effort$time_st),format="%Y-%m-%d %H%M")

## Remove any NA rows and select fewer columns
##  You can probably remove Date and Time from here ...
##  I left them in for ease in checking that the merge below worked.
effort %<>% filter(!is.na(date)) %>% 
  select(serial,date,time_st,DateTime)

## Create a new variable in Raw df that converts the Raw DateTime
##  into an interval (or cut) based on the DateTime variables in Effort.
##  This should create a variable that has the same values as values in
##  DateTime of Effort that is closest but before the DateTime value in
##  Raw.  This assumes that all DateTime values in Raw are after the
##  relevant DateTime in Effort.
sonde$eDateTime <- as.POSIXct(cut(sonde$DateTime,breaks=effort$DateTime,right=FALSE,include.lowest=TRUE))

## Merge the two dfs together where the rows from Raw with
##  an eDataTime that matches rows from Effort are put together
newdf <- merge(sonde,effort,by.x="eDateTime",by.y="DateTime")

## Factor the depth variable from -0.5 to 25.5 meters by 1 meter intervals.
##  The mean depth of each factor should approximately be equivilent 1-m bins
newdf$fdepth <- cut(newdf$Depth,breaks=seq(-0.5,25.5,1),right=FALSE)

## Create new time variable to calculate difference at maximum depth
newdf$DateTime <- as.character(newdf$DateTime)
newdf2 <- strsplit(newdf$DateTime,split=" ")
newdf$Time2 <- unlist(lapply(newdf2, "[",2))
newdf$Time2 <- gsub("[[:punct:]]","",newdf$Time2)
newdf$Time2 <- as.numeric(newdf$Time2)

## Find maximum depth at each serial, merge maximum values into newdf, 
##  calculate the difference of max depth time by each rows time.
##   (positive value = down cast, negative value = up cast)
newdf$serial <- factor(newdf$serial)
df2<-ddply(newdf,c('serial'),function(x) x[which(x$Depth==max(x$Depth)),])
df3 <- merge(df2,newdf,by.x="serial",by.y="serial")
df3$diff <- df3$Time2.x - df3$Time2.y

## Remove all negative time difference values
newdf <- filter(df3,diff >= 0)

## Remove any NA depth bins (surface readings)
newdf <- filter(newdf,!is.na(fdepth.y))

## Summarize each variable by mean, std.dev, and CV.
##  Round variables to the thousandth. Rename headers.

## Summarize Depth
sumdepth <- Summarize(Depth.y~serial+fdepth.y,data=newdf,digits=3)
sumdepth$CV <- sumdepth$sd/sumdepth$mean
sumdepth %<>% select(serial,fdepth.y,n,mean,sd,CV)
sumdepth$mean <- round(sumdepth$mean,3)
sumdepth$sd <- round(sumdepth$sd,3)
sumdepth$CV <- round(sumdepth$CV,3)
names(sumdepth) <- c("Serial","Depth_Factor","n","Depth_Mean","Depth_Std.Dev","Depth_CV")

## Summarize temp
sumtmp <- Summarize(Temp.y~serial+fdepth.y,data=newdf,digits=3)
sumtmp$CV <- sumtmp$sd/sumtmp$mean
sumtmp %<>% select(mean,sd,CV)
sumtmp <- round(sumtmp,3)
names(sumtmp) <- c("Temp_Mean","Temp_Std.Dev","Temp_CV")

## Summarize spCond
sumspCond <- Summarize(spCond.y~serial+fdepth.y,data=newdf,digits=3)
sumspCond$CV <- sumspCond$sd/sumspCond$mean
sumspCond %<>% select(mean,sd,CV)
sumspCond <- round(sumspCond,3)
names(sumspCond) <- c("SpCond_Mean","SpCond_Std.Dev","spCond_CV")

## Summarize pH
sumpH <- Summarize(pH.y~serial+fdepth.y,data=newdf,digits=3)
sumpH$CV <- sumpH$sd/sumpH$mean
sumpH %<>% select(mean,sd,CV)
sumpH <- round(sumpH,3)
names(sumpH) <- c("pH_Mean","pH_Std.Dev","pH_CV")

## Summariaze Turbitity
sumturb <- Summarize(Turbitity.y~serial+fdepth.y,data=newdf,digits=3)
sumturb$CV <- sumturb$sd/sumturb$mean
sumturb %<>% select(mean,sd,CV)
sumturb <- round(sumturb,3)
names(sumturb) <- c("Turbitity_Mean","Turbitity_Std.Dev","Turbitity_CV")

## Summarize Chlorophyll
sumchlor <- Summarize(Chlorophyll.y~serial+fdepth.y,data=newdf,digits=3)
sumchlor$CV <- sumchlor$sd/sumchlor$mean
sumchlor %<>% select(mean,sd,CV)
sumchlor <- round(sumchlor,3)
names(sumchlor) <- c("Chlor_Mean","Chlor_Std.Dev","Chlor_CV")

## Summarize DO %
sumdo_1 <- Summarize(DO_1.y~serial+fdepth.y,data=newdf,digits=3)
sumdo_1$CV <- sumdo_1$sd/sumdo_1$mean
sumdo_1 %<>% select(mean,sd,CV)
sumdo_1 <- round(sumdo_1,3)
names(sumdo_1) <- c("DOpercent_Mean","DOpercent_Std.Dev","DOpercent_CV")

## Summarize DO ppm
sumdo_2 <- Summarize(DO_2.y~serial+fdepth.y,data=newdf,digits=3)
sumdo_2$CV <- sumdo_2$sd/sumdo_2$mean
sumdo_2 %<>% select(mean,sd,CV)
sumdo_2 <- round(sumdo_2,3)
names(sumdo_2) <- c("DOppm_Mean","DOppm_Std.Dev","DOppm_CV")

## Bind all data frames into one
sondeOutput <- cbind(sumdepth,sumtmp,sumspCond,sumpH,sumturb,sumchlor,sumdo_1,sumdo_2)

## Merge the date, time and region variables into the output
sondeOutput2 <- merge(sondeOutput,effort,by.x="Serial",by.y="serial")

## Merge Lat/Longs from effort into summary data frame for each serial.
sondeOutput2 <- merge(sondeOutput2,latlong,by.x="Serial",by.y="serial")

## Reorder summary data frame
sondeOutput2 %<>% select(Serial,date,time_st,Depth_Factor,n,Depth_Mean,Depth_Std.Dev,Depth_CV,
                       Temp_Mean,Temp_Std.Dev,Temp_CV,SpCond_Mean,SpCond_Std.Dev,spCond_CV,pH_Mean,
                       pH_Std.Dev,pH_CV,Turbitity_Mean,Turbitity_Std.Dev,
                       Turbitity_CV,Chlor_Mean,Chlor_Std.Dev,Chlor_CV,DOpercent_Mean,DOpercent_Std.Dev,DOpercent_CV,
                       DOppm_Mean,DOppm_Std.Dev,DOppm_CV,lat_st,long_st)

## Order df by serial and depth in descending order
sondeOutput2 <- sondeOutput2[order(sondeOutput2$Serial,sondeOutput2$Depth_Mean),]

## Convert date back to a character string for ease of reading it in excel.
sondeOutput2$Date <- as.character(sondeOutput2$date)

## Clean up workspace
rm(importWorksheets,df2,df3,effort,latlong,newdf,sonde,wb2,
   newdf2,sumturb,sumtmp,sumspCond,sumpH,sumdo_1,sumdo_2,sumdepth,
   sumchlor,sondeOutput)

## Save the summary output into an excel file
WB_SONDE <- paste(c("data_prep/WB_",season,"_",year,"_WQ_SUMMARY.xlsx"),collapse="")
wb <- loadWorkbook(WB_SONDE,create=T)
createSheet(wb,name="WB_WQ")
writeWorksheet(wb,data=sondeOutput2,sheet="WB_WQ",startRow=1,startCol=1,header=TRUE)
saveWorkbook(wb)

