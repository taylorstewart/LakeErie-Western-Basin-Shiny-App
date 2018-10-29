####################################################################################
### This script assumes that for each cruise you used the seabird_merge script 
### located in each cruise folder ...year/season... to process the seabird data
### and generate a summary table with the correct headings, etc. 

library(readxl)
library(dplyr)

# Read in the spring and fall data from this year
# make sure the file paths are correct
# spring<-read.csv("F:/Western Basin Forage/2017/WBspr2017/Raw Data/Seabird/WB_2017_Spring_WQ.csv",header=TRUE)
autumn<-read.csv("F:/Western Basin Forage/2018/WBfall2018/raw data/SeabirdFiles/WB_2018_Autumn_WQ.csv",header=TRUE)

# Read in the historical data
wq_all<-read.csv("data/WB_WaterQuality.csv",header=T)

# Read in the effort data file
effort_file<-read_excel("data_prep/WB_Effort.xlsx",sheet="Effort")%>%
  filter(year==2018)%>%
  select(serial,lat,long)%>%
  unique()

# Combine spring autumn data from current year
current<-bind_rows(spring,autumn)%>%
# join lat long by serial
  left_join(effort_file)

# or in 2018 b/c only Autumn was sampled
current<-autumn%>%left_join(effort_file)

## Bind new data with previous data set
final_wq <- bind_rows(wq_all,current) ## ignore warning

## Create and save the lengths into an excel file
write.csv(final_wq,"data/WB_WaterQuality.csv",row.names = FALSE)

############################################################################################################
