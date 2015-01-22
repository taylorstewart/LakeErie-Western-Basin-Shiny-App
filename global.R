library(RCurl)
library(dplyr)
library(ggplot2)
library(rgdal)
library(magrittr)

databaseSource <- "online"

if (databaseSource=="online") {
## To run web-based
database <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/WB_expLengths.csv")
database2 <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/WB_CatchperHA.csv")
database3 <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/WB_lw.csv")
database4 <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/WB_catch.csv")
database5 <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/WB_effort.csv")
database6 <- getURL("https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/data/lake_erie_western_basin_shoreline.csv")
wb_exp <- read.csv(text=database,header=T)
catch2 <- read.csv(text=database2,header=T)
lw <- read.csv(text=database3,header=T)
catch <- read.csv(text=database4,header=T)
effort <- read.csv(text=database5,header=T)
wb_shore <- read.csv(text=database6,header=T)
}
if (databaseSource == "local") {
## To run locally
wb_exp <- read.csv("WB_expLengths.csv",header=T)
catch2 <- read.csv("WB_CatchperHA.csv",header=T)
lw <- read.csv("WB_lw.csv",header=T)
catch <- read.csv("WB_catch.csv",header=T)
effort <- read.csv("WB_effort.csv",header=T)
wb_shore <- read.csv("lake_erie_western_basin_shoreline.csv",header=T)
}

wb_exp$tl_exp <- as.numeric(wb_exp$tl_exp)
lw$tl <- as.numeric(lw$tl)
lw$wt <- as.numeric(lw$wt)
lw$logl <- log(lw$tl)
lw$logw <- log(lw$wt)
lw %<>% select(serial,fish_id,species,size,tl,wt,logl,logw,year,season) %>%
  filter(!is.na(wt) & !is.na(tl))
catch <- select(catch,serial,species,size,agg_wt_final,count_final,year,season)
colnames(catch) <- c("Station","Species","Size Mode","Aggregate Weight (g)","Total Count","Year","Season")

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Total Length (mm)" = "tl",
  "Weight (g)" = "wt",
  "Log Length" = "logl",
  "Log Weight" = "logw")

# Variables that can be selected for life stages
life_vars <- c(
  "Young of the Year" = "YOY",
  "Age 1" = "Age_1",
  "Age 2+" = "Age_2+",
  "Yearling and older" = "YAO")

# Variables that can be selected for years
year_vars <- data.frame(unique(lw$year))
year_vars <- as.character(year_vars[order(year_vars$unique.lw.year,decreasing=TRUE),])

# Variables that can be selected for species
species_vars <- data.frame(unique(lw$species))
species_vars <- as.character(species_vars[order(species_vars$unique.lw.species),])

# Variables that can be selected as stations
serial_vars <- data.frame(unique(lw$serial))
serial_vars <- as.character(serial_vars[order(serial_vars$unique.lw.serial),])

## Sum number per HA for all life stages by species
catch2 %<>% group_by(species,life_stage,serial,year,season) %>%
  summarise(NperHA = round(sum(NperHA),1),
            KgperHA = round(sum(KgperHA),2))

## Merge effort and catch data and rename variable names
catchHA <- merge(effort,catch2,by.x="serial",by.y="serial")
catchHA <- catchHA %>% filter(!is.na(long_st) & long_st != "#N/A") %>%
  select(species,serial,life_stage,NperHA,KgperHA,long_st,lat_st,year,season)
colnames(catchHA) <- c("species","serial","life_stage","NperHA","KgperHA","long","lat","year","season")
