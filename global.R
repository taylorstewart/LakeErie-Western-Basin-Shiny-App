library(shiny)
library(RCurl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggvis)

wb_exp <- read.csv("data/WB_expLengths.csv",header=T)
catch2 <- read.csv("data/WB_CatchperHA.csv",header=T)
lw <- read.csv("data/WB_lw.csv",header=T)
catch <- read.csv("data/WB_catch.csv",header=T)
effort <- read.csv("data/WB_effort.csv",header=T)
wb_shore <- read.csv("data/lake_erie_western_basin_shoreline.csv",header=T)
ftg <- read.csv("data/forage_task_group_classifications.csv",header=T)

wb_exp$tl_exp <- as.numeric(wb_exp$tl_exp)
lw$tl <- as.numeric(lw$tl)
lw$wt <- as.numeric(lw$wt)
lw$logl <- log(lw$tl)
lw$logw <- log(lw$wt)
lw %<>% select(serial,fish_id,species,size,tl,wt,logl,logw,year,season) %>%
  filter(!is.na(wt) & !is.na(tl))
catch %<>% select(serial,species,size,agg_wt_final,count_final,year,season)

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
serial_vars <- arrange(select(distinct(lw,serial),serial),serial)

## Sum number per HA for all life stages by species
catch2 %<>% group_by(species,life_stage,serial,year,season) %>%
  summarise(count = round(sum(count),1),
            NperHA = round(sum(NperHA),1),
            KgperHA = round(sum(KgperHA),2))

## Merge effort and catch data and rename variable names
catchHA <- right_join(effort,catch2,by="serial") %>%
  filter(!is.na(long_st) & long_st != "#N/A") %>%
  rename(species=species,serial=serial,life_stage=life_stage,count=count,NperHA=NperHA,KgperHA=KgperHA,long=long_st,lat=lat_st,year=year,season=season)

## Filter density values for forage task group classification
ftg_data <- right_join(catchHA,ftg,by="species")
ftg_data <- bind_rows(filter(ftg_data,life_stage == "YOY", season == "Autumn"),
                   filter(ftg_data,class == "Soft-rayed",season == "Autumn"),
                   filter(ftg_data,species == "Alewife",season == "Autumn"))
