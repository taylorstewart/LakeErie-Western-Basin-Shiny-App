library(shiny)
library(dplyr)
library(magrittr)
library(ggvis)
library(tidyr)
library(DT)

wb_exp <- read.csv("data/WB_expLengths.csv",header=T)
catch2 <- read.csv("data/WB_CatchperHA.csv",header=T)
lw <- read.csv("data/WB_lw.csv",header=T)
effort <- read.csv("data/WB_effort.csv",header=T)
wb_shore <- read.csv("data/lake_erie_western_basin_shoreline.csv",header=T)
ftg <- read.csv("data/forage_task_group_classifications.csv",header=T)
wb_wq <- read.csv("data/WB_WaterQuality.csv",header=T)

wb_exp %<>% mutate(tl_exp = as.numeric(tl_exp))
lw %<>% select(serial,fish_id,species,tl,wt,year,season) %>%
  filter(!is.na(wt) & !is.na(tl)) %>%
  mutate(tl = as.numeric(tl),
         wt = as.numeric(wt),
         logl = log(tl),
         logw = log(wt))

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Total Length (mm)" = "tl",
  "Weight (g)" = "wt",
  "Natural Log Length" = "logl",
  "Natural Log Weight" = "logw")

# Variables that can be selected for life stages
life_vars <- c(
  "Young of the Year (YOY)" = "YOY",
  "Age-1" = "Age_1",
  "Age-2+" = "Age_2+",
  "Yearling and Older (YAO)" = "YAO")

# Variables that can be selected for years
year_vars <- lw %>% distinct(year) %>%
  select(year) %>%
  arrange(desc(year))
year_vars <- as.character(year_vars$year)

# Variables that can be selected for species
species_vars <- lw %>%
  distinct(species) %>%
  select(species) %>%
  arrange(species)
species_vars <- as.character(species_vars$species)

## Sum number per HA for all life stages by species
catch2 %<>% group_by(species,life_stage,serial,year,season) %>%
  summarise(NperHA = round(sum(NperHA),1),
            KgperHA = round(sum(KgperHA),2))

## Merge effort and catch data and rename variable names
catchHA <- right_join(effort,catch2,by="serial") %>%
  filter(!is.na(long_st) & long_st != "#N/A") %>%
  select(species=species,serial=serial,life_stage=life_stage,NperHA=NperHA,KgperHA=KgperHA,
         long=long_st,lat=lat_st,year=year,season=season)

## Filter density values for forage task group classification
## Turn off warning for join (different factor levels, coerces to a character vector)
options(warn=-1)
ftg_data <- right_join(catchHA,ftg,by="species")
ftg_data <- bind_rows(filter(ftg_data,life_stage == "YOY", season == "Autumn"),
                   filter(ftg_data,class == "Soft-rayed",season == "Autumn"),
                   filter(ftg_data,species == "Alewife",season == "Autumn"))
options(warn=1)

wb_wq %<>% select(serial=Serial,Depth=Depth_Mean,Temperature=Temp_Mean,SpConductivity=SpCond_Mean,pH=pH_Mean,
                  Turbitity=Turbitity_Mean,Chlorophyll=Chlor_Mean,DOpercent=DOpercent_Mean,DOppm=DOppm_Mean,year,season) %>%
  gather(parameter,value,3:9) %>% 
  right_join(effort,wb_wq,by="serial")

# Variables that can be selected for water quality
par_vars <- c(
  "Temperature (C)" = "Temperature",
  "Dissolved Oxygen (%)" = "DOpercent",
  "Dissolved Oxygen (mg/L)" = "DOppm",
  "Chlorophyll (ug/L)" = "Chlorophyll",
  "pH" = "pH",
  "Specific Conductitiy (mS/cm)" = "SpConductivity")
