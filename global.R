## Load packages
library(shiny)
library(dplyr)
library(magrittr)
library(ggvis)
library(tidyr)
library(DT)

## Load data
wb_exp <- read.csv("data/WB_ExpandedLengths.csv",header=T)
catch2 <- read.csv("data/WB_Catch.csv",header=T)
lw <- read.csv("data/WB_LengthWeight.csv",header=T)
wb_shore <- read.csv("data/LakeErieShoreline.csv",header=T)
wb_wq <- read.csv("data/WB_WaterQuality.csv",header=T)

## Define standard effort coordinates
effort <- data.frame(serial=c(541,540,539,538,537,536,535,534,533,532,531,530,529,528,527,526,525,524,523,522,521,520,519,518,517,516,515,
                              514,513,512,511,510,509,508,507,506,505,504,503,502,501,141,140,139,138,137,136,135,134,133,132,131,130,129,
                              128,127,126,125,124,123,122,121,120,119,118,117,116,115,114,113,112,111,110,109,108,107,106,105,104,103,102,101),
                     lat_st=c(41.46292,41.66545,41.56448,41.46333,41.76475,41.66335,41.56392,41.46333,41.86350,41.76632,
                              41.65982,41.56332,41.46347,41.96342,41.86333,41.66342,41.56343,41.96330,41.86320,41.76323,
                              41.66318,41.56342,41.96367,41.86325,41.76292,41.66330,41.86357,41.76275,41.66438,41.56347,
                              41.96315,41.86850,41.76343,41.66327,41.96347,41.86343,41.76343,41.66338,41.86340,41.76338,
                              41.76375,41.46292,41.66545,41.56448,41.46333,41.76475,41.66335,41.56392,41.46333,41.86350,
                              41.76632,41.65982,41.56332,41.46347,41.96342,41.86333,41.66342,41.56343,41.96330,41.86320,
                              41.76323,41.66318,41.56342,41.96367,41.86325,41.76292,41.66330,41.86357,41.76275,41.66438,
                              41.56347,41.96315,41.86850,41.76343,41.66327,41.96347,41.86343,41.76343,41.66338,41.86340,
                              41.76338,41.76375),
                     long_st=c(-82.24912,-82.34870,-82.35175,-82.34693,-82.44952,-82.45392,-82.45270,-82.44707,-82.55002,
                               -82.55045,-82.54988,-82.55035,-82.55025,-82.65002,-82.65007,-82.65467,-82.65072,-82.75310,
                               -82.74985,-82.74978,-82.74997,-82.75052,-82.85030,-82.85010,-82.84950,-82.85012,-82.95018,
                               -82.95038,-82.95070,-82.95092,-83.05067,-83.05093,-83.04990,-83.04967,-83.14988,-83.14995,
                               -83.14987,-83.14980,-83.24993,-83.24987,-83.34615,-82.24912,-82.34870,-82.35175,-82.34693,
                               -82.44952,-82.45392,-82.45270,-82.44707,-82.55002,-82.55045,-82.54988,-82.55035,-82.55025,
                               -82.65002,-82.65007,-82.65467,-82.65072,-82.75310,-82.74985,-82.74978,-82.74997,-82.75052,
                               -82.85030,-82.85010,-82.84950,-82.85012,-82.95018,-82.95038,-82.95070,-82.95092,-83.05067,
                               -83.05093,-83.04990,-83.04967,-83.14988,-83.14995,-83.14987,-83.14980,-83.24993,-83.24987,
                               -83.34615))

## Manipulate length-weight data
wb_exp %<>% mutate(tl_exp = as.numeric(tl_exp)) %>% 
  select(-X)
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
ftg <- data.frame(species=c('Walleye','White Perch','White Bass','Freshwater Drum','Yellow Perch','Trout Perch','Round Goby','Gizzard Shad','Emerald Shiner','Rainbow Smelt','Spottail Shiner','Silver Chub','Alewife','Brook Silverside'),
                  class=c('Spiny-rayed','Spiny-rayed','Spiny-rayed','Spiny-rayed','Spiny-rayed','Soft-rayed','Soft-rayed','Clupeids','Soft-rayed','Soft-rayed','Soft-rayed','Soft-rayed','Clupeids','Soft-rayed'))
ftg_data <- right_join(catchHA,ftg,by="species")
ftg_data <- bind_rows(filter(ftg_data,life_stage == "YOY", season == "Autumn"),
                   filter(ftg_data,class == "Soft-rayed",season == "Autumn"),
                   filter(ftg_data,species == "Alewife",season == "Autumn"))
options(warn=1)

## Tidy water quality data
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
