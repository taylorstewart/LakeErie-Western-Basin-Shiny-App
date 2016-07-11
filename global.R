##############################################################
##############################################################
###
##  LEBS Shiny Application
##
##  GLOBAL SCRIPT (Data Manipulation)
##
##############################################################
##############################################################
## -----------------------------------------------------------
## Load packages
## -----------------------------------------------------------
library(shiny)       ## app functions
library(dplyr)       ## data manipulation
library(magrittr)    ## for %<>%
library(ggvis)       ## reactive visualizations
library(tidyr)       ## data reformatting
library(DT)          ## HTML data table

## -----------------------------------------------------------
## Load data
## -----------------------------------------------------------
wb_exp <- read.csv("data/WB_ExpandedLengths.csv",header=T)
catch <- read.csv("data/WB_Catch.csv",header=T)
lw <- read.csv("data/WB_LengthWeight.csv",header=T)
wb_shore <- read.csv("data/LakeErieShoreline.csv",header=T)
wb_wq <- read.csv("data/WB_WaterQuality.csv",header=T)

## -----------------------------------------------------------
## Define standardized sample location coordinates
## -----------------------------------------------------------
effort <- data.frame(serial=c(541,540,539,538,537,536,535,534,533,532,531,530,529,528,527,526,525,524,523,522,521,520,519,518,517,516,515,
                              514,513,512,511,510,509,508,507,506,505,504,503,502,501,141,140,139,138,137,136,135,134,133,132,131,130,129,
                              128,127,126,125,124,123,122,121,120,119,118,117,116,115,114,113,112,111,110,109,108,107,106,105,104,103,102,101),
                     lat=c(41.46292,41.66545,41.56448,41.46333,41.76475,41.66335,41.56392,41.46333,41.86350,41.76632,
                           41.65982,41.56332,41.46347,41.96342,41.86333,41.66342,41.56343,41.96330,41.86320,41.76323,
                           41.66318,41.56342,41.96367,41.86325,41.76292,41.66330,41.86357,41.76275,41.66438,41.56347,
                           41.96315,41.86850,41.76343,41.66327,41.96347,41.86343,41.76343,41.66338,41.86340,41.76338,
                           41.76375,41.46292,41.66545,41.56448,41.46333,41.76475,41.66335,41.56392,41.46333,41.86350,
                           41.76632,41.65982,41.56332,41.46347,41.96342,41.86333,41.66342,41.56343,41.96330,41.86320,
                           41.76323,41.66318,41.56342,41.96367,41.86325,41.76292,41.66330,41.86357,41.76275,41.66438,
                           41.56347,41.96315,41.86850,41.76343,41.66327,41.96347,41.86343,41.76343,41.66338,41.86340,
                           41.76338,41.76375),
                     long=c(-82.24912,-82.34870,-82.35175,-82.34693,-82.44952,-82.45392,-82.45270,-82.44707,-82.55002,
                            -82.55045,-82.54988,-82.55035,-82.55025,-82.65002,-82.65007,-82.65467,-82.65072,-82.75310,
                            -82.74985,-82.74978,-82.74997,-82.75052,-82.85030,-82.85010,-82.84950,-82.85012,-82.95018,
                            -82.95038,-82.95070,-82.95092,-83.05067,-83.05093,-83.04990,-83.04967,-83.14988,-83.14995,
                            -83.14987,-83.14980,-83.24993,-83.24987,-83.34615,-82.24912,-82.34870,-82.35175,-82.34693,
                            -82.44952,-82.45392,-82.45270,-82.44707,-82.55002,-82.55045,-82.54988,-82.55035,-82.55025,
                            -82.65002,-82.65007,-82.65467,-82.65072,-82.75310,-82.74985,-82.74978,-82.74997,-82.75052,
                            -82.85030,-82.85010,-82.84950,-82.85012,-82.95018,-82.95038,-82.95070,-82.95092,-83.05067,
                            -83.05093,-83.04990,-83.04967,-83.14988,-83.14995,-83.14987,-83.14980,-83.24993,-83.24987,
                            -83.34615)) %>% arrange(serial)

## -----------------------------------------------------------
## Manipulate length-weight data
## -----------------------------------------------------------
wb_exp %<>% mutate(tl.mm = as.numeric(tl.mm))
lw %<>% select(serial,day,month,year,season,fish.id,species,tl.mm,wt.g) %>%
  filter(!is.na(wt.g) & !is.na(tl.mm)) %>%
  mutate(tl.mm = as.numeric(tl.mm),
         wt.g = as.numeric(wt.g),
         logl = log(tl.mm),
         logw = log(wt.g))

## -----------------------------------------------------------
## Manipulate catch data
## -----------------------------------------------------------
## Sum number per HA for all life stages by species
catch %<>% group_by(serial,day,month,year,season,species,life.stage) %>%
  summarise(n.per.ha = round(sum(n.per.ha),2),
            kg.per.ha = round(sum(kg.per.ha),2))

## Merge effort and catch data and rename variable names
catch <- right_join(effort,catch,by="serial") %>%
  filter(!is.na(long) & long != "#N/A") %>%
  select(serial,day,month,year,season,species,serial,life.stage,n.per.ha,kg.per.ha,
         long,lat)

## -----------------------------------------------------------
## Create FTG catch data
## -----------------------------------------------------------
## Filter density values for forage task group classification
## Turn off warning for join (different factor levels, coerces to a character vector)
options(warn=-1)
ftg_data <- data.frame(species=c('Walleye','White Perch','White Bass','Freshwater Drum','Yellow Perch','Trout Perch','Round Goby','Gizzard Shad','Emerald Shiner','Rainbow Smelt','Spottail Shiner','Silver Chub','Alewife','Brook Silverside'),
                  class=c('Spiny-rayed','Spiny-rayed','Spiny-rayed','Spiny-rayed','Spiny-rayed','Soft-rayed','Soft-rayed','Clupeids','Soft-rayed','Soft-rayed','Soft-rayed','Soft-rayed','Clupeids','Soft-rayed'))
ftg_data <- right_join(catch,ftg_data,by="species")
ftg_data <- bind_rows(filter(ftg_data,life.stage == "YOY", season == "Autumn"),
                   filter(ftg_data,class == "Soft-rayed",season == "Autumn"),
                   filter(ftg_data,species == "Alewife",season == "Autumn"))
options(warn=1)

## -----------------------------------------------------------
## "Tidy" water quality data
## -----------------------------------------------------------
wb_wq %<>% select(serial,day,month,year,season,BinDepthMin=bin.depth.min,BinDepthMax=bin.depth.max,
                           Depth=depth.mean,Temperature=temp.mean,SpConductivity=cond.mean,pH=ph.mean,
                           Turbitity=turb.mean,Chlorophyll=chloro.mean,DOpercent=do.percent.mean,DOppm=do.ppm.mean) %>%
  gather(parameter,value,9:15) %>% 
  right_join(effort,wb_wq,by="serial") %>% 
  mutate(Depth=round(Depth,0))
  ## You will have NA values for most parameters in 2015 Autumn due to YSI issue; ignore missing values

## -----------------------------------------------------------
## Create variable lists
## -----------------------------------------------------------
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

# Variables that can be selected for water quality
par_vars <- c(
  "Temperature (C)" = "Temperature",
  "Dissolved Oxygen (%)" = "DOpercent",
  "Dissolved Oxygen (mg/L)" = "DOppm",
  "Chlorophyll (ug/L)" = "Chlorophyll",
  "pH" = "pH",
  "Specific Conductitiy (mS/cm)" = "SpConductivity")

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Total Length (mm)" = "tl.mm",
  "Weight (g)" = "wt.g",
  "Natural Log Length" = "logl",
  "Natural Log Weight" = "logw")

# Variables that can be selected for life stages
life_vars <- c(
  "Young of the Year (YOY)" = "YOY",
  "Age-1" = "Age_1",
  "Age-2+" = "Age_2+",
  "Yearling and Older (YAO)" = "YAO")

