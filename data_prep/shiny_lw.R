## Load packages
library(dplyr)
library(magrittr)
library(readxl)

## Enter the season and year you are summarizing...
se <- "Autumn"
yr <- "2016"

### Read in data
lw <- read_excel("data_prep/WB_LW.xlsx",sheet="LW") %>% 
  filter(species != "Unidentified Species", !is.na(wt.g), year==yr,season==se) %>% 
  droplevels()
effort <- read_excel("data_prep/WB_Effort.xlsx",sheet="Effort") %>% 
  filter(year==yr & season==se) %>% 
  select(serial,day,month,lat,long)

## Join LW and Effort
lw %<>% left_join(effort) %>% 
  select(serial,day,month,year,season,fish.id,species,sp.code,size,tl.mm,wt.g,lat,long)

## Read in all previous year's expanded LW data
lw.all <- read.csv("data/WB_LengthWeight.csv",header=T)

## Bind new data with previous data set
final.lw <- bind_rows(lw.all,lw)

## Create and save the lengths into an excel file
write.csv(final.lw,"data/WB_LengthWeight.csv",row.names = FALSE)
