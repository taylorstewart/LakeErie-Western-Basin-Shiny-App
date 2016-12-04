library(dplyr)
library(magrittr)
library(readxl)

## Set year
yr <- 2016
## Set season
se <- "Autumn"

## Load data
catch <- read_excel("data_prep/WB_CatchHA.xlsx",sheet="CatchHA")
effort <- read_excel("data_prep/WB_Effort.xlsx",sheet="Effort") %>% 
  filter(year==yr & season==se) %>% 
  select(serial,time,day,month,fishing.depth.m,wingspread.m,tow.dist.m,area.m2,hectare.ha,lat,long)

## Creata character string of life stages for true/false test
life.stage <- c("YOY","Age_1","Age_2+","YAO","ALL")

## Create a factor string of species names for loop
## Needs to be before the catch fiter in order to maintain consistency between years and seasons
spec <- unique(catch$species)

## Filter catch data by year and season
catch %<>% filter(year==yr & season==se)

## Join effort data with catch
catch %<>% left_join(effort) %>% 
  select(serial,time,day,month,year,season,fishing.depth.m,wingspread.m,tow.dist.m,area.m2,
         hectare.ha,species,life.stage,count,weight.kg,n.per.ha,kg.per.ha,lat,long)

## Create list of serials for loop
## Needs to be after the catch filter becasue serial numbers change between seasons
serial <- unique(catch$serial)

## Apply first loop function
output <- data.frame(do.call(rbind,lapply(serial,function(i) {
  ## Filter catch by each serial
  catch2 <- catch %>% filter(serial==i)
  ## Apply second loop function
  tmp <- lapply(spec,function(j) {
    ## Filter by each species and drop all other factor levels
    catch3 <- catch2 %>% filter(species==j) %>% 
      droplevels()
    ## True/false output if life stages does not exist (zero value life stages)
    ls <- life.stage[!life.stage %in% catch3$life.stage]
    ## Determine the number of life stages to be added
    n <- length(ls)
    ## Create data frame with all zero value life stages, repeat by "n"
    tmp <- data.frame(serial=rep(i,n),
                      time=rep(as.character(filter(effort,serial==i)[2]),n),
                      day=rep(as.numeric(filter(effort,serial==i)[3]),n),
                      month=rep(as.numeric(filter(effort,serial==i)[4]),n),
                      year=rep(yr,n),
                      season=rep(se,n),
                      fishing.depth.m=rep(as.numeric(filter(effort,serial==i)[5]),n),
                      wingspread.m=rep(as.numeric(filter(effort,serial==i)[6]),n),
                      tow.dist.m=rep(as.numeric(filter(effort,serial==i)[7]),n),
                      area.m2=rep(as.numeric(filter(effort,serial==i)[8]),n),
                      hectare.ha=rep(as.numeric(filter(effort,serial==i)[9]),n),
                      species=rep(j,n),
                      life.stage=ls,
                      count=rep(0,n),
                      weight.kg=rep(0,n),
                      n.per.ha=rep(0,n),
                      kg.per.ha=rep(0,n),
                      lat=rep(as.numeric(filter(effort,serial==i)[10]),n),
                      long=rep(as.numeric(filter(effort,serial==i)[11]),n))
  })
  
  ## Bind list into data frame
  tmp2 <- data.frame(do.call(rbind,tmp))
  
  ## Bind all serials
  ls_sum <- if((exists("ls_sum"))==F) {
    tmp2 } else {
      rbind(ls_sum,tmp2)
    }
  ls_sum
}))) %>% ## End double loop
  arrange(serial,species)

## Bind all zero values with non-zero values
all_ls <- bind_rows(catch,output) %>% 
  arrange(year,season,serial,species)
## Ignore warning
## Should be 6355 observations, unless additional species have been added

## Read in previous data to bind to new
data <- read.csv("data/WB_Catch.csv",header=TRUE) %>% 
  mutate(time = as.character(time))
## Bind new data with all years
final_ls <- bind_rows(data,all_ls)

## Save file
write.csv(final_ls,file="data/WB_Catch.csv",row.names = FALSE)
