library(dplyr)
library(magrittr)

## Set year
yr <- 2015
## Set season
se <- "Spring"

## Load data
catch <- read.csv("data_prep/WB_catchperHA_raw.csv",header=T)

## Creata character string of life stages for true/false test
life_stage <- c("YOY","Age_1","Age_2+","YAO","ALL")

## Create a factor string of species names for loop
## Needs to be before the catch fiter in order to maintain consistency between years and seasons
spec <- unique(catch$species)

## Filter catch data by year and season
catch %<>% filter(year==yr & season==se)

## Create list of serials for loop
## Needs to be after the catch filter becasue serial numbers change between seasons
serial <- unique(catch$serial)

## Apply first loop function
output1 <- lapply(serial,function(i) {
  ## Filter catch by each serial
  catch2 <- catch %>% filter(serial==i)
  ## Apply second loop function
  tmp <- lapply(spec,function(j) {
    ## Filter by each species and drop all other factor levels
    catch3 <- catch2 %>% filter(species==spec[j]) %>% 
      droplevels()
    ## True/false output if life stages does not exist (zero value life stages)
    ls <- life_stage[!life_stage %in% catch3$life_stage]
    ## Determine the number of life stages to be added
    n <- length(ls)
    ## Create data frame with all zero value life stages, repeat by "n"
    tmp <- data.frame(serial=rep(i,n),species=rep(spec[j],n),
                      life_stage=ls,NperHA=rep(0,n),KgperHA=rep(0,n),year=rep(yr,n),season=rep(se,n))
  })
  
  ## Bind list into data frame
  tmp2 <- data.frame(do.call(rbind,tmp))
  
  ## Bind all serials
  ls_sum <- if((exists("ls_sum"))==F) {
    tmp2 } else {
      rbind(ls_sum,tmp2)
    }
  ls_sum
})
## End double loop

## Bind list into data frame
output2 <- data.frame(do.call(rbind,output1)) %>% 
  arrange(serial,species)

## Bind all zero values with non-zero values
all_ls <- bind_rows(catch,output2) %>% 
  arrange(year,season,serial,species)
## Ignore warning

## Read in previous data to bind to
data <- read.csv("data/WB_catchperHA_All_LS.csv",header=T) %>% 
  select(-X)

## Bind new data with all years
final_ls <- bind_rows(data,all_ls)

## Save file
write.csv(final_ls,file="data/WB_CatchperHA_All_LS.csv")
