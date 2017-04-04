#####################################################################
##  This code was run on 4-4-2017 to correct the ShinyApp error
##  that Besty found: i.e., All Life Stages output was not correct.
##  It should not be necessary to run this code again.
#####################################################################
catch <- read.csv("data/WB_Catch.csv",header=T)
library(tidyverse)
number<-catch%>%select(year,serial,species,life.stage,count)%>%
  group_by(year,serial,species)%>%
  spread(life.stage,count)%>%
  mutate(temp=sum(Age_1,`Age_2+`,ALL,YOY,YAO))%>%
  mutate(ALL=temp)%>%
  select(-temp)%>%
  gather(life.stage,count,Age_1:YOY)
mass<-catch%>%select(year,serial,species,life.stage,weight.kg)%>%
  group_by(year,serial,species)%>%
  spread(life.stage,weight.kg)%>%
  mutate(temp=sum(Age_1,`Age_2+`,ALL,YOY,YAO))%>%
  mutate(ALL=temp)%>%
  select(-temp)%>%
  gather(life.stage,weight.kg,Age_1:YOY)
data<-left_join(number,mass)
newdata<-catch%>%
  select(-count,-weight.kg)%>%
  left_join(data)%>%
  mutate(n.per.ha=count/hectare.ha,kg.per.ha=weight.kg/hectare.ha)
write.csv(newdata,file="data/WB_Catch.csv",row.names = FALSE)
