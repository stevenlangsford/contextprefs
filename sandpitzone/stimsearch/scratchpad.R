library(tidyverse)
rm(list=ls())
ret.ls <- list()

for(i in 1:20){
    ret.ls[[i]] <-
        read.csv(paste0("interesting",i,".csv"))%>%distinct(trialid, .keep_all=TRUE)%>%select(-optionchosen,-isInteresting,-ppntid)
}
