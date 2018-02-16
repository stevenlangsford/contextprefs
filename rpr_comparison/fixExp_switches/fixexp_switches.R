library(tidyverse)
library(rwebppl)
rm(list=ls())

hm_ppnts = 5
ppnt_calcsd <- rep(5,hm_ppnts)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

payoffprior_mean <- rep(100,hm_ppnts)#note stimcreation modified to set these in assignStim, so this stimcreation file is different from others here.
payoffprior_sd <- c(1,5,10,15,20)

source("stim_creation.R")

stim.df <- read.csv("fixandvarystim.csv")[1:35,]%>%bulkAssignStim

for(rep in 1:50){
    print(paste("rep",rep))
    newchoices <- addChoices(stim.df)
    stim.df[,paste0("choice",rep)] <- newchoices$choice
    }

write.csv(stim.df,"fixandvarystim_differentpayoffpriors.csv")
