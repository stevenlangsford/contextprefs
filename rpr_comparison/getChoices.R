library(tidyverse)
library(rwebppl)
rm(list=ls())
fixandvary.df <- read.csv("fixandvarystim.csv")
fixandvary.df$payoffprior_mean <- 100
fixandvary.df$payoffprior_sd <- 5

hm_ppnts = 1
ppnt_calcsd <- 8
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

source("stim_creation.R")

fixandvary.df <- bulkAssignStim(fixandvary.df)

for(i in 1:50){
    newchoice <- addChoices(fixandvary.df)
    fixandvary.df[,paste0("choice",i)] <- newchoice$choice
}

write.csv(fixandvary.df,"fixandvary_withchoices.csv",row.names=FALSE)
