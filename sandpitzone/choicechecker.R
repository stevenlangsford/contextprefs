library(tidyverse)
library(rwebppl)
rm(list=ls())

stim.df <- read.csv("all_superstim_firstpass.csv")

hm_ppnts = 2
ppnt_calcsd <- c(1,20)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

source("stim_creation.R")

stim.df <- bulkAssignStim(stim.df)
stim.df$payoffprior_mean <- with(stim.df,mean(c(payoffA,payoffB,payoffC)))
stim.df$payoffprior_sd <- with(stim.df,sd(c(payoffA,payoffB,payoffC)))

for(rep in 1:20){
    print(paste("rep",rep))
    newchoices <- addChoices(stim.df)
    stim.df[,paste("choice",rep)] <- newchoices$choice
}

print(endtime-starttime)

write.csv(stim.df,file="choicecheck.csv")
save.image("choicecheck.RData")
