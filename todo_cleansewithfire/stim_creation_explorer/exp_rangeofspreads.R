library(tidyverse)
library(rwebppl)
rm(list=ls())
hm_ppnts = 2
ppnt_calcsd <- c(5,20)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) #replicate(times,function()) if you want to call a function to generate each value
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE


source("stim_creation.R")
simexp.df <- data.frame();

for(aspread in seq(from=.11,to=10,length=30)){
    reps.per.spread <- 5; #Stim at the same spread can be quite different.
    for(arep in 1:reps.per.spread){
        astim <- getSetSpreadTrial(aspread)
        for(ppnt in 1:hm_ppnts){
            simexp.df <- rbind(simexp.df,assignStim(astim,ppnt))
        }

    }
}

simexp.df <- addChoices(simexp.df)
simexp.df$trialid <- rep(1:(nrow(simexp.df)/hm_ppnts),each=hm_ppnts)

hm.repeats <- 10
for(i in 1:hm.repeats){
    newchoice <- addChoices(simexp.df)
    simexp.df[,paste0("choice",i)] <- newchoice$choice
}
