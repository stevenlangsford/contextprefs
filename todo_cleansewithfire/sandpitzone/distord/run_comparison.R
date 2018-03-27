library(tidyverse)
library(rwebppl)
rm(list=ls())

hm_ppnts = 2
ppnt_calcsd <- c(5,8)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

source("stim_creation.R")
simexp.df <- data.frame()
for(i in 1:50){
    simexp.df <- rbind(simexp.df,getRandomTrial())
}
simexp.df <- bulkAssignStim(simexp.df)

timing.df <- data.frame();

for(i in 1:5){
    print("starting original")
    starttime <- Sys.time()
    newchoice <- addChoices(simexp.df,modelname="howes16.ppl")
    endtime <- Sys.time()
    timing.df <- rbind(timing.df,data.frame(rep=i,modeltype="original",runtime=(endtime-starttime)))
    print(timing.df[nrow(timing.df),]);

    print("starting distord")
    starttime <- Sys.time()
    newchoiceII <- addChoices(simexp.df,modelname="howes16distord.ppl")
    endtime <- Sys.time()
    timing.df <- rbind(timing.df,data.frame(rep=i,modeltype="distord",runtime=(endtime-starttime)))
    print(timing.df[nrow(timing.df),]);
    
    simexp.df[,paste0("choice",i)] <- newchoice$choice
    simexp.df[,paste0("choiceII",i)] <- newchoiceII$choice
    print("agreement")
    print(sum(newchoice$choice==newchoiceII$choice));
}

save.image(file="runcomparisonimage.RData");

#write.csv(simexp.df,"simexp.csv",row.names=FALSE)

##conclusion: distord is actually slower, but throws no warnings. Dunno if worth switching. :-( Can you speed it up with careful placement of distribution obj creation?
##other conclusion: starttime= Sys.time(), endtime=Sys.time(), runtime=end-start can give you waaacky results, use system.time instead.
