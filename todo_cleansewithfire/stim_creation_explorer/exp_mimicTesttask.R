library(tidyverse)
rm(list=ls())
hm_ppnts = 1
ppnt_calcsd <- 5
ppnt_tolerance_prob <- rep(.011,hm_ppnts) #replicate(times,function()) if you want to call a function to generate each value
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE
source("stim_creation.R")
payoffprior_mean=19
payoffprior_sd=8

starttime=Sys.time()

testtrials.df <- data.frame()%>%
    rbind(
        getCustomTrial(probA=0.83,probB=0.498,probC=0.55, payoffA=12, payoffB=20, payoffC=22,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.498, probC=0.98, payoffA=12, payoffB=20, payoffC=10.16326531,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.498, probC=0.35, payoffA=12, payoffB=20, payoffC=28.57142857,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.498, probC=0.83, payoffA=12, payoffB=20, payoffC=12,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.498, probC=0.5, payoffA=12, payoffB=20, payoffC=20,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.83, payoffA=12, payoffB=15, payoffC=10,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.62, payoffA=12, payoffB=15, payoffC=15,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.78, payoffA=12, payoffB=15, payoffC=12,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.67, payoffA=12, payoffB=15, payoffC=13,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.78, payoffA=12, payoffB=15, payoffC=10,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.62, payoffA=12, payoffB=15, payoffC=13,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.62, payoffA=12, payoffB=15, payoffC=10,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.62, payoffA=12, payoffB=15, payoffC=10,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.88, payoffA=12, payoffB=15, payoffC=14,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.72, payoffA=12, payoffB=15, payoffC=17,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.98, payoffA=12, payoffB=15, payoffC=10.16326531,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.52, payoffA=12, payoffB=15, payoffC=19.32692308,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.83, payoffA=12, payoffB=15, payoffC=12,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd),
        getCustomTrial(probA=0.83, probB=0.664, probC=0.67, payoffA=12, payoffB=15, payoffC=15,payoffprior_mean=payoffprior_mean,payoffprior_sd=payoffprior_sd)
    )

testtrials.df <- bulkAssignStim(testtrials.df)

reps <- 500
for(i in 1:reps){
    newchoices <- addChoices(testtrials.df)
    testtrials.df[,paste0("choice",i)] <- newchoices$choice
}

endtime=Sys.time()
runtime = endtime-starttime

save.image(file="ran_mimicTesttask.RData")
write.csv(testtrials.df,file="mimic_testtrials.csv")
