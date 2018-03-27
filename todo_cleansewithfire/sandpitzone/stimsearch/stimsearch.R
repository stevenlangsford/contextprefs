library(tidyverse)
library(rwebppl)
rm(list=ls())

for(repnumber in 1:20){
    print(repnumber);
starttime <- Sys.time()
fit <- webppl(program_file="stimsearch.ppl",data=data.frame(hm_particles=100),data_var="rparams"); #note there are interesting params hard-coded not passed, notably the calcsd for the two agents you're trying to distinguish between.
endtime <- Sys.time()

runtime <- endtime-starttime

stimsettings <- fit[[1]] #'value', list of samples, each entry is an array of 3 probs followed by 3 payoffs.
stimscores <- fit[[2]] #negative log-lik provided by webppl, so closer to zero = better.
stim.df <- data.frame(probA=double(),probB=double(),probC=double(),payoffA=double(),payoffB=double(),payoffC=double(),ppl_score=double());

for(i in 1:length(stimsettings))stim.df[i,] <- c(stimsettings[[i]],stimscores[[i]])

stim.df <-   stim.df%>%distinct()%>%arrange(desc(ppl_score))
approx.df <- signif(stim.df,2)%>%distinct()

#Hacky extra distinctness test, also distinct by probs and payoffs separately.
probset <- paste(approx.df[,"probA"],approx.df[,"probB"],approx.df[,"probC"])
payoffset <- paste(approx.df[,"payoffA"],approx.df[,"payoffB"],approx.df[,"payoffC"])

approx.df <- approx.df[!(duplicated(probset) | duplicated(payoffset)),]
approx.df$ppl_score <- NULL
candidatestim.df <- approx.df[1:(min(50,nrow(approx.df))),]
#print(runtime)
#View(approx.df)#popup

##STIM SEARCH ENDS HERE

##Test how successful your stim search was:
hm_ppnts = 2
ppnt_calcsd <- c(3,10)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

source("stim_creation.R")

test.df <- bulkAssignStim(candidatestim.df)
test.df$payoffprior_mean <- 19
test.df$payoffprior_sd <- 8

for(i in 1:20){
    print(paste("getting choice",i));
    newchoice <- addChoices(test.df)
    test.df[,paste0("choice",i)] <- newchoice$choice
}

write.csv(test.df,"testdf.csv",row.names=FALSE)
source("agreement_vis.R")
}
