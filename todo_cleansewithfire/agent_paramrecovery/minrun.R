library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv(file="simexp.csv")%>%select(ppntid,calc_sd,probA,probB,probC,payoffA,payoffB,payoffC,choice) #simulated experiment data with some simulated participant responses
hm_ppnts <- max(simexp.df$ppntid)+1 #ugh

simexp.df$row.name <- 1:nrow(simexp.df);#maybe use this in some diag checking then quietly delete it?

starttime=Sys.time()
recovery.fit <- webppl(program_file="memrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))

save(recovery.fit,file="recoveryfit.RData")
endtime=Sys.time()
runtime = endtime-starttime;

mysamples <- data.frame();
fit.samples <- recovery.fit[[1]]; ##recovery.fit[[2]] is 'scores'.
for(asample in 1:length(fit.samples)){
    thissample <- fit.samples[[asample]]
    for(ppnt in 1:hm_ppnts){
        mysamples <- rbind(mysamples,data.frame(ppntid=ppnt,
                                      calcsd=thissample[1,ppnt],
                                      tolerance_prob=thissample[2,ppnt],
                                      tolerance_payoff=thissample[3,ppnt],
                                      orderr=thissample[4,ppnt]
                                      ))
    }
}

save.image(file="minruncomplete_image.RData")
