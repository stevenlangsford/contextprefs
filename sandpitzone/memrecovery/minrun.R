library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv(file="simexp.csv")%>%select(ppntid,calc_sd,probA,probB,probC,payoffA,payoffB,payoffC,choice) #simulated experiment data with some simulated participant responses
hm_ppnts <- max(simexp.df$ppntid)+1 #ugh

starttime=Sys.time()
recovery.fit <- webppl(program_file="memrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))

save(recovery.fit,file="recoveryfit.RData")
endtime=Sys.time()
runtime = endtime-starttime;

mysamples <- data.frame();

for(i in 1:hm_ppnts){
    mycalcsd <- as.numeric(map(recovery.fit[[1]],function(x){return(x[i])}))
    mysamples <- rbind(mysamples,
                       data.frame(ppntid=i,calcsd=mycalcsd)
                       )
}
