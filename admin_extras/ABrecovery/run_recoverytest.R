library(tidyverse)
library(rwebppl)

simexp.df <- read.csv(file="ABsimexp_withchoices.csv")%>%select(contains("attribute"),ppntid,Aweight,Bweight,contains("value"),contains("choice"))
hm_ppnts <- max(simexp.df$ppntid)+1

starttime <- Sys.time()
fit.recovery <- webppl(program_file="ABrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))
save(fit.recovery,file="fitrecovery.RData")
endtime <- Sys.time()
runtime <- endtime-starttime

fit.samples <- fit.recovery[[1]]

mysamples.df <- data.frame()
for(asample in 1:length(fit.samples)){
    thissample <- fit.samples[[asample]]
    for(ppnt in 1:hm_ppnts){
        mysamples.df <- rbind(mysamples.df,data.frame(ppntid=ppnt-1,
                                      Aweight=thissample[1,ppnt],
                                      Bweight=thissample[2,ppnt]
                                      ))
    }
}

save.image(file="runrecoverytest_complete.RData")

