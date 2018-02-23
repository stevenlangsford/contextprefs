library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv(file="simexp.csv")[1:2,] #simulated experiment data with some simulated participant responses

starttime=Sys.time()
recovery.fit <- as.data.frame(webppl(program_file="recover.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))) #these.trials[1:i,]

save(recovery.fit,file="recoveryfit.RData")
endtime=Sys.time()
runtime = endtime-starttime;

fit.samples <- data.frame(ppnt_id=c(),sample=c())
for(ppnt in 1:hm_ppnts){
    fit.samples <- rbind(fit.samples,
                         data.frame(ppnt_id=ppnt,sample=as.numeric(map(recovery.fit[[1]],function(x){x[ppnt]})))
                         )
}

write.csv(fit.samples,file="fitsamples.csv")
write.csv(data.frame(start=starttime,complete=endtime,difference=runtime), file="runtime.csv",row.names=FALSE)

ggsave(ggplot(fit.samples,aes(x=sample))+geom_histogram()+facet_wrap(~ppnt_id)+theme_bw(),file="recovery.png")
