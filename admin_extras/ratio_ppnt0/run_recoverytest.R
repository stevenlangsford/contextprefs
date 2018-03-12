library(tidyverse)
library(rwebppl)

simexp.df <- read.csv(file="simexp.csv")%>%select(contains("attribute"),ppntid,Aweight,Bweight,contains("value"),contains("choice"))
hm_ppnts <- length(unique(simexp.df$ppntid))
simexp.df$trialid <- rep(1:(nrow(simexp.df)/hm_ppnts), each=hm_ppnts)

runtime <- system.time(
{fit.recovery <<- webppl(program_file="ABrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))}
)
save(fit.recovery,file="fitrecovery.RData")

##CHECK thy return format. I think it's [ppntid, Bweight]. 
 fit.samples <- fit.recovery[[1]]
 mysamples.df <- data.frame()
 for(asample in 1:length(fit.samples)){
     thissample <- fit.samples[[asample]]
     mysamples.df <- rbind(mysamples.df,data.frame(ppnt=thissample[1,1],sample=thissample[1,2]))
 }


save.image(file="runrecoverytest_complete.RData")

