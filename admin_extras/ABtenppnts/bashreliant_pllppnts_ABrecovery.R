library(tidyverse)
library(rwebppl)
##Assume you're being called from Rscript with a valid ppntid arg

##args <- commandArgs(trailingOnly = TRUE)
myppntid <- 4#as.integer(args[1])

simexp.df <- read.csv(file="ABsimexp_withchoices.csv")%>%select(contains("attribute"),ppntid,Aweight,Bweight,contains("value"),contains("choice"))

if(!(myppntid%in%simexp.df$ppntid))stop(paste("Bad ppntid arg ",myppntid))
    
hm_ppnts <- max(simexp.df$ppntid)+1
hm_items <- nrow(simexp.df)/hm_ppnts
simexp.df$trialid <- rep(1:hm_items,each=hm_ppnts)

##MINIRUN FILTERS. iterate on small stuff.
simexp.df <- filter(simexp.df,ppntid==myppntid)%>%as.data.frame
hm_ppnts <- length(unique(simexp.df$ppntid)) #1
hm_items <- length(unique(simexp.df$trialid))
##END MINI

runtime <- system.time({
    fit.recovery <<- webppl(program_file="ABrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))
})
#save(fit.recovery,file="fitrecovery.RData")

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

save.image(file=paste("runrecoverytest_nobashppnt4_",myppntid,".RData"))
