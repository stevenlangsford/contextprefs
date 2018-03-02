library(tidyverse)
library(jsonlite)
rm(list=ls())
saveplots <- TRUE
plotfolder <- "plots/"

load(file="minruncomplete_image.RData")

mysamples.df <- data.frame();

for(asample in recovery.fit[[1]]){ #[[1]] is samples, [[2]] is scores
    for(ppnt in 1:hm_ppnts){
        mysamples.df <- rbind(mysamples.df,data.frame(param="calcsd",
                                                      ppntid=ppnt,
                                                      sample=asample[1,ppnt])
                              )
        mysamples.df <- rbind(mysamples.df,data.frame(param="tolerance_prob",
                                                      ppntid=ppnt,
                                                      sample=asample[2,ppnt])
                              )
        mysamples.df <- rbind(mysamples.df,data.frame(param="tolerance_payoff",
                                                      ppntid=ppnt,
                                                      sample=asample[3,ppnt])
                              )
        mysamples.df <- rbind(mysamples.df,data.frame(param="orderr",
                                                      ppntid=ppnt,
                                                      sample=asample[4,ppnt])
                              )
    }   
}


 param.plot <- function(paramname){
     ggplot(mysamples.df%>%filter(param==paramname),aes(x=sample))+geom_histogram(aes(fill=as.factor(ppntid)))+facet_wrap(~as.factor(ppntid))+theme_bw()+
         ggtitle(paramname)
 }

for(aparam in unique(mysamples.df$param)){ggsave(param.plot(aparam),file=paste0(plotfolder,aparam,".png"))}
