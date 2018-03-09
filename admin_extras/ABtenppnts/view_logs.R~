library(tidyverse)
library(jsonlite)
rm(list=ls())

simsetup.df <- read.csv("ABsimexp_withchoices.csv")
mysamples.df <- map_dfr(paste0("test/",list.files("test/")),function(x){return(as.data.frame(fromJSON(x)))})

length(unique(mysamples.df$simA3)) #unique trials covered (why no trialid?)
table(mysamples.df$ppntid)# How far through the ppnts are you? Last checked 2343 and 1517 for ppnts 0 and 1, dropping to 2~4 at 12~14.

                                        #welp, this is fun, but peeking is nowhere near as much fun as actual posteriors. The relation between these and the posterior is probably more complicated than you think!

targppnt = 0;
ggplot(mysamples.df%>%filter(ppntid==targppnt))+geom_histogram(aes(x=Aweight))+
    geom_vline(aes(xintercept=mean(Aweight),color="recovered"))+
    geom_vline(data=simsetup.df%>%filter(ppntid==targppnt),aes(xintercept=mean(Aweight),color="simtruth"))+
    theme_bw()
