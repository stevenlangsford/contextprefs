library(tidyverse)
library(patchwork)
rm(list=ls())

load("recoveryran.RData")
simexp.df <- read.csv("simexp.csv")


est.df = map(fit$value, function(x){data.frame(ppntid=0:(length(x)-1),est=x)})%>%bind_rows

simtruth.df <- simexp.df%>%group_by(ppntid)%>%summarize(Bweight=max(Bweight))%>%ungroup()%>%data.frame
est.summary.df <- est.df%>%group_by(ppntid)%>%summarize(meanB=mean(est))%>%ungroup()%>%data.frame
est.summary.df$simtruth <- simtruth.df[simtruth.df$ppntid%in%est.summary.df$ppntid,"Bweight"]
    



(ggplot(est.df,aes(x=est))+geom_histogram()+theme_bw()+facet_wrap(~ppntid)+geom_vline(data=simtruth.df,aes(xintercept=Bweight)))/
(ggplot(est.summary.df,aes(x=simtruth,y=meanB))+geom_point()+theme_bw())
