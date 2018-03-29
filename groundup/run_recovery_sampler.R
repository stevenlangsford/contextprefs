library(tidyverse)
library(rwebppl)
library(patchwork)
rm(list=ls())

exp.df <- read.csv("simexp_sanschoices.csv")


fit <- webppl(program_file="recoverAgent_sampler.ppl",data=exp.df,data_var="datadf",packages=c("webppl-json"));
save.image("recoveryran_SMCsampler.RData")

exp.df$ppntid <- as.factor(exp.df$ppntid)
est.df = map(fit$value, function(x){data.frame(ppntid=0:(length(x)-1),est=x)})%>%bind_rows
est.df$ppntid <- as.factor(est.df$ppntid)
simtruth.df <- exp.df%>%group_by(ppntid)%>%summarize(Bweight=max(Bweight))%>%ungroup()%>%data.frame
est.summary.df <- est.df%>%group_by(ppntid)%>%summarize(meanB=mean(est))%>%ungroup()%>%data.frame
est.summary.df$simtruth <- simtruth.df[simtruth.df$ppntid%in%est.summary.df$ppntid,"Bweight"]


ggsave(
(ggplot(est.df,aes(x=est))+geom_histogram()+theme_bw()+facet_wrap(~ppntid)+geom_vline(data=simtruth.df,aes(xintercept=Bweight)))/
(ggplot(est.summary.df,aes(x=simtruth,y=meanB))+geom_point()+theme_bw()+ylim(c(0,1)))
,file="choiceobs_SMC200_10.png")

View("done SMCing")
