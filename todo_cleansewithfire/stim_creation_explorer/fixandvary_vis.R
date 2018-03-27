rm(list=ls())
library(tidyverse)
choices.df <- read.csv("fixandvary_withchoices.csv")
choices.df$ppntid <- as.factor(choices.df$ppntid)
choices.df$stimset <- as.factor(rep(c("fix_expectation","fix_payoff","fix_prob"),each=35*2))

choices.df$hm_A <- select(choices.df,contains("choice"))%>%apply(1,function(x){sum(x==1)})
choices.df$hm_B <- select(choices.df,contains("choice"))%>%apply(1,function(x){sum(x==2)})
choices.df$hm_C <- select(choices.df,contains("choice"))%>%apply(1,function(x){sum(x==3)})

prop.df <- choices.df%>%select(stimset,ppntid,probA,probB,probC,payoffA,payoffB,payoffC,contains("hm_"))%>%gather(key=option,value=chosen,hm_A:hm_C)

fix.payoff.plot <- ggplot(prop.df%>%filter(stimset=="fix_payoff"),aes(x=abs(probA-probC),y=chosen,fill=option))+
    geom_bar(stat="identity",position="dodge")+
    facet_grid(ppntid~.)+
    theme_bw()

fix.prob.plot <- ggplot(prop.df%>%filter(stimset=="fix_prob"),aes(x=abs(payoffA-payoffC),y=chosen,fill=option))+
    geom_bar(stat="identity",position="dodge")+
    facet_grid(ppntid~.)+
    theme_bw()

fix.expectation.plot <- ggplot(prop.df%>%filter(stimset=="fix_expectation"),aes(x=abs(payoffA-payoffC),y=chosen,fill=option))+
    geom_bar(stat="identity",position="dodge")+
    facet_grid(ppntid~.)+
    theme_bw()
