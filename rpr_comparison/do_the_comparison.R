library(tidyverse)
rm(list=ls())
rpr.df <- read.csv("fixandvary_rpr.csv")%>%select(position,pA,pB,pD,vA,vB,vD,ChoicesMadeCount,Achoices,Bchoices,Dchoices)%>%
    transform(propA=Achoices/ChoicesMadeCount,propB=Bchoices/ChoicesMadeCount,propC=Dchoices/ChoicesMadeCount,trialid=1:105)
ppl.df <- read.csv("fixandvary_webppl.csv")
ppl.df <- ppl.df%>%transform(trialid=1:nrow(ppl.df),position=rep(c("fixExp","fixPay","fixProb"),each=35))%>%
    gather(choiceRep,optionChosen,choice1:choice50)%>%
    group_by(trialid,position,probA,probB,probC,payoffA,payoffB,payoffC)%>%
    summarize(propA=sum(optionChosen==1)/n(),propB=sum(optionChosen==2)/n(),propC=sum(optionChosen==3)/n())%>%
    ungroup()

rpr.choices.df <- rpr.df%>%select(propA,propB,propC)
names(rpr.choices.df) <- paste0("rpr_",names(rpr.choices.df))
ppl.choices.df <- ppl.df%>%select(propA,propB,propC)
names(ppl.choices.df) <- paste0("ppl_",names(ppl.choices.df))

combo.df <- data.frame(trialid=1:105,position=rpr.df$position)%>%cbind(rpr.choices.df)%>%cbind(ppl.choices.df)

combo.plot <- ggplot(combo.df,aes(x=trialid%%35))+facet_grid(position~.)+
    geom_point(aes(y=rpr_propA,shape="A",color="rpr"))+geom_line(aes(y=rpr_propA,linetype="A",color="rpr"))+
    geom_point(aes(y=ppl_propA,shape="A",color="ppl"))+geom_line(aes(y=ppl_propA,linetype="A",color="ppl"))+
#    geom_point(aes(y=rpr_propB,shape="B",color="rpr"))+geom_line(aes(y=rpr_propB,linetype="B",color="rpr"))+ #A+B+C=1, so one can be dropped, B fixed @ (.5,100)
#    geom_point(aes(y=ppl_propB,shape="B",color="ppl"))+geom_line(aes(y=ppl_propB,linetype="B",color="ppl"))+
    geom_point(aes(y=rpr_propC,shape="C",color="rpr"))+geom_line(aes(y=rpr_propC,linetype="C",color="rpr"))+
    geom_point(aes(y=ppl_propC,shape="C",color="ppl"))+geom_line(aes(y=ppl_propC,linetype="C",color="ppl"))+    
    theme_bw()
ggsave(combo.plot,file="rpr_ppl_comparison.png")
