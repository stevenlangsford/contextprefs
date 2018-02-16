library(tidyverse)
exp.df <- read.csv("fixandvarystim_differentpayoffpriors.csv")

exp.df <- exp.df%>%transform(trialid=1:nrow(ppl.df))%>%
    gather(choiceRep,optionChosen,choice1:choice50)%>%
    group_by(trialid,ppntid,probA,probB,probC,payoffA,payoffB,payoffC)%>%
    summarize(propA=sum(optionChosen==1)/n(),propB=sum(optionChosen==2)/n(),propC=sum(optionChosen==3)/n())%>%
    ungroup()

prefs.plot <- ggplot(exp.df,aes(x=trialid%%35))+facet_grid(ppntid~.)+
#    geom_point(aes(y=rpr_propA,shape="A",color="rpr"))+geom_line(aes(y=rpr_propA,linetype="A",color="rpr"))+
    geom_point(aes(y=ppl_propA,shape="A",color="ppl"))+geom_line(aes(y=ppl_propA,linetype="A",color="ppl"))+
#    geom_point(aes(y=rpr_propB,shape="B",color="rpr"))+geom_line(aes(y=rpr_propB,linetype="B",color="rpr"))+ #A+B+C=1, so one can be dropped, B fixed @ (.5,100)
#    geom_point(aes(y=ppl_propB,shape="B",color="ppl"))+geom_line(aes(y=ppl_propB,linetype="B",color="ppl"))+
#    geom_point(aes(y=rpr_propC,shape="C",color="rpr"))+geom_line(aes(y=rpr_propC,linetype="C",color="rpr"))+
    geom_point(aes(y=ppl_propC,shape="C",color="ppl"))+geom_line(aes(y=ppl_propC,linetype="C",color="ppl"))+    
    theme_bw()
