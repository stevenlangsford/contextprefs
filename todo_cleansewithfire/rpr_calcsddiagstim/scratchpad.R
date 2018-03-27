library(tidyverse)
rm(list=ls())
rprReader <- function(filestring){
return (read.csv(filestring)%>%select(pA,pB,pD,vA,vB,vD,ChoicesMadeCount,Achoices,Bchoices,Dchoices)%>%
        mutate(propA=Achoices/ChoicesMadeCount,propB=Bchoices/ChoicesMadeCount,propC=Dchoices/ChoicesMadeCount)
        )
    }

rpr_lo.df <- rprReader("rpr_localcsddiagstim.csv")
rpr_hi.df <- rprReader("rpr_hicalcsddiagstim.csv")

rpr_lo.df$ppntid=0
rpr_hi.df$ppntid=1

rpr.df <- rbind(rpr_lo.df,rpr_hi.df)
rm(list=c("rpr_lo.df","rpr_hi.df"))

ppl.df <- read.csv("webppl_hilocalcsd_choices.csv")%>%
    gather(rep,optionchosen,choice1:choice20)%>%
    group_by(ppntid,stimid)%>%
    summarize(pplpropA=sum(optionchosen==1)/n(),pplpropB=sum(optionchosen==2)/n(),pplpropC=sum(optionchosen==3)/n())%>%ungroup()

combo.df <- cbind(rpr.df,ppl.df)
combo.df$diffA=with(combo.df,propA-pplpropA)
combo.df$diffB=with(combo.df,propB-pplpropB)
combo.df$diffC=with(combo.df,propC-pplpropC)
combo.df$ppntid <- as.factor(combo.df$ppntid)
combo.df$stimid <- as.factor(combo.df$stimid)

## with(combo.df,plot(pplpropA,propA))
## with(combo.df,plot(pplpropB,propB))
## with(combo.df,plot(pplpropC,propC)) ##etc etc
