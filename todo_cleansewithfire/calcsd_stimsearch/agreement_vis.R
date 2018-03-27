library(tidyverse)
rm(list=ls())
exp.df <- read.csv("testdf.csv")
hm_ppnts <- max(exp.df$ppntid)+1#ugh
hm_trials <- nrow(exp.df)/hm_ppnts #ugh
exp.df$trialid <- rep(1:hm_trials,each=hm_ppnts)#sigh

exp.df$ppntid <- as.factor(exp.df$ppntid)
tall.df <- exp.df%>%gather(choicenumber,opt_chosen,choice1:choice20)

choice_proportion.df <- tall.df%>%
    group_by(ppntid,trialid,probA,probB,probC,payoffA,payoffB,payoffC)%>%
    summarize(A=sum(opt_chosen==1)/n(),B=sum(opt_chosen==2)/n(),C=sum(opt_chosen==3)/n())%>%
    ungroup()%>%
    as.data.frame

proportion_byoption.df <- choice_proportion.df%>%gather(optionchosen,propchosen,A:C)

isInteresting <- function(atrialid){
    threshold <- .2

    ppnt1 <- choice_proportion.df[as.character(choice_proportion.df$trialid)==as.character(atrialid)&choice_proportion.df$ppntid==0,c("A","B","C")]
    ppnt2 <- choice_proportion.df[as.character(choice_proportion.df$trialid)==as.character(atrialid)&choice_proportion.df$ppntid==1,c("A","B","C")]

    return(any(abs(c(ppnt1$A-ppnt2$A,ppnt1$B-ppnt2$B,ppnt1$C-ppnt2$C))>threshold))
    #return(atrialid%%2==0)
}

proportion_byoption.df$isInteresting <- sapply(proportion_byoption.df$trialid,isInteresting)
repnumber <- sum(grepl("interesting",list.files()))

write.csv(filter(proportion_byoption.df,isInteresting),file=paste0("interesting",repnumber,".csv"))

stimcheck.plot <- ggplot(proportion_byoption.df,aes(x=optionchosen,y=propchosen,shape=ppntid,fill=optionchosen,color=optionchosen))+
     geom_rect(aes(fill = isInteresting),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
    facet_wrap(~trialid)+
    geom_point(size=5)+
    scale_fill_brewer()+
    theme_bw()

ggsave(stimcheck.plot,file=paste0("stimcheck",repnumber,".png"))
#ggsave(stimcheck.plot,file="stimcheck.png")
