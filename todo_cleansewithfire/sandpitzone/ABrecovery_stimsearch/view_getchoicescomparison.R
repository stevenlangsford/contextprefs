library(tidyverse)
rm(list=ls())

load(file="getchoices_lmvshowes_ran.RData")

lm_simexp.df$decisionmaker <- "linear"
howes_simexp.df$decisionmaker <- "howes"

##TO DELETE## upstream script now fixed.
#names(lm_simexp.df)[7] <- "trialid"
#names(howes_simexp.df)[7] <- "trialid"
##end to delete

expectedReturn <- function(i,adf){
    adf[i,"value1"]*adf[i,"prop_opt1"]+adf[i,"value2"]*adf[i,"prop_opt2"]+adf[i,"value3"]*adf[i,"prop_opt3"]
}
lm_simexp.df$expectedReturn <- sapply(1:nrow(lm_simexp.df),function(i)expectedReturn(i,lm_simexp.df))
howes_simexp.df$expectedReturn <- sapply(1:nrow(howes_simexp.df),function(i)expectedReturn(i,howes_simexp.df))


consistencyMeasure <- function(i,adf){
    mylog <- function(x){
    if(x==0)return(0);
    return (log2(x));
}
    
    return(-1*(adf[i,"prop_opt1"]*mylog(adf[i,"prop_opt1"])+
               adf[i,"prop_opt2"]*mylog(adf[i,"prop_opt2"])+
               adf[i,"prop_opt3"]*mylog(adf[i,"prop_opt3"]))
           )
}

lm_simexp.df$consistencyMeasure <- sapply(1:nrow(lm_simexp.df),function(i)consistencyMeasure(i,lm_simexp.df))
howes_simexp.df$consistencyMeasure <- sapply(1:nrow(howes_simexp.df),function(i)consistencyMeasure(i,howes_simexp.df))

lm_simexp.df%>%group_by(ppntid)%>%summarize(performance=sum(expectedReturn))
howes_simexp.df%>%group_by(ppntid)%>%summarize(performance=sum(expectedReturn))

lm_simexp.df%>%group_by(ppntid)%>%summarize(consistancy=sum(consistencyMeasure))
howes_simexp.df%>%group_by(ppntid)%>%summarize(consistancy=sum(consistencyMeasure))


difference.df <- data.frame(diff1=lm_simexp.df$prop_opt1-howes_simexp.df$prop_opt1,
                            diff2=lm_simexp.df$prop_opt2-howes_simexp.df$prop_opt2,
                            diff3=lm_simexp.df$prop_opt3-howes_simexp.df$prop_opt3,
                            trialid=lm_simexp.df$trialid,
                            ppntid=lm_simexp.df$ppntid
                            )%>%filter(!trialid%in%c(3,4,15,17,21,22,23,28))#failed crude eyeball-the-plot boringness test.

ggplot(difference.df)+facet_grid(ppntid~trialid)+
    geom_bar(aes(x=1,y=diff1),stat="identity")+
    geom_bar(aes(x=2,y=diff2),stat="identity")+
    geom_bar(aes(x=3,y=diff3),stat="identity")+
    theme_bw()
    

## combo.df <- rbind(lm_simexp.df,howes_simexp.df)%>%select(trialid,ppntid,contains("prop_opt"),decisionmaker)
## combo.df$ppntid <- as.factor(combo.df$ppntid)
## ##NOW WHAT? visualize the agreement: ppnts have different weights, but the same ppnt/weights appear in the two decision-maker flavors, which is the critical bit.
## #Go wide with the ppnts: the agreement/disagreement probably depends on the weights? How to see this?

## abdistance.plot <- ggplot(combo.df,aes(x=prop_opt1,y=prop_opt2,color=decisionmaker,shape=ppntid))+geom_point(size=2,alpha=.5)+facet_wrap(~trialid)+theme_bw()

## bars.plot <- ggplot(combo.df,aes(group=ppntid,fill=ppntid))+
##     geom_bar(aes(x=1,y=prop_opt1),stat="identity",position="dodge")+
##     geom_bar(aes(x=2,y=prop_opt2),stat="identity",position="dodge")+
##     geom_bar(aes(x=3,y=prop_opt3),stat="identity",position="dodge")+
##     facet_wrap(~trialid)+
##     theme_bw()

## ggsave(abdistance.plot,file="abdistance.png",width=15)
## ggsave(bars.plot,file="prefbars.png",width=15)

## print(abdistance.plot);x11();print(bars.plot)
