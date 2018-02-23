library(tidyverse)
rm(list=ls())

appendExpectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

appendChoiceReturn <- function(exp.df){
    for(i in 1:nrow(exp.df)){
        exp.df[i,"choiceReturn"] <- exp.df[i,c("exA","exB","exC")][exp.df[i,"choice"]]
    }
    return(exp.df)
}

simexp.df <- read.csv("simexp.csv")%>%appendExpectation%>%appendChoiceReturn

funfacts.df <- simexp.df%>%group_by(ppntid)%>%summarize(meanreturn=mean(choiceReturn))
ggplot(simexp.df,aes(x=choiceReturn))+facet_grid(ppntid~.)+geom_histogram()+theme_bw()+geom_vline(data=funfacts.df,aes(xintercept=meanreturn))
