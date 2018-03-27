library(tidyverse)
rm(list=ls())
set.seed(4)
appendExpectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

ppl.df <- read.csv("horserace_webpplchoices_sd50.csv")%>%select(1:6,choice)%>%appendExpectation #sd10 choices match sd5 ones at 0.929, return is indistinguishable.
for(i in 1:nrow(ppl.df)){
    ppl.df[i,"agentReturn"] <- ppl.df[i,c("exA","exB","exC")][ppl.df[i,"choice"]]
    ppl.df[i,"oracleReturn"] <- max(ppl.df[i,c("exA","exB","exC")])
}

rpr.df <- read.csv("rpr_sd50.csv")%>%select(4:9,ChoicesMadeCount,Achoices,Bchoices,Dchoices)%>%
    mutate(propA=Achoices/ChoicesMadeCount,
           propB=Bchoices/ChoicesMadeCount,
           propC=Dchoices/ChoicesMadeCount
           )
names(rpr.df)[1:6] <- names(ppl.df)[1:6]
rpr.df <- appendExpectation(rpr.df)

for(i in 1:nrow(rpr.df)){
    #rpr.df[i,"agentReturn"] <- rpr.df[i,"propA"]*rpr.df[i,"exA"]+rpr.df[i,"propB"]*rpr.df[i,"exB"]+rpr.df[i,"propC"]*rpr.df[i,"exC"]
    rpr.df[i,"agentReturn"] <- base::sample(rpr.df[i,c("exA","exB","exC")],1,prob=rpr.df[i,c("propA","propB","propC")])
}

horserace.df <- data.frame(model=c("ppl","rpr"),
                           winnings=c(sum(ppl.df$agentReturn)/nrow(ppl.df),sum(rpr.df$agentReturn)/nrow(rpr.df)),
                           ci.low=c(sum(ppl.df$agentReturn)/nrow(ppl.df)+qnorm(.025)*sd(ppl.df$agentReturn)/sqrt(nrow(ppl.df)),
                                    sum(rpr.df$agentReturn)/nrow(rpr.df)+qnorm(.025)*sd(rpr.df$agentReturn)/sqrt(nrow(rpr.df))
                                    ),
                           ci.high=c(sum(ppl.df$agentReturn)/nrow(ppl.df)+qnorm(.975)*sd(ppl.df$agentReturn)/sqrt(nrow(ppl.df)),
                                     sum(rpr.df$agentReturn)/nrow(rpr.df)+qnorm(.975)*sd(rpr.df$agentReturn)/sqrt(nrow(rpr.df))
                                     )
                           )
    
horserace.plot <- ggplot(horserace.df,aes(x=model,y=winnings))+
    geom_bar(stat="identity")+
    geom_hline(data=data.frame(oracle=sum(ppl.df$oracleReturn)/nrow(ppl.df)),aes(yintercept=oracle,color="oracle"))+
    geom_errorbar(aes(ymin=ci.low,ymax=ci.high),width=.5)+
    coord_cartesian(ylim=c(horserace.df$ci.low[1]*.9,horserace.df$ci.high[1]*1.1))+
    ylab("Average return on choices")+
    theme_bw()

ggsave(horserace.plot,file="maximization_performance_sd50.png")
