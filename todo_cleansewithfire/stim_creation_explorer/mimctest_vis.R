library(tidyverse)
rm(list=ls())
test.trials.df <- read.csv("mimic_testtrials.csv")

choice.df <- test.trials.df%>%gather(choicenumber,optionchosen,choice1:choice500)%>%
    group_by(X)%>% #X is stim id, called X 'cause auto gen from row name I guess.
    summarize(propA=sum(optionchosen==1)/n(),propB=sum(optionchosen==2)/n(),propC=sum(optionchosen==3)/n())%>%
    ungroup()

rpr.df <- read.csv("mimictest/rpr_results.csv")%>%select(ChoicesMadeCount,Achoices,Bchoices,Dchoices)%>%
    transmute(X=1:n(),propA=Achoices/ChoicesMadeCount,propB=Bchoices/ChoicesMadeCount,propC=Dchoices/ChoicesMadeCount)

choice.df$model="webppl"
rpr.df$model="rpr"

combo.df <- rbind(choice.df,rpr.df)

byitem.plot <- ggplot(combo.df,aes(x=X,shape=model,size=5))+
    geom_point(aes(y=propA,color="A"))+
    geom_point(aes(y=propB,color="B"))+
    geom_point(aes(y=propC,color="C"))+theme_bw()


corr.df <- rbind(data.frame(itemid=1:nrow(choice.df),webppl=choice.df$propA, rpr=rpr.df$propA),
                 data.frame(itemid=1:nrow(choice.df),webppl=choice.df$propB, rpr=rpr.df$propB),
                 data.frame(itemid=1:nrow(choice.df),webppl=choice.df$propC, rpr=rpr.df$propC)
                 )

corr.plot <- ggplot(corr.df,aes(x=rpr,y=webppl,color=as.factor(itemid)))+geom_point()+
    geom_text(aes(label=itemid),hjust=2,vjust=1)+
    theme_bw()


winnerlist <- function(my.df){
    props = my.df%>%select(propA,propB,propC)
    for(i in 1:nrow(props)){
        winner <- which(props[i,c("propA","propB","propC")]==max(props[i,c("propA","propB","propC")]))
        props[i,"winner"] <- winner
        }
    return(props$winner)
}

sum(winnerlist(rpr.df)==winnerlist(choice.df)) #10 of 19. Not great.

appendExpectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

choice.df$model <- NULL
choice.df$X <- NULL
rpr.df$model <- NULL
rpr.df$X <- NULL
names(choice.df) <- paste0(names(choice.df),"_ppl")
names(rpr.df) <- paste0(names(rpr.df),"_rpr")

sidebyside.df <- cbind(appendExpectation(test.trials.df[,1:7]),choice.df,rpr.df)
write.csv(sidebyside.df,file="sidebyside.csv",row.names=FALSE)


for(i in 1:nrow(sidebyside.df)){
    sidebyside.df[i,"comparison_distance"] <- sqrt(
    (sidebyside.df[i,"propA_ppl"]-sidebyside.df[i,"propA_rpr"])^2+
    (sidebyside.df[i,"propB_ppl"]-sidebyside.df[i,"propB_rpr"])^2+
    (sidebyside.df[i,"propC_ppl"]-sidebyside.df[i,"propC_rpr"])^2
    )
}


distance.plot <- ggplot(sidebyside.df,aes(x=comparison_distance))+geom_histogram(binwidth=.01)+theme_bw()
ggsave(distance.plot,file="distances.png")
