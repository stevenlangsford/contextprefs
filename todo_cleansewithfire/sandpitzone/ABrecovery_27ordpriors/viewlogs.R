library(tidyverse)
library(jsonlite)
library(patchwork)

rm(list=ls())

denull <- function(x){if(is.null(x)){return("is.null");}else return(x)}
log.df <- data.frame()
for(afile in list.files("logs/")){
    print(paste("getting",afile))
    log.df <- rbind(log.df,as.data.frame(map(fromJSON(paste0("logs/",afile)),denull)))
}


simexp.df <- read.csv("simexp.csv")
simtruth.df <- simexp.df%>%group_by(ppntid)%>%summarize(Bweight=max(Bweight))%>%ungroup()

logsummary.df <- log.df%>%group_by(ppntid)%>%summarize(Bweight=mean(Bweight))%>%ungroup();


recovery.plot <- ggplot(log.df,aes(x=Bweight))+geom_histogram()+facet_wrap(~ppntid)+
    geom_vline(data=simtruth.df,aes(xintercept=Bweight))+
    geom_vline(data=logsummary.df,aes(xintercept=Bweight,color="est"))+
    geom_vline(data=data.frame(priormean=5/2),aes(xintercept=priormean,color="prior"),linetype="dashed")+
    theme_bw()


optionest_vs_simtruth.plot <- ggplot(log.df,aes(x=myA1-simA1))+geom_histogram()+theme_bw()+
ggplot(log.df,aes(x=myA2-simA2))+geom_histogram()+theme_bw()+
ggplot(log.df,aes(x=myA3-simA3))+geom_histogram()+theme_bw()+
ggplot(log.df,aes(x=myB1-simB1))+geom_histogram()+theme_bw()+
ggplot(log.df,aes(x=myB2-simB2))+geom_histogram()+theme_bw()+
ggplot(log.df,aes(x=myB3-simB3))+geom_histogram()+theme_bw()

log.df$simBweight <- sapply(log.df$ppntid,function(id){as.numeric(simtruth.df[simtruth.df$ppntid==id,"Bweight"])})

cor.plot <- (ggplot(log.df,aes(x=simBweight,y=Bweight))+geom_point()+theme_bw())/
    (ggplot(log.df,aes(x=simBweight,y=Bweight,group=simBweight))+geom_boxplot()+theme_bw())


recovery.plot+cor.plot


logsummary.df <- log.df%>%group_by(simA1,simA2,simA3,simB1,simB2,simB3)%>%summarize(
                                                                              meanA1=mean(myA1),
                                                                              meanA2=mean(myA2),
                                                                              meanA3=mean(myA3),
                                                                              meanB1=mean(myB1),
                                                                              meanB2=mean(myB2),
                                                                              meanB3=mean(myB3))

ggplot(logsummary.df,aes(x=simA1,y=meanA1))+geom_point()
ggplot(logsummary.df,aes(x=simB1,y=meanB1))+geom_point()
                                                                              
                                                                              
                                                                              
                                                                                    
