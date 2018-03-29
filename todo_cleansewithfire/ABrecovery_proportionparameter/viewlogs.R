library(tidyverse)
library(jsonlite)
library(patchwork)
rm(list=ls())

## denull <- function(x){if(is.null(x)){return("is.null");}else return(x)}
 log.df <- data.frame()
 for(afile in list.files("logs/")){
     print(paste("getting",afile))
     bob <- map(fromJSON(paste0("logs/",afile)),function(x){if(is.list(x)){return(x["x"]);} else return(x)})
     log.df <- rbind(log.df,as.data.frame(
                                map(fromJSON(paste0("logs/",afile)),function(x){if(is.list(x)){return(x["x"]);} else return(x)})
                            ))
 }
names(log.df) <- names(bob)#OMG the ugliness why.
log.df$ppntid <- as.factor(log.df$ppntid)


optionsummary.df <- log.df%>%group_by(simA1,simA2,simA3,simB1,simB2,simB3)%>%summarize(
                                                   myA1=mean(myA1),
                                                   myA2=mean(myA2),
                                                   myA3=mean(myA3),
                                                   myB1=mean(myB1),
                                                   myB2=mean(myB2),
                                                   myB3=mean(myB3)
                                                   )%>%ungroup()
checkoptionests.df <- bind_cols(optionsummary.df%>%select(contains("sim"))%>%gather(whichoption,value,1:6),optionsummary.df%>%select(contains("my"))%>%gather(whichoption,value,1:6))


checkoptionests.plot <- ggplot(checkoptionests.df,aes(x=value,y=value1))+geom_point()+theme_bw()+xlab("simulation attribute value")+ylab("value estimated during recovery")
                                                         
## optionests.plot <- ggplot(log.df)+
##     geom_point(aes(x=simA1,y=myA1))+
##     geom_point(aes(x=simA2,y=myA2))+
##     geom_point(aes(x=simA3,y=myA3))+theme_bw()

## optionestsB.plot <- ggplot(log.df)+
##     geom_point(aes(x=simB1,y=myB1))+
##     geom_point(aes(x=simB2,y=myB2))+
##     geom_point(aes(x=simB3,y=myB3))+theme_bw()

#optionsummary.df <- log.df%>%group_by(rowname)%>%summarize(estA1=mean(myA1),simA1=mean(simA1))%>%ungroup()



Bsummary.df <- log.df%>%group_by(ppntid)%>%summarize(myBweight=mean(myBweight),simBweight=mean(simBweight))%>%ungroup()
#Brecovery.plot <- ggplot(Bsummary.df,aes(x=simBweight,y=myBweight))+geom_point()+theme_bw()

Brecovery.plot <- ggplot(log.df,aes(x=simBweight,y=myBweight,group=ppntid))+geom_boxplot()+theme_bw()+ylab("est B weight")

ggsave(Brecovery.plot+checkoptionests.plot,file="midrun_Bweight_optionests.png",width=15)

print(with(log.df,sum(mychoice==simchoice)/nrow(log.df)))
print(with(log.df,sum(simchoice==oraclechoice)/nrow(log.df))) #calcsd is pretty low for now to give recovery the best possible chance.
print(with(log.df,sum(mychoice==oraclechoice)/nrow(log.df)))
print(paste(length(unique(log.df$simA1)),"trials considered"))
print(paste(length(unique(log.df$ppntid)),"ppnts considered"))

logsummary.df <- log.df%>%group_by(ppntid)%>%summarize(myBweight=mean(myBweight),simBweight=mean(simBweight))%>%ungroup()
bhist.plot <- ggplot(log.df,aes(x=myBweight))+geom_histogram()+facet_wrap(~ppntid)+
    geom_vline(aes(xintercept=.5,color="prior"),alpha=.5,size=2,linetype="dashed")+
    geom_vline(data=logsummary.df,aes(xintercept=myBweight,color="recovered"))+
    geom_vline(data=logsummary.df,aes(xintercept=simBweight,color="simtruth"))+
    theme_bw()

ggsave(bhist.plot,file="bhists.png")
