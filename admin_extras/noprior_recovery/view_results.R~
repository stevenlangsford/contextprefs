library(tidyverse)
library(jsonlite)
rm(list=ls())
saveplots <- TRUE #hey you have est calcsd hanging around on those agent saves, color by that instead of ppntid, check ppnt/est_calcsd match.
plotfolder <- "ploglog_plots/"
folder="problogs/"
agents <- grep("agent*",list.files(folder),value=TRUE)
agents.df <- data.frame()
for(anagent in agents){
    agents.df <- rbind(agents.df,as.data.frame(fromJSON(paste0(folder,anagent))))
}
rm("agents")
agents.df$simexA = with(agents.df,sim_pA*sim_vA)
agents.df$simexB = with(agents.df,sim_pB*sim_vB)
agents.df$simexC = with(agents.df,sim_pC*sim_vC)

optionest.df <- with(agents.df,
                     data.frame(
                         sim=c(simexA,simexB,simexC),
                         agent=c(estA,estB,estC),
                         ppntid=as.factor(rep(sim_ppntid,3)),
                         est_calcsd=as.factor(rep(est_calcsd,3))
                     ))

probest.df <- with(agents.df,
                     data.frame(
                         sim=c(sim_pA,sim_pB,sim_pC),
                         agent=c(est_pa,est_pb,est_pc),
                         ppntid=as.factor(rep(sim_ppntid,3)),
                         est_calcsd=as.factor(rep(est_calcsd,3))
                     ))

payoffest.df <- with(agents.df,
                     data.frame(
                         sim=c(sim_vA,sim_vB,sim_vC),
                         agent=c(est_va,est_vb,est_vc),
                         ppntid=as.factor(rep(sim_ppntid,3)),
                         est_calcsd=as.factor(rep(est_calcsd,3))
                     ))

optionest.plot <- ggplot(optionest.df,aes(x=sim,y=agent,color=est_calcsd,shape=ppntid,group=est_calcsd))+geom_point(size=3)+theme_bw()+geom_smooth()
probest.plot <- ggplot(probest.df,aes(x=sim,y=agent,color=est_calcsd,shape=ppntid,group=est_calcsd))+geom_point(size=3)+theme_bw()+geom_smooth()
payoffest.plot <- ggplot(payoffest.df,aes(x=sim,y=agent,color=est_calcsd,shape=ppntid,group=est_calcsd))+geom_point(size=3)+theme_bw()+geom_smooth()

agents.df$agentchoice <- sapply(1:nrow(agents.df),function(i){which(agents.df[i,c("estA","estB","estC")]==max(agents.df[i,c("estA","estB","estC")]))})
agents.df$oraclechoice <- sapply(1:nrow(agents.df),function(i){which(agents.df[i,c("simexA","simexB","simexC")]==max(agents.df[i,c("simexA","simexB","simexC")]))})

if(saveplots==TRUE){
ggsave(optionest.plot,file=paste0(plotfolder,"agent_optionest.png"))
ggsave(probest.plot,file=paste0(plotfolder,"agent_probest.png"))
ggsave(payoffest.plot,file=paste0(plotfolder,"agent_payoff.png"))
}

##Agent performance: Agreement with oracle vs ppntid, agreement with oracle vs est_calcsd. If est_calcsd doesn't track agreement-with-oracle the world is broken.
##Ideally, ppntid and est_calcsd should be strongly related!
ppnt_performance.df <- as.data.frame(with(agents.df,table(sim_ppntid,agentchoice==oraclechoice))/apply(with(agents.df,table(sim_ppntid,agentchoice==oraclechoice)),1,sum))%>%filter(Var2==TRUE)%>%select(-Var2, agreement = Freq)
ppnt_performance.plot <- ggplot(ppnt_performance.df,aes(x=sim_ppntid,y=agreement))+geom_bar(stat="identity")+theme_bw()+ggtitle("ppnt performance")

estcalcsd_performance.df <- as.data.frame(with(agents.df,table(est_calcsd,agentchoice==oraclechoice))/apply(with(agents.df,table(est_calcsd,agentchoice==oraclechoice)),1,sum))%>%filter(Var2==TRUE)%>%select(-Var2, agreement = Freq)
estcalcsd_performance.plot <- ggplot(estcalcsd_performance.df,aes(x=est_calcsd,y=agreement))+geom_bar(stat="identity")+theme_bw()+ggtitle("estcalcsd performance")

idsdallocation.plot <- ggplot(agents.df,aes(x=est_calcsd,fill=as.factor(sim_ppntid)))+geom_bar(position="dodge")+theme_bw()

if(saveplots){
    ggsave(ppnt_performance.plot,file=paste0(plotfolder,"ppntperformance.png"))
    ggsave(estcalcsd_performance.plot,file=paste0(plotfolder,"estcalcsdperformance.png"))
    ggsave(idsdallocation.plot,file=paste0(plotfolder,"idallocation.png"))
    }

####################################################################################################


calcord <- grep("calcord*",list.files(folder),value=TRUE)
calcord.df <- data.frame()
for(anobs in calcord){
    calcord.df <- rbind(calcord.df,as.data.frame(fromJSON(paste0(folder,anobs))))
}
rm("calcord")

####################################################################################################
decision <- grep("decision*",list.files(folder),value=TRUE)



## cauchyprior.df = data.frame(x=rcauchy(1000,location=5,scale=2))

## cauchyprior.plot <- ggplot()+
##     geom_histogram(data=cauchyprior.df,aes(x=x, fill="prior"),alpha=.2,color="grey")+
##     geom_histogram(data=filter(agents.df,sim_ppntid==0),aes(x=est_calcsd, fill="ppnt0"),alpha=.5)+
##     geom_histogram(data=filter(agents.df,sim_ppntid==1),aes(x=est_calcsd, fill="ppnt1"),alpha=.5)+
##     theme_bw()+
##     xlim(c(0,20))+xlab("calcsd")+ggtitle("Estimating calcsd")
