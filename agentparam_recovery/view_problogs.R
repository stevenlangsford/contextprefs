library(tidyverse)
library(jsonlite)
rm(list=ls())
saveplots <- FALSE #hey you have est calcsd hanging around on those agent saves, color by that instead of ppntid, check ppnt/est_calcsd match.
plotfolder <- "problog_plots/"

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

agents.df$agentchoice <- sapply(1:nrow(agents.df),function(i){which(agents.df[i,c("estA","estB","estC")]==max(agents.df[i,c("estA","estB","estC")]))})
agents.df$oraclechoice <- sapply(1:nrow(agents.df),function(i){which(agents.df[i,c("simexA","simexB","simexC")]==max(agents.df[i,c("simexA","simexB","simexC")]))})

##Agent performance: Agreement with oracle vs ppntid, agreement with oracle vs est_calcsd. If est_calcsd doesn't track agreement-with-oracle the world is broken.
##Ideally, ppntid and est_calcsd should be strongly related!
ppnt_performance.df <- as.data.frame(with(agents.df,table(sim_ppntid,agentchoice==oraclechoice))/apply(with(agents.df,table(sim_ppntid,agentchoice==oraclechoice)),1,sum))%>%filter(Var2==TRUE)%>%select(-Var2, agreement = Freq)
ppnt_performance.plot <- ggplot(ppnt_performance.df,aes(x=sim_ppntid,y=agreement))+geom_bar(stat="identity")+theme_bw()+ggtitle("ppnt performance")

get_paramplot <- function(paramname){
pzero <- "ppnt0"
pone <- "ppnt1"
ggplot()+
    geom_histogram(data=filter(agents.df,sim_ppntid==0),aes_string(x=paramname,fill="pzero"),alpha=.5)+
    geom_histogram(data=filter(agents.df,sim_ppntid==1),aes_string(x=paramname,fill="pone"),alpha=.5)+
    ggtitle(paramname)+
    theme_bw()
}

ggsave(get_paramplot("est_calcsd"),file=paste0(plotfolder,"calcsd.png"))
ggsave(get_paramplot("est_tolerance_prob"),file=paste0(plotfolder,"tolerance_prob.png"))
ggsave(get_paramplot("est_tolerance_payoff"),file=paste0(plotfolder,"tolerance_payoff.png"))
ggsave(get_paramplot("est_orderr"),file=paste0(plotfolder,"orderr.png"))
ggsave(get_paramplot("est_payoffpriormean"),file=paste0(plotfolder,"payoffpriormean.png"))
ggsave(get_paramplot("est_payoffpriorsd"),file=paste0(plotfolder,"payoffpriorsd.png"))


