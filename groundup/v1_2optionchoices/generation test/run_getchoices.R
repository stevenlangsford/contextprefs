library(tidyverse)
library(rwebppl)
library(patchwork)
rm(list=ls())

exp.df <- read.csv("simexp_sanschoices.csv")
exp.df$ppntid <- as.factor(exp.df$ppntid)

exp.df$choice <- webppl(program_file="getChoices.ppl",data=exp.df,data_var="datadf",packages=c("webppl-json"));

exp.summary.df <- exp.df%>%group_by(ppntid,trialid)%>%summarize(prop1=sum(choice==1)/n())%>%ungroup()


discrimination.plot <- (ggplot(exp.summary.df,aes(x=trialid,y=prop1,color=ppntid,group=ppntid))+geom_point()+geom_line()+theme_bw()+guides(color=FALSE))+(
    ggplot(exp.summary.df,aes(x=trialid,y=prop1,color=ppntid,group=ppntid))+geom_point()+geom_line()+theme_bw()+facet_wrap(~ppntid)+guides(color=FALSE))

saveplots <- FALSE
if(saveplots==TRUE){ggsave(discrimination.plot,file="discriminationishard.png",width=15);
}else{print(discrimination.plot)}

save.image("discriminationishard20x100rnd.RData")

## est.df <-  data.frame(ppntid=1:length(fit)-1,est=fit)

## mysummary.df <- exp.df%>%group_by(ppntid)%>%summarize(Bweight=mean(Bweight))%>%ungroup()%>%plyr::join(est.df,by="ppntid")

## ##print(est.df)
## print(paste("r=",with(mysummary.df,cor(Bweight,est))))

## saveplots <- FALSE;

## recovery.plot <- ggplot(mysummary.df)+
##     geom_point(aes(x=Bweight,y=est,color="recovered"))+
##     geom_smooth(aes(x=Bweight,y=est,color="recovered"),method='lm')+
##     ggtitle(paste("r=",with(mysummary.df,signif(cor(Bweight,est),3))))+
##     theme_bw()+guides(color=FALSE)

## vstruth.plot <- ggplot(mysummary.df)+
##     geom_line(aes(x=Bweight,y=Bweight,color="sim"),alpha=.5)+geom_point(aes(x=Bweight,y=Bweight,color="sim"),alpha=.5,size=2)+
##     geom_point(aes(x=Bweight,y=est,color="recovered"))+ggtitle(paste("r=",with(mysummary.df,signif(cor(Bweight,est),3))))+
##    ylim(c(0,1))+
##     theme_bw()


## if(saveplots){
##     ggsave(recovery.plot,file="recoveredonly_withfactor.png")
## }else{
##     print(vstruth.plot+recovery.plot);
## }
