library(tidyverse)
library(patchwork)
rm(list=ls())


getEstgaps <- function(saveplots=TRUE){
estgaps.df <- data.frame();    
for(afile in grep("runrecoverytest_complete*",list.files(),value=TRUE)){
load(afile)
myA.plot <- ggplot(mysamples.df,aes(x=Aweight))+geom_histogram()+
    geom_vline(aes(xintercept=mean(Aweight),color="recovered"))+
    geom_vline(data=simexp.df,aes(xintercept=mean(Aweight),color="simtruth"))+
    guides(color=FALSE)+
    theme_bw()

myB.plot <- ggplot(mysamples.df,aes(x=Bweight))+geom_histogram()+
    geom_vline(aes(xintercept=mean(Bweight),color="recovered"))+
    geom_vline(data=simexp.df,aes(xintercept=mean(Bweight),color="simtruth"))+
    theme_bw()

if(saveplots)ggsave(myA.plot+myB.plot,file=paste0("recovery_histograms/",strsplit(afile,".RData")[[1]],".png"),width=15)

estgaps.df <- rbind(estgaps.df,data.frame(estgapA=mean(simexp.df$Aweight)-mean(mysamples.df$Aweight),estgapB=mean(simexp.df$Bweight)-mean(mysamples.df$Bweight)))
}
return(estgaps.df);
}


afile = grep("runrecoverytest_complete*",list.files(),value=TRUE)[[4]]

load(afile)
#View(simexp.df)
#View(mysamples.df)
