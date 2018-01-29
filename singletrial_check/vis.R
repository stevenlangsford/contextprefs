library(jsonlite)
library(tidyverse)

choiceVis <- function(fileprefix,titleinfo){
A <- fromJSON(paste0(fileprefix,"_Adistribution.json"))
B <- fromJSON(paste0(fileprefix,"_Bdistribution.json"))
C <- fromJSON(paste0(fileprefix,"_Cdistribution.json"))

alltogether <- data.frame(sample = c(A,B,C),option=c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C))))

return(ggplot(alltogether,aes(x=sample,group=option,color=option))+geom_density()+theme_bw()+ggtitle(titleinfo))

}

decoyImpact <- function(fileprefix){
A <- fromJSON(paste0(fileprefix,"_Adistribution.json"))
B <- fromJSON(paste0(fileprefix,"_Bdistribution.json"))
C <- fromJSON(paste0(fileprefix,"_Cdistribution.json"))

Atiny <- fromJSON(paste0(fileprefix,"_tinydecoy_Adistribution.json"))
Btiny <- fromJSON(paste0(fileprefix,"_tinydecoy_Bdistribution.json"))
Ctiny <- fromJSON(paste0(fileprefix,"_tinydecoy_Cdistribution.json"))

#alltogether <- data.frame(sample = c(A,B,C),option=c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C))))
compareA.df <- data.frame(sample=c(A,Atiny),trialtype=c(rep("realdecoy",length(A)),rep("tinydecoy",length(Atiny))))
compareB.df <- data.frame(sample=c(B,Btiny),trialtype=c(rep("realdecoy",length(B)),rep("tinydecoy",length(Btiny))))
compareC.df <- data.frame(sample=c(C,Ctiny),trialtype=c(rep("realdecoy",length(C)),rep("tinydecoy",length(Ctiny))))

compareA.df$plotframe="evaluation of A"
compareB.df$plotframe="evaluation of B"
compareC.df$plotframe="evaluation of C"

compareall.df <- rbind(compareA.df,compareB.df,compareC.df)
compareall.plot <- ggplot(compareall.df,aes(x=sample,group=trialtype,color=trialtype))+geom_density()+facet_wrap(~plotframe)+theme_bw()
return(compareall.plot)
}
