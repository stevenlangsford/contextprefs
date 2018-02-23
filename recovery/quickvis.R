library(tidyverse)
fit.samples <- read.csv(file="fitsamples.csv")

idlist <- unique(fit.samples$ppnt_id)
truth.df <- data.frame(ppnt_id=idlist,simtruth=c(8,20))
prior.df <- data.frame(ppnt_id=idlist,priormean = 10)
recovered.df <- data.frame(ppnt_id=idlist,
                           postmean=(fit.samples%>%group_by(ppnt_id)%>%summarize(postmean=mean(sample))%>%arrange(ppnt_id)%>%ungroup())$postmean
                           )

fit.plot <- ggplot(fit.samples,aes(x=sample))+geom_histogram()+facet_wrap(~ppnt_id)+
    geom_vline(data=truth.df,aes(xintercept=simtruth,color="simtruth"))+
    geom_vline(data=prior.df,aes(xintercept=priormean,color="prior"))+
    geom_vline(data=recovered.df,aes(xintercept=postmean,color="recovered"))+
    theme_bw()
