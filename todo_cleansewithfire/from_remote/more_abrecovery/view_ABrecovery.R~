library(tidyverse)
load("runrecoverytest_complete.RData")

funfacts.df <- simexp.df%>%group_by(ppntid)%>%summarize(Aweight=mean(Aweight),Bweight=mean(Bweight))%>%ungroup()#lazyhack, mean==max==min==unique_value for now.
                                                  
ppntAB.plot <- ggplot(mysamples.df)+geom_histogram(aes(x=Aweight,fill="A"),alpha=.5)+
    geom_histogram(aes(x=Bweight,fill="B"),alpha=.5)+
    facet_wrap(~ppntid)+
    geom_vline(data=funfacts.df,aes(xintercept=Aweight,color="A"),size=2)+
    geom_vline(data=funfacts.df,aes(xintercept=Bweight,color="B"),size=2)+
    theme_bw()

print(ppntAB.plot)
ggsave(ppntAB.plot,file="ABrecovery.png")
