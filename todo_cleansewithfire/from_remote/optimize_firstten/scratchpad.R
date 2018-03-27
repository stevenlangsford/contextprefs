library(tidyverse)
rm(list=ls())
load("runrecoverytest_complete.RData")

recovery.plot <- ggplot(data.frame(est=estB,simtruth=trueB),aes(x=simtruth,y=est))+geom_point()+
    geom_line(aes(y=simtruth,color='simtruth'))+
    geom_point(aes(y=simtruth,color='simtruth'))+
    ggtitle(paste("r^2=",cor(estB,trueB)))+
    theme_bw()


print(paste("ran in ",data.frame(time=runtime[["elapsed"]]/60)))
print(recovery.plot)
