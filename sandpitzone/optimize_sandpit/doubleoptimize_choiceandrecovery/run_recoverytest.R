library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv(file="simexp.csv")%>%select(contains("attribute"),ppntid,Aweight,Bweight,contains("value"),contains("choice"))
hm_ppnts <- length(unique(simexp.df$ppntid))
simexp.df$trialid <- rep(1:(nrow(simexp.df)/hm_ppnts), each=hm_ppnts)

simexp.df <- simexp.df%>%filter(ppntid<10)

runtime <- system.time(
{fit.recovery <<- webppl(program_file="ABrecovery.ppl",data=simexp.df,data_var="expdf")} #,packages=c("webppl-json")
)
save(fit.recovery,file="fitrecovery.RData")

estB <- as.numeric(map(fit.recovery,"data.0"))
trueB <- unique(simexp.df$Bweight)

save.image(file="runrecoverytest_complete.RData")

recovery.plot <- ggplot(data.frame(est=estB,simtruth=trueB),aes(x=simtruth,y=est))+geom_point()+
    geom_line(aes(y=simtruth,color='simtruth'))+
    geom_point(aes(y=simtruth,color='simtruth'))+
    ggtitle(paste("r^2=",cor(estB,trueB)))+
    theme_bw()


print(paste("ran in ",data.frame(time=runtime[["elapsed"]]/60)))
print(recovery.plot)
