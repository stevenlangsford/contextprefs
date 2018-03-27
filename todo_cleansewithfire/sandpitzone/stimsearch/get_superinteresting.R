library(tidyverse)
library(rwebppl)
rm(list=ls())

stim.df <- data.frame();
for(afile in grep("interesting.*csv",list.files(),value=TRUE)){
    stim.df <- rbind(stim.df,read.csv(afile))
}

stim.df <- stim.df%>%filter(optionchosen=="A")%>%select(contains("prob"),contains("payoff"))%>%distinct

rowarrange <- function(arow){
    canonical <- order(arow[,1:3])
    #browser()
    data.frame(probA=arow[,which(canonical==1)],
               probB=arow[,which(canonical==2)],
               probC=arow[,which(canonical==3)],
               payoffA=arow[,which(canonical==1)+3],
               payoffB=arow[,which(canonical==2)+3],
               payoffC=arow[,which(canonical==3)+3])
}

sorted.df <- data.frame()
for(i in 1:nrow(stim.df))sorted.df <- rbind(sorted.df,rowarrange(stim.df[i,]))

# Determine number of clusters
wss <- (nrow(sorted.df)-1)*sum(apply(sorted.df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(sorted.df,
                                     centers=i)$withinss)

## plot(1:15, wss, type="b", xlab="Number of Clusters",
##      ylab="Within groups sum of squares") #4 or 5?

fit <- kmeans(sorted.df, 4)
sorted.df$stimtype=fit$cluster


appendExpectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}
sorted.df <- appendExpectation(sorted.df)

for(i in 1:nrow(sorted.df)){
    mywinner <- max(sorted.df[i,c("exA","exB","exC")])
    mysecond <- sort(sorted.df[i,c("exA","exB","exC")],partial=2)[2]
    sorted.df[i,"wingap"] <- mywinner-mysecond
}

sorted.df <- filter(sorted.df,wingap<5)

## hm_ea <- 15
## candidate.df <- rbind(sample_n(filter(sorted.df,stimtype==1),hm_ea,replace=FALSE),
##       sample_n(filter(sorted.df,stimtype==2),hm_ea,replace=FALSE),
##       sample_n(filter(sorted.df,stimtype==3),hm_ea,replace=FALSE),
##       sample_n(filter(sorted.df,stimtype==4),hm_ea,replace=FALSE))%>%select(contains("prob"),contains("payoff"))

## write.csv(candidate.df,"superstim.csv",row.names=FALSE)

write.csv(sorted.df,file="all_superstim_firstpass.csv",row.names=FALSE)
