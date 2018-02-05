library(tidyverse)
rm(list=ls())
append.expectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

stim.df <- append.expectation(read.csv("gotchoices.csv"))


closestpair <- function(a,b,c){    
    diffs <- c(abs(a-b),abs(a-c), abs(b-c))
    return(
        diffs[which(diffs==min(diffs))] #warning, ties break this
    )
}

for(i in 1:nrow(stim.df)){
    stim.df[i,"mingap"] <- closestpair(stim.df[i,"exA"],stim.df[i,"exB"],stim.df[i,"exC"])
}

ggplot(stim.df,aes(x=mingap))+geom_histogram(binwidth=.1)+theme_bw() #Well, there are lots of small mingaps, but a few > 5 and even >10.
