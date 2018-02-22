library(tidyverse)
rm(list=ls())

stim.df <- read.csv("superstim_withchoices.csv")
stim.df$trialid <- rep(1:60,each=2)

agreement <- filter(stim.df,ppntid==0)$choice==filter(stim.df,ppntid==1)$choice
sum(agreement)
