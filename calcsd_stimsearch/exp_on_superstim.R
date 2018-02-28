library(tidyverse)
library(rwebppl)
rm(list=ls())

hm_ppnts = 2
ppnt_calcsd <- c(1,15)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE

source("stim_creation.R")

stim.df <- read.csv("superstim.csv")%>%bulkAssignStim
#priors match actual distributions
stim.df$payoffprior_mean <- with(stim.df,mean(c(payoffA,payoffB,payoffC)))
stim.df$payoffprior_sd <-  with(stim.df,sd(c(payoffA,payoffB,payoffC)))

stim.df <- addChoices(stim.df)
write.csv(stim.df,"superstim_withchoices.csv",row.names=FALSE)
