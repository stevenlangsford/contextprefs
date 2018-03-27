library(tidyverse)
library(rwebppl)
rm(list=ls())

stim.df <- head(read.csv("superstim.csv"))

hm_ppnts = 1
ppnt_calcsd <- c(5)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE
payoffprior_mean <- with(stim.df,mean(c(payoffA,payoffB,payoffC)))
payoffprior_sd <- with(stim.df,sd(c(payoffA,payoffB,payoffC)))

source("stim_creation.R")
stim.df <- bulkAssignStim(stim.df)
stim.df$payoffprior_mean <- with(stim.df,mean(c(payoffA,payoffB,payoffC)))
stim.df$payoffprior_sd <- with(stim.df,sd(c(payoffA,payoffB,payoffC)))

starttime <- Sys.time();
fit <- webppl(program_file="agent.ppl",data=stim.df,data_var="expdf")
stim.df$choice <- fit
endtime <- Sys.time();

