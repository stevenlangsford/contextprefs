library(tidyverse)
library(rwebppl)

stim.df <- read.csv("hilocalcsd_diagstim.csv")

hm_ppnts = 2
ppnt_calcsd <- c(1,20)
ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
ppnt_orderror <- rep(.1,hm_ppnts)
useord <- TRUE
usecalc <- TRUE
ppnt_payoffprior_mean = rep(20,hm_ppnts)
ppnt_payoffprior_sd = rep(8,hm_ppnts)

source("stim_creation.R")

simexp.df <- bulkAssignStim(stim.df)%>%addChoices

write.csv(simexp.df,file="simexp.csv",row.names=FALSE)
