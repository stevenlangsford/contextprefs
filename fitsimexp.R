rm(list=ls())
library(rstan)
library(shinystan)
library(ggplot2)
library(dplyr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

simexp.df <- read.csv("simexp.csv")

datalist <- list(
    N=nrow(simexp.df),
    hm_ppnts=length(unique(simexp.df$ppntid)),
    ppntid=simexp.df$ppntid,
    choice=simexp.df$choice,
    true_prob=data.matrix(simexp.df[,c("probA","probB","probC")]),
    true_payoff=data.matrix(simexp.df[,c("payoffA","payoffB","payoffC")])
)

stan_warmup = 10;
stan_iter = 20; 
stan_treedepth = 10; 

timer <- system.time(
    fit<- stan(file="recover_agentparams.stan",data=datalist,warmup=stan_warmup,iter=stan_iter,chains=4,control = list(max_treedepth = stan_treedepth))
)
