library(dplyr)
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
rm(list=ls())

#setup
stan_warmup = 500;
stan_iter = 1500; #note this includes warmup, ie if warmup=iter you get no samples.
stan_treedepth = 15; #would be nice to increase to ~20, but that does come with a time cost.

#impressions contain details of agent setup, the stimuli they faced, and their "impressions" (calc and ord) of those stimuli following howes16.
impressions.df <- read.csv("impressions.csv")

                                        #for each trial, have the participant infer which option is best given only the impressions and knowledge of their own setup parameters, ie tolerance, p.err, and calc.sd

for(ppnt in unique(impressions.df$ppntID)){
ppnt=1
    my.df <- filter(impressions.df,ppntID==ppnt)
    my.calcobservations = matrix(c(my.df$calcA,my.df$calcB,my.df$calcC),nrow=nrow(my.df),ncol=3,byrow=FALSE)
    my.ordprob = matrix(c(my.df$probAB,my.df$probAC,my.df$probBC),nrow=nrow(my.df),ncol=3,byrow=FALSE)
    my.ordpayoff = matrix(c(my.df$payoffAB,my.df$payoffAC,my.df$payoffBC),nrow=nrow(my.df),ncol=3,byrow=FALSE)
    
datalist = list(
    N=nrow(my.df),
    K=3,##three options assumed.
    calc_observations=my.calcobservations,
    ord_observations_prob= my.ordprob,
    ord_observations_payoff=my.ordpayoff,
    calc_sd=my.df$ppnt_calcsd[1],#repeated value, any one will do
    tolerance_prob=my.df$ppnt_tolerance_prob[1],
    tolerance_payoff=my.df$ppnt_tolerance_payoff[1]
)

    fit <-stan(file="impressionReader.stan",data=datalist,warmup=stan_warmup,iter=stan_iter,chains=4, control = list(max_treedepth = stan_treedepth))

estValue <- extract(fit,"estValue")$estValue #3d matrix, [iteration, trial, option]. iteration in 1:stan_iter, trial in 1:nrow(my.df), option in 1:3
for(atrial in unique(my.df$trial)){
    impressions.df[impressions.df$ppntID==ppnt&impressions.df$trial==atrial,"estValue_1"]=mean(estValue[,atrial,1])
    impressions.df[impressions.df$ppntID==ppnt&impressions.df$trial==atrial,"estValue_2"]=mean(estValue[,atrial,2])
    impressions.df[impressions.df$ppntID==ppnt&impressions.df$trial==atrial,"estValue_3"]=mean(estValue[,atrial,3])
}
}#end for each participant

##fill in the actual choices
for(i in 1:nrow(impressions.df)){
    impressions.df[i,"choice"] <- which(impressions.df[1,c("estValue_1","estValue_2","estValue_3")]==max(impressions.df[1,c("estValue_1","estValue_2","estValue_3")]))
}

write.csv(impressions.df,"simChoicedata.csv")
