rm(list=ls())
library(dplyr)
library(ggplot2)
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(any(list.files()=="multiord1.RData")){
    load("multiord1.RData")#skip fitting.
}else{
    
set.seed(4);

##agent params:
calc_sd = 3;
tolerance_prob = .011;
tolerance_payoff = 1.1;
p_err = .01;

##exp params
hm_trials = 10;

##setup sim data
obs_error <- function(){#decides if an error occured
    rbinom(1,1,p_err)==1;
}

obs_status <- function(target,i,j,k,tolerance){#returns a true relation (between attributes j and k in row i of target)
    ##using 1, 2, 3 to stand in for <, =, >
    if(abs(target[i,j]-target[i,k])<tolerance) {
        return(2);
    }else{
        if(target[i,j]<target[i,k]){
            return(1);
        } else{
            return(3);
        }
    }
}
obs_errorvalue <- function(){#error relations are uniform at random
    base::sample(c(1,2,3),1)
}

prob <- matrix(rbeta(hm_trials*3,1,1),ncol=3,nrow=hm_trials)
payoff <- matrix(rnorm(hm_trials*3,100,5),ncol=3,nrow=hm_trials)

##setup impressions: this is what the agent sees.
calc_observations = matrix(ncol=3,nrow=hm_trials)
##ord obs use the index to mean [ab, ac, bc] comparison
##value is 1=lessthan 2=equal 3=greaterthan
ord_prob = matrix(ncol=3,nrow=hm_trials)
ord_payoff = matrix(ncol=3,nrow=hm_trials)

for(i in 1:hm_trials){
    for(j in 1:3){
        calc_observations[i,j] = rnorm(1,prob[i,j]*payoff[i,j],calc_sd);
    }#end for each option

    ##ab
    ord_prob[i,1] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,i,1,2,tolerance_prob))
    ##ac
    ord_prob[i,2] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,i,1,3,tolerance_prob))
    ##bc
    ord_prob[i,3] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,i,2,3,tolerance_prob))
    ##ab
    ord_payoff[i,1] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,i,1,2,tolerance_payoff))
    ##ac
    ord_payoff[i,2] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,i,1,3,tolerance_payoff))
    ##bc
    ord_payoff[i,3] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,i,2,3,tolerance_payoff))    
}#end for each trial


##Stan bit:

datalist = list(#note agent doesn't see attributes, only 'impressions' of them
    N=hm_trials,
    K=3,##three options assumed.
    calc_obs=calc_observations,
    ord_observations_prob= ord_prob,
    ord_observations_payoff=ord_payoff
)

#stan setup: set once to keep the three fits in synch
stan_warmup = 500;
stan_iter = 1500; #note this includes warmup, ie if warmup=iter you get no samples.
stan_treedepth = 15; #would be nice to increase to ~20, but that does come with a time cost.

timer_full <- system.time(
    fit_full<<- stan(file="howes16_multiord.stan",data=datalist,warmup=stan_warmup,iter=stan_iter,chains=4,control = list(max_treedepth = stan_treedepth))
)

save.image(file="multiord1.RData");
View("done");

##print(prob);
##print(payoff);
##launch_shinystan(fit_sans);
    
}#ends "if couldn't just load from RData"
