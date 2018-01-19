rm(list=ls())
library(dplyr)
library(ggplot2)
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## if(any(list.files()=="multiord1.RData")){
##     load("multiord1.RData")#skip fitting.
## }else{

set.seed(4);

##exp params
hm_ppnts = 2;
hm_trials = 20; #per participant, not total.

##agent params:
calc_sd = abs(rnorm(hm_ppnts,3,.5))
tolerance_prob = rbeta(hm_ppnts,1,20)+.01
tolerance_payoff = abs(rnorm(hm_ppnts,1,1))+1
p_err = .01;


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

calc_obs <- function(aprob,apayoff,calcsd){
    #Why is noise associated with the product, and not the attributes? "log(attributevalue)+N(0,1/perceptAcuity)" on each attribute maybe worth a try? (with the same noise model for both calc and ord_difference?)
    return(rnorm(1,aprob*apayoff,calcsd)); #pulled out as a function so you can add alpha, U() etc in one place if needed later.
}

#random stim
#prob <- matrix(rbeta(hm_trials*3,1,1),ncol=3,nrow=hm_trials);
#payoff <- matrix(rnorm(hm_trials*3,100,5),ncol=3,nrow=hm_trials);#same set of stimuli repeated for all participants.

##wedell stim (well, wedell-ish, random selection of A,B, and decoy from AB and decoy pools)
source("wedellExp1Stim.R")
mystim <- getStim_twomatrixformat(hm_trials)
prob <- mystim[[1]]
payoff <- mystim[[2]]

impressions.df <- data.frame(ppntid=c(),trialid=c(),probA=c(),probB=c(),probC=c(),payoffA=c(),payoffB=c(),payoffC=c(),calcA=c(),calcB=c(),calcC=c(),ordprobAB=c(),ordprobAC=c(),ordprobBC=c(),ordpayoffAB=c(),ordpayoffAC=c(),ordpayoffBC=c()); #via df rather than directly to datalist for convenient inspection.

impressionrow <- 1;
for(ppnt in 1:hm_ppnts){
    for(trial in 1:hm_trials){
        impressions.df[impressionrow,"ppntid"] <- ppnt;
        impressions.df[impressionrow,"trialid"] <- trial;
        impressions.df[impressionrow,"probA"] <- prob[trial,1]
        impressions.df[impressionrow,"probB"] <- prob[trial,2]
        impressions.df[impressionrow,"probC"] <- prob[trial,3]
        impressions.df[impressionrow,"payoffA"] <- payoff[trial,1]
        impressions.df[impressionrow,"payoffB"]<- payoff[trial,2]
        impressions.df[impressionrow,"payoffC"]<- payoff[trial,3]
        impressions.df[impressionrow,"calcA"] <- calc_obs(prob[trial,1],payoff[trial,1],calc_sd[ppnt])
        impressions.df[impressionrow,"calcB"] <- calc_obs(prob[trial,2],payoff[trial,2],calc_sd[ppnt])
        impressions.df[impressionrow,"calcC"] <- calc_obs(prob[trial,3],payoff[trial,3],calc_sd[ppnt])
        impressions.df[impressionrow,"ordprobAB"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,trial,1,2,tolerance_prob[ppnt]))
        impressions.df[impressionrow,"ordprobAC"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,trial,1,3,tolerance_prob[ppnt]))
        impressions.df[impressionrow,"ordprobBC"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(prob,trial,2,3,tolerance_prob[ppnt]))
        impressions.df[impressionrow,"ordpayoffAB"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,trial,1,2,tolerance_payoff[ppnt]))
        impressions.df[impressionrow,"ordpayoffAC"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,trial,1,3,tolerance_payoff[ppnt]))
        impressions.df[impressionrow,"ordpayoffBC"] <- ifelse(obs_error(), obs_errorvalue(), obs_status(payoff,trial,2,3,tolerance_payoff[ppnt]))

        impressionrow <- impressionrow+1;#lazy rowfinder.
    }
}

datalist = list(#note agent doesn't see attributes, only 'impressions' of them
    N=nrow(impressions.df),
    hm_ppnts=length(unique(impressions.df$ppntid)),
    ppntid=impressions.df$ppntid,
    calc_sd=calc_sd,
    betawidth_prob= rep(5,hm_ppnts),#WARNING NEED TO UNDERSTAND THESE BETAWIDTHS BETTER. If they're too small,  the ord obs has no effect, estValue correlates perfectly with calcobs. If they're too big the impact of ordobs might also get too big? Also if they're too big, the ord-obs gets very step-like, the model slows down and starts struggling more. The right value is whatever best caputures people's actual tendency to perceive things as 'equal'. It would be nice to normalize somehow to sidestep scale differences? Right now they are different magnitudes to reflect the units of the attributes, but since they're guesses this could easily mean the attributes are getting different weights. Ugh!
    betawidth_payoff=rep(50,hm_ppnts),
    calc_observations=data.matrix(impressions.df[,c("calcA","calcB","calcC")]),
    ord_observations_prob=data.matrix(impressions.df[,c("ordprobAB","ordprobAC","ordprobBC")]),
    ord_observations_payoff=data.matrix(impressions.df[,c("ordpayoffAB","ordpayoffAC","ordpayoffBC")])
)

##stan setup                                        
stan_warmup = 800;
stan_iter = 2000; #note this includes warmup, ie if warmup=iter you get no samples.
stan_treedepth = 15; #would be nice to increase to ~20, but that does come with a time cost.

timer_full <- system.time(
    fit<- stan(file="howes16_multiord_multippnt.stan",data=datalist,warmup=stan_warmup,iter=stan_iter,chains=4,control = list(max_treedepth = stan_treedepth))
)

estvals <- apply(extract(fit,"estValue")$estValue,c(2,3),mean)
estvalsd <- apply(extract(fit,"estValue")$estValue,c(2,3),sd) #estval distributions are usually pretty normal-looking, but right-skewed when hitting floor of 0.
dimnames(estvals)=list(NULL,c("estA","estB","estC"))
dimnames(estvalsd)=list(NULL,c("estAsd","estBsd","estCsd"))

impressions.df <- cbind(impressions.df,as.data.frame(estvals))
impressions.df <- cbind(impressions.df,as.data.frame(estvalsd))

for(i in 1:nrow(impressions.df)){
impressions.df[i,"choice"]=which(impressions.df[i,c("estA","estB","estC")]==max(impressions.df[i,c("estA","estB","estC")])) #ignores chance of tie. Left as numbers rather than converting to ABC because these are about to be fed back into Stan.
}

#by all means record impressions.df somwhere, but this is what you're allowed to observe.
simexp.df <- impressions.df[,c("ppntid","trialid","probA","probB","probC","payoffA","payoffB","payoffC","choice")]

write.csv(simexp.df,file="simexp.csv",row.names=FALSE)
write.csv(impressions.df,file="impressions.csv",row.names=FALSE)
##print(prob);
##print(payoff);
##launch_shinystan(fit_sans);

                                        #}#ends "if couldn't just load from RData"
