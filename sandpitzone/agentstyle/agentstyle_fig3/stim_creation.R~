library(tidyverse)
library(rwebppl)
##Supports simulated-experiment building scripts
#Assumes the participant setup is done before any of these functions are called
##Functions: append.expectation(exp.df), assignStim(stim,ppntid), getXYZTrial,addChoices(simexp.df)

##A simulated-experiment should init ppnts, create a bunch of trials using getXYZTrial, assign trials to participants, then add simulated choices.


##format for ppnt-setting. Use "replicate(times,fn())" if you want to a function to generate each value
## hm_ppnts = 2
## ppnt_calcsd <- c(5,20)
## ppnt_tolerance_prob <- rep(.01,hm_ppnts) 
## ppnt_tolerance_payoff <- rep(1.1,hm_ppnts)
## ppnt_orderror <- rep(.1,hm_ppnts)
## useord <- TRUE
## usecalc <- TRUE
## payoffprior_mean <- with(stim.df,mean(c(payoffA,payoffB,payoffC)))
## payoffprior_sd <- with(stim.df,sd(c(payoffA,payoffB,payoffC)))

if(!exists("hm_ppnts"))stop("Sourced stim_creation without ppnt inits set") 
if(!exists("ppnt_calcsd"))stop("Sourced stim_creation without ppnt inits set")
if(!exists("ppnt_tolerance_prob"))stop("Sourced stim_creation without ppnt inits set")
if(!exists("ppnt_tolerance_payoff"))stop("Sourced stim_creation without ppnt inits set")
if(!exists("ppnt_orderror"))stop("Sourced stim_creation without ppnt inits set")
if(!exists("useord"))stop("Sourced stim_creation without ppnt inits set")
if(!exists("usecalc"))stop("Sourced stim_creation without ppnt inits set")


##helper functions
appendExpectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

##Let trial generation functions generate stim properties, add ppnt params to each row to be read by the .ppl fit
##NOTE it might make sense to assign payoff priors here, but they're left in the stim generation functions, because those are aware of the true payoff distributions, and the prior should match that (probably exactly, for now). Random and Wedell are pretty different, for example. If one day you want individual differences in payoff priors this will have to change.
assignStim <- function(stim,ppntid){
    stim$ppntid <- ppntid-1 #convert from R 1-index to webppl 0-index (both use ppntid as index into an array)
    stim$calc_sd = ppnt_calcsd[ppntid]
    stim$tolerance_prob =  ppnt_tolerance_prob[ppntid]
    stim$tolerance_payoff =  ppnt_tolerance_payoff[ppntid]
    stim$p_err = ppnt_orderror[ppntid]
    stim$useord = useord #set up with ppnt properties, but assigned here for consistency.
    stim$usecalc = usecalc

    return(stim);
}

bulkAssignStim <- function(stim.df){
    ret.df <- data.frame()
    for(i in 1:nrow(stim.df)){
        for(ppnt in 1:hm_ppnts){
            ret.df <- rbind(ret.df,assignStim(stim.df[i,],ppnt))
        }
    }
    return(ret.df)
}


##Trial generation functions:
getRandomTrial <- function(){
    payoff.dist <- function(){
        rnorm(1,100,5)
    }
    prob.dist <- function(){
        rbeta(1,1,1)
    }
    return(
        data.frame(
            probA = prob.dist(),
            probB = prob.dist(),
            probC = prob.dist(),
            payoffA = payoff.dist(),
            payoffB = payoff.dist(),
            payoffC = payoff.dist(),

            payoffprior_mean= 100,
            payoffprior_sd= 5
            )
    )
}

##sets default args for everything, just fix the things you want to change.
getCustomTrial <- function(probA=.5,probB=.5,probC=.5,payoffA=100,payoffB=100,payoffC=100,payoffprior_mean=100,payoffprior_sd=5){
    return(
        data.frame(
            probA = probA,
            probB = probB,
            probC = probC,
            payoffA = payoffA,
            payoffB = payoffB,
            payoffC = payoffC,
            
            payoffprior_mean= payoffprior_mean,
            payoffprior_sd= payoffprior_sd
        )
    )
}

getWedellesqueTrial <- function(){
    #Not very wedellesque to just mix and match! Oh well.
    prob.dist <- function(){
        base::sample(c(.3,.4,.5,.67,.83),1)
    }
    payoff.dist <- function(){
        base::sample(c(10,20,18,13,33,25,15,12,30),1)
    }
    return(
        data.frame(
            probA = prob.dist(),
            probB = prob.dist(),
            probC = prob.dist(),
            payoffA = payoff.dist(),
            payoffB = payoff.dist(),
            payoffC = payoff.dist(),

            payoffprior_mean= 20,
            payoffprior_sd= 8.5
            )
    )
}

#fixed expectation of all options at 50, B is (.5,100), payoffA is 100-stepsize, payoffC is 100+stepsize, probs adjusted to make all expectations equal.
getTradeoffTrial <- function(payoff_stepsize){
    probA=50/(100-payoff_stepsize)
    probC=50/(100+payoff_stepsize)
    return(getCustomTrial(probA=probA,probC=probC,payoffA=100-payoff_stepsize,payoffC=100+payoff_stepsize))#everything else is default-town.
}

getSetSpreadTrial <- function(stepsize){
    payoff.dist <- function(){
        rnorm(1,100,5)
    }

    ##Set gap size by fixing payoffs and adjusting probs such that difference in expectation between neighbors is stepsize
    ##Done this way round because the prior on payoffs is kinda restrictive, but probs can fall anywhere, so this is more model-friendly.
    payoffs <- sort(c(payoff.dist(),payoff.dist(),payoff.dist()))

    #Distribution of middle option is almost the same as rndTrial, but truncated so that expected values of min trial never go below 0.
    centerpoint <- max(payoff.dist()*rbeta(1,1,1),stepsize+1)

    probA <- (centerpoint-stepsize)/payoffs[1]
    probB <- centerpoint/payoffs[2]
    probC <- (centerpoint+stepsize)/payoffs[3]

    #Keeping the randomly dealt payoffs and insisting on positive expectation means sometimes you'd need a probability greater than one to get the expectation spread you want. If that happens, just take the highest prob you can, and increase the payoff until the expectation spread is fixed. This messes up the match between payoff priors and actual payoffs somewhat. Lesser evil than probs greater than one.
    if(probA>1){probA <- 1; payoffs[1] <- centerpoint-stepsize}
    if(probB>1){probB <- 1; payoffs[2] <- centerpoint}
    if(probC>1){probC <- 1; payoffs[3] <- centerpoint+stepsize}

    return(
        data.frame(
            probA = probA,
            probB = probB,
            probC = probC,
            payoffA = payoffs[1],
            payoffB = payoffs[2],
            payoffC = payoffs[3],
            
            payoffprior_mean= 100,
            payoffprior_sd= 5
            )
    )
}

##Experiment generation zone

addChoices <- function(simexp.df,modelname="howes16.ppl"){
fit <- webppl(program_file=modelname,data=simexp.df,data_var="expdf")

simexp.df$choice <- fit[[8]] ##most important bit
##calcobs
calcobs <- fit[[1]]
simexp.df$calcA <- calcobs[,1]
simexp.df$calcB <- calcobs[,2]
simexp.df$calcC <- calcobs[,3]
##ordobs
simexp.df$ABprob <- fit[[2]]
simexp.df$ACprob <- fit[[3]]
simexp.df$BCprob <- fit[[4]]

simexp.df$ABpayoff <- fit[[5]]
simexp.df$ACpayoff <- fit[[6]]
simexp.df$BCpayoff <- fit[[7]]

return(simexp.df);    
}
