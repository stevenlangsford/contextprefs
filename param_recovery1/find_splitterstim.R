rm(list=ls())
library(tidyverse)
#library(jsonlite) #name clash with purrr 'flatten'
library(rwebppl)

##Setup params
set.seed(4);
hm_ppnts = 2;
hm_trials = 20;
starttime=Sys.time()
##Agent population params
ppnt.calcsd.dist <- function(){
    10 #todo: draw from a population distribution, eg abs(rnorm(1,10,5)) or something.
}
ppnt.tolerance_prob.dist <- function(){
    0.11
}
ppnt.tolerance_payoff.dist <- function(){
    1.1
}
ppnt.orderror.dist <- function(){
    .1
}

##stim distribution params
prob.rnd.dist <- function(){
    rbeta(1,1,1)
}
payoff.rnd.dist <- function(){
    rnorm(1,100,10)
}

##Not very wedellesque to just mix and match! Oh well.
prob.wedell.dist <- function(){
    base::sample(c(.3,.4,.5,.67,.83),1)
}
payoff.wedell.dist <- function(){
    base::sample(c(10,20,18,13,33,25,15,12,30),1)
}

##misc helper functions
append.expectation <- function(stim.df){
    stim.df$exA <- stim.df$probA*stim.df$payoffA
    stim.df$exB <- stim.df$probB*stim.df$payoffB
    stim.df$exC <- stim.df$probC*stim.df$payoffC

    return(stim.df)
}

                                        #START HERE
##Get sim ppnts:
ppnt_calcsd = c(8,20) #replicate(hm_ppnts,ppnt.calcsd.dist())
ppnt_tolerance_prob = replicate(hm_ppnts,ppnt.tolerance_prob.dist())
ppnt_tolerance_payoff = replicate(hm_ppnts,ppnt.tolerance_payoff.dist())
ppnt_orderror = replicate(hm_ppnts,ppnt.orderror.dist())

##modelparams: switches in the model you might want to play with, passed in as data to avoid having multiple model files.
useord = TRUE
usecalc = TRUE

getStim <- function(prob.dist,payoff.dist,payoffprior.mean,payoffprior.sd){#assumes options, prob, payoff all independent.
    data.frame(
        probA = prob.dist(),
        probB = prob.dist(),
        probC = prob.dist(),
        payoffA = payoff.dist(),
        payoffB = payoff.dist(),
        payoffC = payoff.dist(),
        payoffprior_mean = payoffprior.mean,
        payoffprior_sd = payoffprior.sd
    )
}

#Non-independant sim generators
#Rig the deck so that two options are close in expectation: interesting trials can't have a clear winner or be all identical.
getAABStim <- function(prob.dist,payoff.dist,payoffprior.mean,payoffprior.sd){
                                        #TODO
}

getStaircaseStim <- function(prob.dist,payoff.dist,stepsize){ #stepsize is in expected value.
                                        #TODO
}

assignStim <- function(stim,ppntid){
    stim$ppntid <- ppntid-1 #convert from R 1-index to webppl 0-index (both use ppntid as index into an array)
    stim$calc_sd = ppnt_calcsd[ppntid]
    stim$tolerance_prob =  ppnt_tolerance_prob[ppntid]
    stim$tolerance_payoff =  ppnt_tolerance_payoff[ppntid]
    stim$p_err = ppnt_orderror[ppntid]
    stim$useord = TRUE
    stim$usecalc = TRUE

    return(stim);
}

bulk.assignStim <- function(stim.df){
    ret.df <- data.frame()
    for(i in 1:nrow(stim.df)){
        for(ppnt in 1:hm_ppnts){
            ret.df <- rbind(ret.df,assignStim(stim.df[i,],ppnt))
        }
    }
    return(ret.df)
}

simexp.df <- data.frame();
for(trial in 1:hm_trials){
    atrial <- getStim(prob.dist=prob.rnd.dist,payoff.dist=payoff.rnd.dist,payoffprior.mean=100,payoffprior.sd=10)
    atrial$trial_id=trial
    
    for(ppnt in 1:hm_ppnts){
        simexp.df <- rbind(simexp.df,assignStim(atrial,ppnt)) #rbind(simexp.df,getRandomTrial(ppnt))
    }
}

agreement.filter <- function(simexp.df){
    fit1 <- webppl(program_file="howes16.ppl",data=simexp.df,data_var="expdf")
    fit2 <- webppl(program_file="howes16.ppl",data=simexp.df,data_var="expdf")

    append.fitcols <- function(my.df,my.fit){
        my.df$choice <- my.fit[[8]] ##most important bit
        ##calcobs
        calcobs <- my.fit[[1]]
        my.df$calcA <- calcobs[,1]
        my.df$calcB <- calcobs[,2]
        my.df$calcC <- calcobs[,3]
        ##ordobs
        my.df$ABprob <- my.fit[[2]]
        my.df$ACprob <- my.fit[[3]]
        my.df$BCprob <- my.fit[[4]]

        my.df$ABpayoff <- my.fit[[5]]
        my.df$ACpayoff <- my.fit[[6]]
        my.df$BCpayoff <- my.fit[[7]]

        ## with(my.df, {
        ##     calcinfo <<- ifelse(usecalc, paste("calc:",signif(calcA,3),signif(calcB,3),signif(calcC,3)), "calc_off");
        ##     ordinfo <<- ifelse(useord, paste("pAB",ABprob,"pAC",ACprob,"pBC",BCprob,"vAB",ABpayoff,"vAC",ACpayoff,"vBC",BCpayoff),"ord_off");
        ##     infostring <<- paste(calcinfo,ordinfo,"choice",choice);
        ## })

        return(my.df)
    }

    run1.df <- append.fitcols(simexp.df,fit1)
    run2.df <- append.fitcols(simexp.df,fit2)

    mostcommonvalue <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }



    ##interesting cases are: one consistent, one inconsistent, or both consistently choosing different items
    ##Boring cases are: both inconsistent, both consistent on same item.
    run1.df$interesting <- TRUE #start optimistic & eliminate boring cases.

    run1.df$consistency <- run1.df$choice==run2.df$choice;
    
    for(atrial in run1.df$trial_id){
        ccheck <- run1.df[run1.df$trial_id==atrial,"consistency"]
        if(sum(ccheck)==0){#nothing consistent
            run1.df[run1.df$trial_id==atrial,"interesting"] <- FALSE
        }
        if(sum(ccheck)==length(ccheck)){#all ppnts consistent
            choicelist <- run1.df[run1.df$trial_id==atrial,"choice"]
            if(mean(choicelist)==max(choicelist)) {
                run1.df[run1.df$trial_id==atrial,"interesting"] <- FALSE #total agreement, all choices are the same.
            }
        }
    }

        ret <- filter(run1.df,interesting==TRUE)%>%
        group_by(trial_id,probA,probB,probC,payoffA,payoffB,payoffC,payoffprior_mean,payoffprior_sd)%>% #sad this removes why trial was interesting, doing it anyway to reduce to unique trial ids.
        summarize()%>%
        ungroup()%>%
        as.data.frame()

    return(ret)
}

#singlepass.df <- agreement.filter(simexp.df)
#doublepass.df <- agreement.filter(bulk.assignStim(singlepass.df)) #is there anything left at this stage?
endtime=Sys.time()

#save.image(file="splitterstim.RData")
