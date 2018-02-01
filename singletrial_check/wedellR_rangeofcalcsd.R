rm(list=ls())
library(tidyverse)
library(jsonlite) #name clash with purrr 'flatten'
library(rwebppl)
##agentparams: fix for each trial
ppnt_calcsd = 5
ppnt_tolerance_prob = .011
ppnt_tolerance_payoff = 1.1
ppnt_orderror =.15
starttime <- Sys.time()
##modelparams: passed in with each row, just to avoid having multiple model files.
useord = TRUE
usecalc = TRUE

##Some interesing specific trials:
identicaltriplet.trial <- data.frame(
    ppntid = 0,
    probA = .5,
    probB = .5,
    probC = .5,
    payoffA = 100,
    payoffB = 100,
    payoffC = 100,

    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,

    useord = useord, 
    usecalc = usecalc,
    payoffprior_mean= 100, #prior on probs is always beta(1,1) for now, but payoffs are pretty different between wedell and howes16 rnd-stim.
    payoffprior_sd= 10,
    trial_id="identicaltriplets"
)

rangetie.trial <- data.frame(
    ppntid = 0,
    probA = .8,
    probB = .8/2,
    probC = .8/4,
    payoffA = 70,
    payoffB = 70*2,
    payoffC = 70*4,
    
    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,
    
    useord = useord,
    usecalc = usecalc,
    payoffprior_mean= 100,
    payoffprior_sd= 10,
    trial_id="rangetie"
)


##Wedell text examples: Note DIFFERENT PRIORS ON PAYOFF
wedellR.trial <- data.frame(
    ppntid = 0,
    probA = .5,
    probB = .4,
    probC = .5,
    payoffA = 20,
    payoffB = 25,
    payoffC = 18,
    
    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,
    
    useord = useord,
    usecalc = usecalc,
    payoffprior_mean= 20,
    payoffprior_sd= 20,
    trial_id="wedellR"
)

wedellR_tinydecoy.trial <- data.frame(
    ppntid = 0,
    probA = .5,
    probB = .4,
    probC = .1,
    payoffA = 20,
    payoffB = 25,
    payoffC = 1,
    
    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,
    
    useord = useord,
    usecalc = usecalc,
    payoffprior_mean= 20,
    payoffprior_sd= 20,
    trial_id="wedellR_tinydecoy"
)

wedellF.trial <- data.frame(
    ppntid = 0,
    probA = .67,
    probB = .5,
    probC = .5,
    payoffA = 15,
    payoffB = 20,
    payoffC = 18,
    
    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,
    
    useord = useord,
    usecalc = usecalc,
    payoffprior_mean= 20,
    payoffprior_sd= 10,
    trial_id="wedellF"
)


wedellF_tinydecoy.trial <- data.frame(
    ppntid = 0,
    probA = .67,
    probB = .5,
    probC = .1,
    payoffA = 15,
    payoffB = 20,
    payoffC = 1,
    
    calc_sd = ppnt_calcsd,
    tolerance_prob =  ppnt_tolerance_prob,
    tolerance_payoff =  ppnt_tolerance_payoff,
    p_err = ppnt_orderror,
    
    useord = useord,
    usecalc = usecalc,
    payoffprior_mean= 20,
    payoffprior_sd= 10,
    trial_id="wedellF_tinydecoy"
)

wedellR_decoyeffect_rangeofcalcsd <- function(n.per.cell, calcsd.vector){
    dat <- data.frame()
    for(ansd in calcsd.vector){
#        browser();
        nextone <- rbind(as.data.frame(lapply(wedellR.trial, rep, n.per.cell)),
                         as.data.frame(lapply(wedellR_tinydecoy.trial, rep, n.per.cell))
                         )
        nextone$calc_sd <- ansd
        dat <- rbind(dat,nextone)
    }
    return(dat)
}

wedellF_decoyeffect_rangeofcalcsd <- function(n.per.cell, calcsd.vector){
    dat <- data.frame()
    for(ansd in calcsd.vector){
#        browser();
        nextone <- rbind(as.data.frame(lapply(wedellF.trial, rep, n.per.cell)),
                         as.data.frame(lapply(wedellF_tinydecoy.trial, rep, n.per.cell))
                         )
        nextone$calc_sd <- ansd
        dat <- rbind(dat,nextone)
    }
    return(dat)
}

##RUN STARTS HERE
#simexp.df <- wedellR_decoyeffect_rangeofcalcsd(250,c(.5,1,2,3,4,5))
simexp.df <- wedellF_decoyeffect_rangeofcalcsd(250,c(.5,1,2,3,4,5))

##Mess with trial properties:
##wide prior that sucker ##DONE IN TRIAL DEFS NOW N(20,20) paper uses t(df=100,19,8) this is important.
#simexp.df$payoffprior_sd <- 20
simexp.df$trial_id <- paste0(simexp.df$trial_id,"payoffpriorsd10");


simexp.df$row_id <- 1:nrow(simexp.df)
                   
fit <- webppl(program_file="howes16.ppl",data=simexp.df,data_var="expdf",packages="webppl-json")

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

with(simexp.df, {
calcinfo <<- ifelse(usecalc, paste("calc:",signif(calcA,3),signif(calcB,3),signif(calcC,3)), "calc_off");
ordinfo <<- ifelse(useord, paste("pAB",ABprob,"pAC",ACprob,"pBC",BCprob,"vAB",ABpayoff,"vAC",ACpayoff,"vBC",BCpayoff),"ord_off");
infostring <<- paste(calcinfo,ordinfo,"choice",choice);
})

simexp.df$choice <- as.factor(simexp.df$choice)

#output
prefs_sdrange.plot <- ggplot(simexp.df,aes(x=trial_id,group=choice,fill=choice))+geom_bar(position="dodge")+theme_bw()+facet_wrap(~calc_sd)
write.csv(simexp.df,file=paste0(simexp.df$trial_id[1],"_rangeofcalcsd.csv"),row.names=FALSE)
ggsave(prefs_sdrange.plot,file=paste0(simexp.df$trial_id[1],"_rangeofcalcsd.png"))
print(prefs_sdrange.plot)
endtime <- Sys.time()
