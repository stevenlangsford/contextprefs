library(tidyverse)
library(rwebppl)
rm(list=ls())

##Setup params
set.seed(4);
hm_ppnts = 2;

                                        #starttime=Sys.time()
##Agent population params
                                        #ppnt.calcsd.dist <- function(){
                                        #   10 #todo: draw from a population distribution, eg abs(rnorm(1,10,5)) or something.
                                        #}

if(!(any(list.files()=="gotchoices.csv"))){ #load choices if you got 'em

    ppnt.tolerance_prob.dist <- function(){
        0.11
    }
    ppnt.tolerance_payoff.dist <- function(){
        1.1
    }
    ppnt.orderror.dist <- function(){
        .1
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


    simexp.df <- bulk.assignStim(read.csv("splitterstim.csv"))

    fit <- webppl(program_file="howes16.ppl",data=simexp.df,data_var="expdf")
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
    simexp.df <- append.fitcols(simexp.df,fit)
    write.csv(simexp.df,file="gotchoices.csv",row.names=FALSE);
}#end load choices from file if you got 'em

simexp.df <- read.csv(file="gotchoices.csv") #sim exp with choices attached.

agreementCheck <- simexp.df%>%group_by(trial_id)%>%summarize(agreement=mean(choice)==max(choice))%>%ungroup()

splittrials <- filter(agreementCheck,agreement==FALSE)$trial_id
agreetrials <- filter(agreementCheck,agreement==TRUE)$trial_id

use.these.trials <- simexp.df #%>%filter(trial_id%in%c(splittrials))
                                        #,base::sample(agreetrials,length(splittrials),replace=FALSE)))


starttime=Sys.time()
recovery.fit <- as.data.frame(webppl(program_file="recover.ppl",data=use.these.trials,data_var="expdf")) #these.trials[1:i,]
endtime=Sys.time()
runtime = endtime-starttime;

fit.samples <- data.frame(ppnt_id=c(),sample=c())
for(ppnt in 1:hm_ppnts){
    fit.samples <- rbind(fit.samples,
                         data.frame(ppnt_id=ppnt,sample=as.numeric(map(recovery.fit[[1]],function(x){x[ppnt]})))
                         )
}

write.csv(fit.samples,file="fitsamples.csv")
write.csv(data.frame(start=starttime,complete=endtime,difference=runtime), file="runtime.csv",row.names=FALSE)

ggsave(ggplot(fit.samples,aes(x=sample))+geom_histogram()+facet_wrap(~ppnt_id)+theme_bw(),file="recovery.png")
