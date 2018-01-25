library(purrr)

wedell_ABoptions <- list(
                                        #options A and B on the roughly-equivalent line
    list(prob=.3,payoff=33),
    list(prob=.4,payoff=25),
    list(prob=.5,payoff=20),
    list(prob=.67,payoff=15),
    list(prob=.83,payoff=12)
)

wedell_decoys <- list(
                                        #R|F decoy options (status depends on A/B pair
    list(prob=.4,payoff=20),
    list(prob=.5,payoff=18),
    list(prob=.67,payoff=13),
    list(prob=.83,payoff=10),
    list(prob=.25,payoff=33),
    list(prob=.35,payoff=25),
    list(prob=.45,payoff=20),
    list(prob=.62,payoff=15),
    list(prob=.78,payoff=12),
    list(prob=.3,payoff=30),
                                        #R&F decoys
    list(prob=.35,payoff=20),
    list(prob=.45,payoff=18),
    list(prob=.62,payoff=13),
    list(prob=.78,payoff=10),
    list(prob=.25,payoff=30),
    list(prob=.35,payoff=20),
    list(prob=.45,payoff=18),
    list(prob=.62,payoff=13)
)

                                        #wedell actually rings the changes instead of choosing randomly, so these are only vaguely related to the paper stim.
getWedelltrial <- function(){
    AB <- sample(wedell_ABoptions,2,replace=FALSE) #no repeats
    decoy <- sample(wedell_decoys,1)
    return(list(
        A=AB[[1]],
        B=AB[[2]],
        C=decoy[[1]]
    ))
}

getRandomtrial <- function(){
    getRandomOption <- function(){
        return(
            list(
                prob=rbeta(1,1,1),
                payoff=rnorm(1,100,5) #range used in Howes16
                                        #payoff=runif(1,10,40)#Similar range to Wedell payoffs, so don't have to change model priors switching between Wedell-style stim and these.
            )
        )
    }
    return(list(A=getRandomOption(),B=getRandomOption(), C=getRandomOption()))
}

getStim_twomatrixformat <- function(hm_stim,trial_generator=getWedelltrial){
    stim <- map(1:hm_stim,function(x){trial_generator()})
    probs <- map(stim,function(trial){as.numeric(map(trial,function(option){option$prob}))})
    probs <- reduce(probs,rbind, init=NULL); dimnames(probs)=NULL;
    payoffs <- map(stim,function(trial){as.numeric(map(trial,function(option){option$payoff}))})
    payoffs <- reduce(payoffs,rbind, init=NULL); dimnames(payoffs)=NULL;

    return(list(probs,payoffs));
}


wedellTextExample_twomatrixformat<- function(){
    ## For example, when (.50, $20) was paired with (.40, $25), the R A
    ## decoy was (.50, $18), extending the range downward on Dimension
    ## I. When (.50, $20) was paired with (.67, $ 15), it became the B target;
    ## the same decoy gamble (.50, $18) was now an F B decoy, serving to
    ## increase the proportion of stimuli lying below target B
    stim <- list(
        ##R example
        list(
            list(prob=.5,payoff=20),#A
            list(prob=.4,payoff=25),#B
            list(prob=.5,payoff=18)# R decoy
        ),
        ##R example sansdecoy
        list(
            list(prob=.5,payoff=20),#A
            list(prob=.4,payoff=25),#B
            list(prob=.1,payoff=1)#Decoy: inconsequential
        ),
        ##F  decoy
        list(
            list(prob=.67,payoff=15),#A
            list(prob=.5,payoff=20), #B
            list(prob=.5,payoff=18) #F decoy
        ),
        ##F sans decoy
        list(
            list(prob=.67,payoff=15),#A
            list(prob=.5,payoff=20), #B
            list(prob=.1,payoff=1) #Decoy: inconsequential
        )
    )
    probs <- map(stim,function(trial){as.numeric(map(trial,function(option){option$prob}))})
    probs <- reduce(probs,rbind, init=NULL); dimnames(probs)=NULL;
    payoffs <- map(stim,function(trial){as.numeric(map(trial,function(option){option$payoff}))})
    payoffs <- reduce(payoffs,rbind, init=NULL); dimnames(payoffs)=NULL;

    return(list(probs,payoffs));
}
