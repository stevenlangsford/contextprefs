library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
rm(list=ls())

simexp.df <- read.csv("simexp.csv")
agent_vs_random <- function(){
    ggplot(simexp.df)+geom_density(aes(x=agent_loss),color="red")+geom_density(aes(x=randomChoice_loss),color="blue")+theme_bw()
}

single_trial <- function(i=NULL){
    if(is.null(i))stop("specify a row i")
    arow <- simexp.df[i,]
    vis.df <- with(arow,data.frame(
                            option=c("A","B","C"),
                            prob=c(probA,probB,probC),
                            payoff=c(payoffA,payoffB,payoffC),
                            chosen=c(choice==1,choice==2,choice==3),
                            exVal=c(exValA,exValB,exValC),
                            best=c(exValA==best_available,exValB==best_available,exValC==best_available)
                        ))

    ggplot(vis.df,aes(x=prob,y=payoff,color=chosen,shape=best))+geom_point(data=vis.df,aes(size=exVal))+theme_bw()
}

bunch_o_trials <- function(n=15){
    for(i in base::sample(1:nrow(simexp.df),n)){
        x11();
        print(single_trial(i));
    }
    
}
