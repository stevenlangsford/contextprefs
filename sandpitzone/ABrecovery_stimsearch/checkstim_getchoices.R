library(tidyverse)
library(rwebppl)
rm(list=ls())

hm_ppnts <- 2

getcalcsd <- function(){
    return( abs(rnorm(1,1,2.5))+1)
}

ppnt_Aweight <- rnorm(hm_ppnts,0,1)
ppnt_Bweight <- rnorm(hm_ppnts,0,1)
ppnt_calcsd <-  replicate(hm_ppnts,getcalcsd())
ppnt_toleranceA <- rep(3,hm_ppnts)
ppnt_toleranceB <- rep(3,hm_ppnts)
ppnt_orderr <- rep(.1,hm_ppnts)

assignStim <- function(arow){
    thisrow <- data.frame()
    
    for(ppnt in 1:hm_ppnts){
        thisrow <- rbind(thisrow,cbind(arow,data.frame(ppntid=ppnt-1,
                                                  Aweight=ppnt_Aweight[ppnt],
                                                  Bweight=ppnt_Bweight[ppnt],
                                                  toleranceA=ppnt_toleranceA[ppnt],
                                                  toleranceB=ppnt_toleranceB[ppnt],
                                                  calcsd=ppnt_calcsd[ppnt],
                                                  orderr=ppnt_orderr[ppnt],
                                                  value1=ppnt_Aweight[ppnt]*arow[,"attributeA_option1"]+ppnt_Bweight[ppnt]*arow[,"attributeB_option1"],
                                                  value2=ppnt_Aweight[ppnt]*arow[,"attributeA_option2"]+ppnt_Bweight[ppnt]*arow[,"attributeB_option2"],
                                                  value3=ppnt_Aweight[ppnt]*arow[,"attributeA_option3"]+ppnt_Bweight[ppnt]*arow[,"attributeB_option3"]
                                                  )))
    }
    return(thisrow);
}
bulkAssignStim <- function(my.df){
    exp.df <- data.frame()
    for(arow in 1:nrow(my.df)){
        exp.df <- rbind(exp.df,assignStim(my.df[arow,]))
    }
    return(exp.df)
}
trials.df <- read.csv("AB_lm_vs_howes_30stim.csv")%>%select(-X)%>%mutate(trialid=1:n())%>%
    rename(attributeA_option1=A_opt1,attributeA_option2=A_opt2,attributeA_option3=A_opt3, #rename expected by ppl arow[name-a-property] access.
           attributeB_option1=B_opt1,attributeB_option2=B_opt2,attributeB_option3=B_opt3
           )

simexp.df <- bulkAssignStim(trials.df)

lm_simexp.df <- simexp.df
howes_simexp.df <- simexp.df

hm_reps <- 20
for(rep in 1:hm_reps){
    print(paste("howes",rep))
    howes_simexp.df[,paste0("choice",rep)] <- webppl(program_file="ABchoices.ppl",data=simexp.df,data_var="expdf")

    print(paste("lm",rep))
    
      lm_simexp.df[,paste0("choice",rep)] <- webppl(program_file="lm_getchoices.ppl",data=simexp.df,data_var="expdf")
    
}

addChoicestats <- function(mydf){
    for(arow in 1:nrow(mydf)){
    mydf[arow,"prop_opt1"] <- sum(mydf[arow,paste0("choice",1:hm_reps)]==1)/hm_reps
    mydf[arow,"prop_opt2"] <- sum(mydf[arow,paste0("choice",1:hm_reps)]==2)/hm_reps
    mydf[arow,"prop_opt3"] <- sum(mydf[arow,paste0("choice",1:hm_reps)]==3)/hm_reps
    }
    return(mydf);
}

lm_simexp.df <- addChoicestats(lm_simexp.df)
howes_simexp.df <- addChoicestats(howes_simexp.df)

save.image(file="getchoices_lmvshowes_ran.RData")
