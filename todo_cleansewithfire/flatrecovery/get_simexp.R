#This sets up a simulated experiment, passing a populated dataframe to ABchoices.ppl to get simulated responses, and saving the results as simexp.csv
library(tidyverse)
library(rwebppl)
rm(list=ls())

##world params
getA <- function(){
   return(runif(1,0,10)); #Turn this back into a distribution later, but for now, match the recovery model exactly. (You're in ABratio land)
}

getB <- function(){
    return(runif(1,0,10))#Goodness of B is expressed as some scaling factor of goodness of A. Both features are assumed to be positive for now?
}
getcalcsd <- function(){
    return(.5); ##? reasonable?
}
##exp params
hm_ppnts <- 10
hm_trials <- 35

##set up the stimuli:
trials.df <- data.frame(attributeA_option1=c(),
                        attributeA_option2=c(),
                        attributeA_option3=c(),
                        attributeB_option1=c(),
                        attributeB_option2=c(),
                        attributeB_option3=c())

for(atrial in 1:hm_trials){
    trials.df <- rbind(trials.df,
                       data.frame(attributeA_option1=getA(),
                                  attributeA_option2=getA(),
                                  attributeA_option3=getA(),
                                  attributeB_option1=getB(),
                                  attributeB_option2=getB(),
                                  attributeB_option3=getB()
                                  ))
}

##set up the participants
set.seed(4)
ppnt_Aweight <- rep(1,hm_ppnts);
ppnt_Bweight <- runif(hm_ppnts,0,5)
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
simexp.df <- bulkAssignStim(trials.df)
simexp.df$rowname <- 1:nrow(simexp.df) #not used for anything except a progress-marker console.log
##webppl(program_file="memrecovery.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))
simexp.df$choice <- webppl(program_file="ABchoices.ppl",data=simexp.df,data_var="datadf",packages=c("webppl-json"))

for(arow in 1:nrow(simexp.df)){
    simexp.df[arow,"oraclechoice"]=which(simexp.df[arow,c("value1","value2","value3")]==max(simexp.df[arow,c("value1","value2","value3")]))
}


write.csv(simexp.df,file="simexp.csv",row.names=FALSE)
save.image(file="getsimexpran.RData")
#View("done")
