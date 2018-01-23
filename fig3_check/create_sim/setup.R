library(ggplot2)
library(dplyr)
library(tidyr)
library(rwebppl)

##Loop calling this with various setup args follows...

write_simexp_fixedagent <- function(modelname,hm_ppnts,hm_trials,calcsd,tolerance_prob,tolerance_payoff,p_err,stimtype=c("random","wedellish")[1],savepath="../",saveplots=FALSE,extraid=""){

    ##added to file name of any saved output.
    simidstring <- gsub("\\.","pt",paste0(extraid,modelname,"_calcsd",calcsd,"_tprob",tolerance_prob,"_tpay",tolerance_payoff,"_orderr",p_err,"_trials",hm_trials,"_stim",stimtype))
    print(simidstring);#diag, cheap progress bar
    ##    browser();
    
    ##hm__ppnts is not totally moot: they all have the same params set here, but the stim presented repeat for each participant.
    ##population params
    calc_sd = rep(calcsd,hm_ppnts) #abs(rnorm(hm_ppnts,3,.5))
    tolerance_prob = rep(tolerance_prob,hm_ppnts) #rbeta(hm_ppnts,1,20)+.01
    tolerance_payoff = rep(tolerance_payoff,hm_ppnts)#abs(rnorm(hm_ppnts,1,1))+1
    p_err = rep(p_err,hm_ppnts);

    set.seed(4) #should fix the stimuli generated for each call (with the same number of total stimuli). Check this is true by returning the max expected.
    source(paste0("wedellExp1Stim.R"))
    if(stimtype=="random"){
        mystim <- getStim_twomatrixformat(hm_trials,getRandomtrial)
    }else if(stimtype=="wedellish"){
        mystim <- getStim_twomatrixformat(hm_trials,getWedelltrial)
    }else{
        stop(paste("Did not recognize stimtype ",stimtype))
    }
    
    prob <- mystim[[1]]
    payoff <- mystim[[2]]

    simexp.df <- data.frame(ppntid=c(),calc_sd=c(),tolerance_prob=c(), tolerance_payoff=c(),p_err=c(),trialid=c(),probA=c(),probB=c(),probC=c(),payoffA=c(),payoffB=c(),payoffC=c()); #via df rather than directly to datalist for convenient inspection.

    impressionrow <- 1;
    for(ppnt in 1:hm_ppnts){
        for(trial in 1:hm_trials){
            simexp.df[impressionrow,"ppntid"] <- ppnt-1;#puts ids on webppls's 0 indexed scale. Remember to undo this at the other end.
            simexp.df[impressionrow,"calc_sd"] <- calc_sd[ppnt];
            simexp.df[impressionrow,"tolerance_prob"] <- tolerance_prob[ppnt];
            simexp.df[impressionrow,"tolerance_payoff"] <- tolerance_payoff[ppnt];
            simexp.df[impressionrow,"p_err"] <- p_err[ppnt];
            simexp.df[impressionrow,"trialid"] <- trial;
            simexp.df[impressionrow,"probA"] <- prob[trial,1]
            simexp.df[impressionrow,"probB"] <- prob[trial,2]
            simexp.df[impressionrow,"probC"] <- prob[trial,3]
            simexp.df[impressionrow,"payoffA"] <- payoff[trial,1]
            simexp.df[impressionrow,"payoffB"]<- payoff[trial,2]
            simexp.df[impressionrow,"payoffC"]<- payoff[trial,3]
            
            impressionrow <- impressionrow+1;#lazy rowfinder.
        }
    }

    ##add simulated choices
    timer <- system.time(
                                        #simexp.df$choice
        agentstuff <- webppl(program_file=paste0(modelname),data=simexp.df,data_var="expdf")
    )
    ##you could just add the choices (agentstuff[[8]]), 
    simexp.df$choice <- agentstuff[[8]] ##DONE

    ##all the other stuff is just here to inspect the model's working-out.
    plot.dir=paste0(savepath,"plots/")

    calcobs <- agentstuff[[1]]
    simexp.df$calcA <- calcobs[,1]
    simexp.df$calcB <- calcobs[,2]
    simexp.df$calcC <- calcobs[,3]

    simexp.df$ABprob <- agentstuff[[2]]
    simexp.df$ACprob <- agentstuff[[3]]
    simexp.df$BCprob <- agentstuff[[4]]

    simexp.df$ABpayoff <- agentstuff[[5]]
    simexp.df$ACpayoff <- agentstuff[[6]]
    simexp.df$BCpayoff <- agentstuff[[7]]


    ##expected values for reference
    simexp.df$exValA <- simexp.df$probA*simexp.df$payoffA
    simexp.df$exValB <- simexp.df$probB*simexp.df$payoffB
    simexp.df$exValC <- simexp.df$probC*simexp.df$payoffC

    ##check the calc observation looks like you'd expect:
    calcvstruth.plot <- ggplot(simexp.df,aes(x=exValA,y=calcA))+geom_point()+
        xlab("Option A value in expectation")+
        ylab("Agent's calc observation")+
        theme_bw()
    if(saveplots)ggsave(calcvstruth.plot,file=paste0(plot.dir,paste0("calcvstruth",simidstring,".png")));

    ##check the ord observation looks like you'd expect:
    check_ordobs <- function(a,b,tolerance,ordobs){
        if(abs(a-b)<tolerance){
            true_relation="="
        }else if (a>b){
            true_relation=">"
        }else {
            true_relation="<"
        }
        return(ordobs==true_relation);
    }

    ordobs_errorrate_check.df <- data.frame(
        ABprob= sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(probA,probB,tolerance_prob,ABprob))
        })))/nrow(simexp.df),
        ACprob = sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(probA,probC,tolerance_prob,ACprob))
        })))/nrow(simexp.df),
        BCprob= sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(probB,probC,tolerance_prob,BCprob))
        })))/nrow(simexp.df),
        ABpayoff=sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(payoffA,payoffB,tolerance_payoff,ABpayoff))
        })))/nrow(simexp.df),
        ACpayoff=sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(payoffA,payoffC,tolerance_payoff,ACpayoff))
        })))/nrow(simexp.df),
        BCpayoff=sum(as.logical(map(1:nrow(simexp.df),function(i){
            with(simexp.df[i,],check_ordobs(payoffB,payoffC,tolerance_payoff,BCpayoff))
        })))/nrow(simexp.df)
    )

    errate_plotable.df <- gather(ordobs_errorrate_check.df,comparison,err.rate)

    ord_error.plot <- ggplot(errate_plotable.df,aes(x=comparison,group=comparison,y=err.rate))+
        geom_bar(stat="identity")+
        coord_cartesian(ylim=c(.95,1))+
        geom_hline(aes(yintercept = 1-mean(p_err)+(1/3)*mean(p_err),color="expected"))+
        xlab("Comparison")+
        ylab("Proportion correct")
    if(saveplots)ggsave(ord_error.plot,file=paste0(plot.dir,paste0("ordrelation_errors",simidstring,".png")));

    ##expected value of chosen option
    for(i in 1:nrow(simexp.df)){
        simexp.df[i,"val_chosen"] <- simexp.df[i,c("exValA","exValB","exValC")][,simexp.df[i,"choice"]];
        simexp.df[i,"best_available"] <- simexp.df[i,c("exValA","exValB","exValC")][,which(simexp.df[i,c("exValA","exValB","exValC")]==max(simexp.df[i,c("exValA","exValB","exValC")]))[1]]
        simexp.df[i,"agent_loss"] <- simexp.df[i,"val_chosen"]-simexp.df[i,"best_available"]
        simexp.df[i,"val_randomchoice"] <- simexp.df[i,c("exValA","exValB","exValC")][,base::sample(1:3,1)];
        simexp.df[i,"randomChoice_loss"] <-simexp.df[i,"val_randomchoice"]-simexp.df[i,"best_available"]  
    }


    performance.plot <- ggplot(simexp.df)+geom_density(aes(x=randomChoice_loss,color="guesser"))+
        geom_density(aes(x=agent_loss,color="agent"))+
        xlab("Chosen - best_available (in expectation)")+
        theme_bw()
    if(saveplots)ggsave(performance.plot,file=paste0(plot.dir,paste0("agentvsguesser",simidstring,".png")));


    write.csv(simexp.df,file=paste0(savepath,"simexp",simidstring,".csv"),row.names=FALSE)
    return(
        list(
            oracle=sum(simexp.df[,"best_available"]),#expected value of choosing the best option every time. Constant across runs with the same #stim since seed set. (check)
            agent=sum(simexp.df[,"val_chosen"]) #what the agent could expect to get out of its choices.
        )
    )
}


##Do the thing
varycalcsd.df <- data.frame(
    model=c(),
    hm_ppnts=c(),
    hm_trials=c(),
    calcsd=c(),
    tolerance_prob=c(),
    tolerance_payoff=c(),
    p_err=c(),
    stimtype=c(),
    maxperformance=c(),
    agentperformance=c()
)

varyorderr.df <- data.frame(
    model=c(),
    hm_ppnts=c(),
    hm_trials=c(),
    calcsd=c(),
    tolerance_prob=c(),
    tolerance_payoff=c(),
    p_err=c(),
    stimtype=c(),
    maxperformance=c(),
    agentperformance=c()
)

##Any global settings
hm_ppnts=1; #ppnt params are fixed, but stimuli repeat per participant, so this means 'draw fresh stimuli for every observation'
hm_trials=1000 #10,000000 in Howes16

tolerance_prob = .011
tolerance_payoff = 1.1
stimtype = "random" #Must match an option in wedellExp1Stim.R, currently accepts "random" or "wedellish"
modellist = c("howes16_full.ppl","ordonly.ppl","calconly.ppl") #soon to include single-obs versions.

##left panel: vary calcsd
p_err=.01 #fixed locally for this loop
for(model in modellist){
    for(calcsd in (seq(from=0,to=50,by=5)+.01)){#Ah. Can't deal with starting at 0, sigma of 0 in a distribution breaks webppl.
        my_performance <- write_simexp_fixedagent(
            model=model,
            hm_ppnts=hm_ppnts, 
            hm_trials=hm_trials,
            calcsd=calcsd,
            tolerance_prob=tolerance_prob,
            tolerance_payoff=tolerance_payoff,
            p_err=p_err,
            stimtype=stimtype,
            savepath="output/",
            saveplots=TRUE,
            extraid="varycalcsd_"
        )
        varycalcsd.df <- rbind(varycalcsd.df,data.frame(
                                                   model=model,
                                                   hm_ppnts=hm_ppnts, 
                                                   hm_trials=hm_trials,
                                                   calcsd=calcsd,
                                                   tolerance_prob=tolerance_prob,
                                                   tolerance_payoff=tolerance_payoff,
                                                   p_err=p_err,
                                                   stimtype=stimtype,
                                                   maxperformance=my_performance[["oracle"]],
                                                   agentperformance=my_performance[["agent"]]
                                               )
                                )

        
    }
}

##Right panel: vary orderr
calcsd = 30;
for(model in c(modellist)){
    for(p_err in seq(from=0,to=1,by=.1)){
        my_performance <- write_simexp_fixedagent(
            model=model,
            hm_ppnts=hm_ppnts, 
            hm_trials=hm_trials,
            calcsd=calcsd,
            tolerance_prob=tolerance_prob,
            tolerance_payoff=tolerance_payoff,
            p_err=p_err,
            stimtype=stimtype,
            savepath="output/",
            saveplots=TRUE,
            extraid="varyorderr_"
        )
        varyorderr.df <- rbind(varyorderr.df,data.frame(
                                                   model=model,
                                                   hm_ppnts=hm_ppnts, 
                                                   hm_trials=hm_trials,
                                                   calcsd=calcsd,
                                                   tolerance_prob=tolerance_prob,
                                                   tolerance_payoff=tolerance_payoff,
                                                   p_err=p_err,
                                                   stimtype=stimtype,
                                                   maxperformance=my_performance[["oracle"]],
                                                   agentperformance=my_performance[["agent"]]
                                               )
                                )

        
    }
}

write.csv(varycalcsd.df,file="output/performance_summary/varycalcsd.csv",row.names=FALSE)
write.csv(varyorderr.df,file="output/performance_summary/varyorderr.csv",row.names=FALSE)

varycalcsd.plot <- ggplot(varycalcsd.df,aes(x=calcsd,y=agentperformance,color=model))+geom_line()+geom_point()+geom_hline(aes(yintercept=maxperformance))+theme_bw()
varyorderr.plot <- ggplot(varyorderr.df,aes(x=p_err,y=agentperformance,color=model))+geom_line()+geom_point()+geom_hline(aes(yintercept=maxperformance))+theme_bw()

ggsave(varycalcsd.plot,file="varycalcsd_performance.png")
ggsave(varyorderr.plot,file="varyorderr_performance.png")

print(varycalcsd.plot); x11(); print(varyorderr.plot); #popup when done.
