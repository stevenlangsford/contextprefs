rm(list=ls())
source("create_sim/setup.R")
source("infer_agent/setup.R")

timeinfo.df <- data.frame(hm_ppnts=c(),hm_trials=c(),stimtype=c(),simtime=c(),recovertime=c())

for(hm_ppnts in seq(from=20,to=80,by=20)){
    for(hm_trials in seq(from=20,to=40,by=10)){
        for(stimtype in c("random","wedellish")){
            
            sim.time <- system.time(
                write_simexp(hm_ppnts=hm_ppnts ,hm_trials=hm_trials,stimtype=stimtype,savepath="./simdata_store/",runpath="create_sim/",saveplots=FALSE)
            )
            recover.time <- system.time(
                write_agent_recovery(simexpfolder="simdata_store/",simexpcsvname=paste0("simexp",hm_ppnts,"ppnts",hm_trials,"trials",stimtype,"stim.csv"),modelpath="infer_agent/",outputpath="./simdata_recovery/")
            )

            timeinfo.df <- rbind(timeinfo.df,data.frame(hm_ppnts=hm_ppnts,hm_trials=hm_trials,stimtype=stimtype,simtime=sim.time[["elapsed"]],recovertime=recover.time[["elapsed"]]))
        }
    }
}

write.csv(timeinfo.df,file="timeinfo.csv",row.names=FALSE)
