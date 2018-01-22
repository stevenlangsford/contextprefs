library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

showme <- function(recovered.df){
    for(avar in c("calcsd","tolerance_prob","tolerance_payoff","orderror")){
        x11();
        eval(parse(text=
                       paste0("plot(recovered.df$",avar,"_simtruth,recovered.df$",avar,")")
                   ));
    }
}


targcsv <- read.csv("simdata_recovery/simexp2ppnts2trialsrandomstim_recovery.csv")

showme(targcsv);
