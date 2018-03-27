library(tidyverse)
library(rwebppl)
rm(list=ls())
set.seed(4)

hm_trials <- 2
simexp.df <- data.frame(probA=rbeta(hm_trials,1,1),
                        payoffA=rnorm(hm_trials,100,20),
                        probB=rbeta(hm_trials,1,1),
                        payoffB=rnorm(hm_trials,100,20),
                        probC=rbeta(hm_trials,1,1),
                        payoffC=rnorm(hm_trials,100,20)
                        )

myfit <- webppl(program_file="multisamples.ppl", data=simexp.df, data_var="datadf")

