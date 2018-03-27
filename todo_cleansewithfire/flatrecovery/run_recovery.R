library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv("simexp.csv")
                                        #%>%filter(ppntid<10);

fit <- webppl(program_file="ABrecovery.ppl",data=simexp.df,data_var="datadf",packages=c("webppl-json"));
save.image("recoveryran.RData")

View("done")
