library(tidyverse)
library(rwebppl)
rm(list=ls())

exp.df <- read.csv("simexp.csv")#%>%filter(ppntid<10);
#exp.df$rowname <- 1:nrow(exp.df);

fit <- webppl(program_file="ABrecovery.ppl",data=exp.df,data_var="datadf",packages=c("webppl-json"));
save.image("recoveryran.RData")

View("done distagent")

