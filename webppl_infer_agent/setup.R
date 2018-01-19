rm(list=ls())
library(ggplot2)
library(dplyr)
library(rwebppl)

##source("mailstuff.R")

exp.df <- read.csv("simexp.csv")
exp.df$ppntid <- exp.df$ppntid-1; #webppl is 0 indexed.

exp.df$expectA = exp.df$probA*exp.df$payoffA
exp.df$expectB = exp.df$probB*exp.df$payoffB
exp.df$expectC = exp.df$probC*exp.df$payoffC

##data frames arrive in webppl as an array of objects, each object is a row, with attributes colnames holding the values in that row.

time.df <- data.frame(rows=c(),fittime=c())
for(i in 1:nrow(exp.df)){
    timer <- system.time(
        fit  <- webppl(program_file="model.ppl",data=exp.df[1:i,],data_var="expdf")
    )
    if(is.null(fit))break;
    time.df <- rbind(time.df,data.frame(rows=i,fittime=timer[["elapsed"]]))
}

write.csv(time.df,file="rowtimes.csv",row.names=FALSE)
