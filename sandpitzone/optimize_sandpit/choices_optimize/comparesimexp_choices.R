library(tidyverse)
library(rwebppl)
rm(list=ls())

simexp.df <- read.csv("simexp.csv")%>%filter(ppntid<=20) #should be plenty?

simexp.df$optchoice <- webppl(program_file="ABchoices.ppl",data=simexp.df,data_var="expdf",packages=c("webppl-json"))

save.image(file="addedopttooldsimexp.RData")

with(simexp.df,table(choice,optchoice,oraclechoice))
with(simexp.df,sum(optchoice==choice)/nrow(simexp.df)) #opt and MCMC agree: 0.68
with(simexp.df,sum(optchoice==oraclechoice)/nrow(simexp.df)) #opt and oracle agree: 0.81
with(simexp.df,sum(choice==oraclechoice)/nrow(simexp.df)) #MCMC and oracle agree: 0.73
#Conclusion: opt is closer to oracle than MCMC, but isn't just an oracle, there's still variability. so... now what?
#***Does this show context effects?***
