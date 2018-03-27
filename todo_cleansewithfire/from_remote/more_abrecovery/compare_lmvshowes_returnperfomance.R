library(tidyverse)
rm(list=ls())
howeschoices.df <- read.csv("ABsimexp_withchoices.csv")
lmchoices.df <- read.csv("lmtest/ABsimexp_lmchoices_manyppnts.csv")

howeswinnings.df <- howeschoices.df%>%group_by(ppntid)%>%summarize(winnings=sum(c(value1,value2,value3)[choice]))
lmwinnings.df <- lmchoices.df%>%group_by(ppntid)%>%summarize(winnings=sum(c(value1,value2,value3)[choice]))

names(lmwinnings.df) <- paste0("lm",names(lmwinnings.df))
names(howeswinnings.df) <- paste0("howes",names(howeswinnings.df))

combo.df <- cbind(howeswinnings.df,lmwinnings.df)%>%mutate(difference=howeswinnings-lmwinnings)
summary(combo.df$difference) #mean -11, sd 40, these ain't different none.
