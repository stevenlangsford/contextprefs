library(tidyverse)
simexp.df <- read.csv("simexp.csv")

summary.df <- simexp.df%>%group_by(ppntid,calcsd)%>%summarize(agreement=sum(choice==oraclechoice)/n())%>%ungroup()

summary.df
