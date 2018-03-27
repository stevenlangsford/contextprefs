library(tidyverse)
diag.df <- read.csv("diag.csv")
ggplot(diag.df%>%group_by(rowval)%>%summarize(appraisedas=mean(appraisedas))%>%ungroup(),aes(x=rowval,y=appraisedas))+geom_point()+theme_bw()
