library(jsonlite)
library(tidyverse)

choiceVis <- function(fileprefix,titleinfo){
A <- fromJSON(paste0(fileprefix,"_Adistribution.json"))
B <- fromJSON(paste0(fileprefix,"_Bdistribution.json"))
C <- fromJSON(paste0(fileprefix,"_Cdistribution.json"))

alltogether <- data.frame(sample = c(A,B,C),option=c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C))))

return(ggplot(alltogether,aes(x=sample,group=option,color=option))+geom_density()+theme_bw()+ggtitle(titleinfo))

}
