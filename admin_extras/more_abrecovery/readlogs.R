library(tidyverse)
library(jsonlite)
rm(list=ls())
denull <- function(x){if(is.null(x)){return("is.null");}else return(x)}

log.df <- data.frame()
for(afile in list.files("logs/")){
    log.df <- rbind(log.df,as.data.frame(map(fromJSON(paste0("logs/",afile)),denull)))
}
