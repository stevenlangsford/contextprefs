library(tidyverse)

stim.df <- read.csv("hilocalcsd_diagstim.csv")

## > with(stim.df,mean(c(payoffA,payoffB,payoffC)))
## [1] 19.15556
## > with(stim.df,sd(c(payoffA,payoffB,payoffC)))
## [1] 6.266718
##payoff hist is closer to uniform(10,30) than normal, mean 20 sd 8 for payoff priors with these stim?
