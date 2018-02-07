library(tidyverse)
fit.samples <- read.csv(file="fitsamples.csv")
print(ggplot(fit.samples,aes(x=sample))+geom_histogram()+facet_wrap(~ppnt_id)+theme_bw())
