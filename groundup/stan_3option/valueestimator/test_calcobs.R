library('tidyverse')
library('rstan')
library('shinystan')
library('patchwork')
rm(list=ls())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#read simexp
simexp.df <- read.csv("simexp_sanschoices.csv")
simexp.df$ppntid <- simexp.df$ppntid+1 #0-indexing was just for webppl.

#convert to stand friendly data input (ppnt properties in vectors hm_ppnts long)
ppnt_ks <- sapply(unique(simexp.df$ppntid),function(x){simexp.df%>%filter(ppntid==x)%>%select(Bweight)%>%filter(row_number()==1)%>%as.numeric})

ppnt_calcsd <- sapply(unique(simexp.df$ppntid),function(x){simexp.df%>%filter(ppntid==x)%>%select(calcsd)%>%filter(row_number()==1)%>%as.numeric})

datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=max(simexp.df$ppntid),
                ppntid=simexp.df$ppntid,

                trueA1=simexp.df$attributeA_option1,
                trueB1=simexp.df$attributeB_option1,
                trueA2=simexp.df$attributeA_option2,
                trueB2=simexp.df$attributeB_option2,
                trueA3=simexp.df$attributeA_option3,
                trueB3=simexp.df$attributeB_option3,
                
                k=ppnt_ks,
                calcsd=ppnt_calcsd)

#fit: sensible things to recover are A, B, and estval.
fit <- stan(file="calcobs.stan",
           data=datalist,
           iter=1000,
           chains=4);

#launch_shinystan(fit);

mysamples <- as.data.frame(extract(fit, permuted = TRUE)) # extract returns a list of arrays

estvals <- mysamples%>%select(contains("estval1"))

##sanity check:
##meanest <- apply(estvals,2,mean)
##plot(simexp.df$value3, meanest)
