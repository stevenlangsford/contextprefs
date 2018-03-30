library('rstan')
library('shinystan')
rm(list=ls())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


##get data
data.df <- read.csv("simexp_sanschoices.csv")
data.df$ppntid <- data.df$ppntid+1 #0-index was just for webppl.
datalist <- list(hm_trials=nrow(data.df),
                 hm_ppnts=max(data.df$ppntid),
                 A1=data.df$attributeA_option1,
                 B1=data.df$attributeB_option1,
                 A2=data.df$attributeA_option2,
                 B2=data.df$attributeB_option2,
                 A3=data.df$attributeA_option3,
                 B3=data.df$attributeB_option3,
                 ppntid=data.df$ppntid,
                 choice=data.df$oraclechoice #later, noisy choices?
                 )

fit <- stan(file="recoverAgent.stan",
            data=datalist,
            iter=1000,
            chains=4
            );

launch_shinystan(fit)

#stan_trace(fit) #obligatory mixing check.

#mysamples <- as.data.frame(extract(fit, permuted = TRUE)) # extract returns a list of arrays
