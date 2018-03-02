library(tidyverse)
rm(list=ls())

load(file="getchoices_lmvshowes_ran.RData")

lm_simexp.df$decisionmaker <- "linear"
howes_simexp.df$decisionmaker <- "howes"

##TO DELETE## upstream script now fixed.
names(lm_simexp.df)[7] <- "trialid"
names(howes_simexp.df)[7] <- "trialid"
##end to delete

combo.df <- rbind(lm_simexp.df,howes_simexp.df)%>%select(trialid,ppntid,contains("prop_opt"))

##NOW WHAT? visualize the agreement: ppnts have different weights, but the same ppnt/weights appear in the two decision-maker flavors, which is the critical bit.
#Go wide with the ppnts: the agreement/disagreement probably depends on the weights? How to see this?


