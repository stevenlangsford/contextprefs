library(tidyverse)
library(rwebppl)
rm(list=ls())

search.fit <- webppl(program_file="stimsearch.ppl")


mysamples.df <- data.frame();

for( i in 1:nrow(search.fit)){
    colnames <- c("A_opt1","A_opt2","A_opt3","B_opt1","B_opt2","B_opt3")
    for(k in 1:6)mysamples.df[i,colnames[k]] <- search.fit[[1]][[i]][k]
    mysamples.df[i,"score"] <- search.fit[[2]][[i]]
}

distinct.df <- mysamples.df%>%signif(digits=2)%>%distinct%>%arrange(desc(score))



##kmeans scree-plot:

## wss <- (nrow(distinct.df)-1)*sum(apply(distinct.df,2,var))
## for (i in 2:(nrow(distinct.df)-1)) wss[i] <- sum(kmeans(distinct.df, centers=i)$withinss)
## plot(1:(nrow(distinct.df)-1), wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
## abline(v=30);

##Guessing the elbow is around 30 here. Which is a nice enough number of stim to get anyway.
eyeball.elbow <- 30

save.image(file="simsearchran.RData")


fit <- kmeans(distinct.df, eyeball.elbow)
# append cluster assignment
distinct.df <- data.frame(distinct.df, fit$cluster)

superstim.df <- data.frame()
for(i in 1:max(fit$cluster)){
    superstim.df <- rbind(superstim.df,
                          distinct.df%>%
                          filter(fit.cluster==i)%>%
                          arrange(desc(score))%>%filter(row_number()==1)#best in class
                          )
}
superstim.df <- superstim.df%>%select(-score,-fit.cluster)

write.csv(superstim.df,file="AB_lm_vs_howes_30stim.csv")
