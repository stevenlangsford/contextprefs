## compare.df <- with(impressions.df, data.frame(expected=c(expectedA,expectedB,expectedC),estimated=c(estValue_1,estValue_2,estValue_3),calcobs=c(calcA,calcB,calcC)))
rm(list=ls())
## ggplot(compare.df,aes(x=estimated, y=calcobs))+geom_point()+theme_bw()

p <- seq(from=.1,to=.9,by=.01)

a <- 5*p
b <- a/p-a

m <- c()
v <- c()
for(i in 1:length(p)){
    atest=rbeta(10000,a[i],b[i])
    m[i]=mean(atest)
    v[i]=sd(atest)
    }
plot(p,v)
