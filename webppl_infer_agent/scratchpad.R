libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
message (paste0('Load libjvm.dylib from: ',libjvm))
dyn.load(libjvm)

## library(ggplot2)
## times <- read.csv("rowtimes.csv")

## ggplot(times,aes(x=rows,y=fittime))+geom_point()+theme_bw()
