simexp.df <- read.csv("simexp.csv")
View(simexp.df)
with(simexp.df,sum(choice==oraclechoice)/nrow(simexp.df))
