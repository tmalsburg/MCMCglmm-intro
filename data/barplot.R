library(plyr)
library(ggplot2)
data<-read.csv("data.tsv", sep="")

summary <- ddply(data,.(subject, c, a, b),summarize,prop=mean(p=="1")) #
summary <- ddply(summary,.(c, a, b),summarize,mean=mean(prop), N=length(prop),se=sd(prop)/sqrt(N))

summary$a <- factor(summary$a)
summary$b <- factor(summary$b)
summary$c <- factor(summary$c)

g <- ggplot(summary,aes(x = interaction(c, b),fill= a, y=mean))
g <- g + geom_bar(stat="identity",pos="dodge",colour="black")
g <- g + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size= .5, width=.2, position = position_dodge(.9))
g <- g + ylim(c(0,1))
g <- g + theme_bw(base_size=12)
g <- g + ylab("Proportion of p")
print(g)
#ggsave("three_way_mean.pdf",height=4,width=4)


