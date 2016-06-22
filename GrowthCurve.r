library(ggplot2)
library(nlsMicrobio)
library(tidyr)
library(dplyr)
prepareData <- function(df, temp) {
  df2 <- gather(df, "t", "OD", 4:ncol(df)) 
  df2$Temperature <- as.factor(temp)
  df2$t <- as.numeric(df2$t)
  df2
}
df <- read.delim("~/BTSync/Code/LoH/growth.txt", header=TRUE, check.names = FALSE)

df<-prepareData(df, 50)
ggplot(df, aes(x=t, y=log10(OD), group=paste(Sample, Temperature), colour=Temperature)) +
  geom_point(size=.01) +
  facet_grid(Row ~ Column) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.direction= "horizontal")

test <-subset(df, Sample == 'Sample X3')
test$LOG10N<-log10(test$OD+1)
test <- select(test, t, LOG10N)
plot(test)
nls <-nls(baranyi, test, list(lag=1, mumax=.5, LOG10N0 = .1, LOG10Nmax = 0.45))
plotfit(nls, smooth=TRUE)
