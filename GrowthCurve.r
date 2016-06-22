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

MuMax <- function(od, time){
    test = data.frame(OD=od, t=time)
    #test <-subset(df, Sample == name)
    test$LOG10N<-log10(test$OD)
    test <- select(test, t, LOG10N)
    if (max(test$LOG10N)-min(test$LOG10N) > 0.1) {
         mumax<- tryCatch({
          nls1 <- nls(baranyi, test, list(lag=1, mumax=min(1,(max(test$LOG10N)-min(test$LOG10N))*2), LOG10N0 = min(test$LOG10N), LOG10Nmax = max(test$LOG10N)))
          nls1$m$getPars()[2][[1]]
        }, error = function(err) {
          NA
        })
        
        #plot(test)
        #plotfit(nls1, smooth=TRUE)
        
    }
    else {
        mumax <- 0
    }
    mumax
}

AllMuMax <-df %>% group_by(Sample) %>% summarise(mumax=MuMax(OD, t))
