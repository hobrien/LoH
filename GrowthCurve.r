library(ggplot2)
library(nlsMicrobio)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
prepareData <- function(df, temp) {
  df2 <- gather(df, "t", "OD", 4:ncol(df)) 
  df2$Temperature <- as.factor(temp)
  df2$t <- as.numeric(df2$t)
  df2
}
df <- read.delim("~/BTSync/Code/LoH/growth.txt", header=TRUE, check.names = FALSE)

df<-prepareData(df, 50)
mid_time <- ifelse(length(df$t) %% 2 == 0, median(df$t[1:length(df$t)-1]), median(df$t))
mid_od <-log10(median(df$OD))
ggplot(df, aes(x=t, y=log10(OD), group=paste(Sample, Temperature), colour=Temperature)) +
  geom_point(size=.01) +
  geom_text(aes(label=round(mumax, digits=3)), data=labels, colour='black', size=2, y=min(log10(df$OD)), vjust=-1, hjust=1) +
  facet_grid(Row ~ Column) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.direction= "horizontal")

df2 <- group_by(df, Sample) %>% nest()

MuMax <- function(test){
    test$LOG10N<-log10(test$OD)
    test <- select(test, t, LOG10N)
      nls(baranyi, test, list(lag=1, 
                            mumax=min(1,(max(test$LOG10N)-min(test$LOG10N))*2), 
                            LOG10N0 = min(test$LOG10N), 
                            LOG10Nmax = max(test$LOG10N)))
}
AllMuMax <- mutate(df2, Mod=map(data, possibly(MuMax, NA)))
AllMuMax <- mutate(df2, Mod=map(data, MuMax))
AllMuMax <- mutate(AllMuMax, params= Mod %>% map(tidy))
labels <- unnest(AllMuMax, params) %>% 
  subset(term == 'mumax') %>% 
  select(Sample, mumax=round(estimate, digits=2), mumax.se=std.error) %>%
  full_join(df)
  
labels <- unnest(AllMuMax, params) %>% 
  subset(term == 'LOG10Nmax') %>% 
  select(Sample, nmax=estimate, nmax.se=std.error) %>%
  full_join(labels) %>% 
  filter(t==max(df$t))
