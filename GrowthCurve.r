library(ggplot2)
library(nlsMicrobio)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
rm(list=ls())

prepareData <- function(df, temp) {
  df<-df[-c(1,2,3,4),]
  df2 <- gather(df, "t", "OD", 4:ncol(df)) 
  df2$t <- as.numeric(df2$t)
  df2
}
MuMax <- function(test){
  test$LOG10N<-log10(test$OD)
  test <- select(test, t, LOG10N)
  nls(baranyi, test, list(lag=1, 
                          mumax=min(1,(max(test$LOG10N)-min(test$LOG10N))*2), 
                          LOG10N0 = min(test$LOG10N), 
                          LOG10Nmax = max(test$LOG10N)))
}

AllMuMax <- function(growth){
  growth <- group_by(growth, Sample, Column, Row) %>% nest()
  AllMuMax <- mutate(growth, Mod=map(data, possibly(MuMax, NA)))
  AllMuMax <- mutate(AllMuMax, params= Mod %>% map(tidy))
  labels <- unnest(AllMuMax, params) %>% 
    subset(term == 'mumax') %>% 
    select(Sample, mumax=round(estimate, digits=2), mumax.se=std.error) %>%
    full_join(growth) %>%
    mutate(t=tmax)
  labels  
}

30deg <- read.delim("GrowthCurves/Growth_30C_AXL2.txt", header=TRUE, check.names = FALSE)
30deg<-prepareData(30deg)

mid_time <- ifelse(length(30deg$t) %% 2 == 0, median(30deg$t[1:length(30deg$t)-1]), median(30deg$t))
min_od <-log10(min(30deg$OD))
tmax <- max(30deg$t)

labels <-AllMuMax(30deg)



info <- read.delim("GrowthCurves/Growth_AXL2_LoH.txt", header=TRUE, check.names = FALSE)
info<-info[-c(1,2,3,4),]
labels <- full_join(labels, info)


ggplot(unnest(labels[,-7], data), aes(x=t, y=log10(OD), colour=Hsp90_Stress)) +
  geom_point(size=.01) +
  geom_text(aes(label=round(mumax, digits=3)), data=labels, colour='black', size=2, y=min_od, vjust=-1, hjust=1) +
  facet_grid(Row ~ Column) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.direction= "horizontal")

