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

GetAllMuMax <- function(growth){
  tmax<- max(growth$t)
  growth <- group_by(growth, Sample, Column, Row) %>% nest()
  results <- mutate(growth, Mod=map(data, possibly(MuMax, NA)))
  results <- mutate(results, params= Mod %>% map(tidy))
  results <- unnest(results, params) %>% 
    subset(term == 'mumax') %>% 
    select(Sample, mumax=round(estimate, digits=2), mumax.se=std.error, mumax.p=p.value) %>%
    full_join(growth) %>%
    mutate(t=tmax) %>%
    subset(mumax.p <0.01)
  results  
}

deg30 <- read.delim("GrowthCurves/Growth_30C_AXL2.txt", header=TRUE, check.names = FALSE)
deg30<-prepareData(deg30)
deg30 <-GetAllMuMax(deg30)
colnames(deg30)[c(2,3,4)] <- c('mumax30', 'mumax.se30', 'mumax.p30')

deg39 <- read.delim("GrowthCurves/Growth_39C_AXL2.txt", header=TRUE, check.names = FALSE)
deg39 <-prepareData(deg39)
deg39 <- GetAllMuMax(deg39)
colnames(deg39)[c(2,3,4)] <- c('mumax39', 'mumax.se39', 'mumax.p39')


info <- read.delim("GrowthCurves/Growth_AXL2_LoH.txt", header=TRUE, check.names = FALSE)
info<-info[-c(1,2,3,4),]

combined <- deg39[,c(1,2,3)] %>% full_join(deg30) %>% full_join(info)
ggplot(combined, aes(x=mumax30, y=mumax39, colour=Genotype)) + 
  geom_point() +
  facet_grid(. ~ Hsp90_Stress) +
  coord_fixed() +
  scale_x_continuous(breaks=c(.3,.6,.9,1.2)) +
  scale_y_continuous(breaks=c(.3,.6,.9,1.2))

labels <- left_join(deg30, info)


ggplot(unnest(labels[,-8], data), aes(x=t, y=log10(OD), colour=Hsp90_Stress)) +
  geom_point(size=.01) +
  geom_text(aes(label=round(mumax30, digits=3)), data=labels, colour='black', size=2, y=-1, vjust=-1, hjust=1) +
  facet_grid(Row ~ Column) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.direction= "horizontal")

