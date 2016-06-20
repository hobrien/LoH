library(ggplot2)
library(nlsMicrobio)

ggplot(df, aes(x=time, y=OD, group=paste(Sample, Temperature), colour=Temperature)) +
  geom_line() +
  facet_grid(Row ~ Column) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.direction= "horizontal")

