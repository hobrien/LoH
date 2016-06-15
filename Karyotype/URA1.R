library(ggbio)
library(GenomicRanges)

source("../Karyotype/Calbicans.R")
print(Calbicans_karyo)
chaperones <- read.delim2("../Karyotype/cochaperones.txt")
chaperones_GR <-makeGRangesFromDataFrame(chaperones, seqinfo = chromosomes, keep.extra.columns = TRUE, ignore.strand=TRUE)
chaperones_GR$y <- 5
chaperones_GR$y2 <- 8
Calbicans_karyo + layout_karyogram(data=chaperones_GR, aes(x=start, y=y), geom = 'point', shape = 25, colour='red') +
  layout_karyogram(data=chaperones_GR, aes(x=start, y=y2, label=locus), geom = 'text', size=20)

clients <- read.delim("../Karyotype/clients.txt")
clients_GR <-makeGRangesFromDataFrame(clients, seqinfo = chromosomes, keep.extra.columns = TRUE, ignore.strand=TRUE)
Calbicans_karyo + layout_karyogram(data=clients_GR, geom = 'rect') +
  layout_karyogram(data=URA1_GR, aes(x=start, y=y), geom = 'point', shape = 25, colour='red')



source("../Karyotype/Calbicans.R")
print(Calbicans_karyo)
URA1 <- read.delim2("../Karyotype/URA1.txt")
URA1_GR <-makeGRangesFromDataFrame(URA1, seqinfo = chromosomes, keep.extra.columns = TRUE, ignore.strand=TRUE)
URA1_GR$y <- 5
URA1_GR$y2 <- 8
Calbicans_karyo + layout_karyogram(data=URA1_GR, aes(x=start, y=y), geom = 'point', shape = 25, colour='red') +
  layout_karyogram(data=URA1_GR, aes(x=start, y=y2, label=locus), geom = 'text', size=2)

clients <- read.delim("../Karyotype/clients.txt")
clients_GR <-makeGRangesFromDataFrame(clients, seqinfo = chromosomes, keep.extra.columns = TRUE, ignore.strand=TRUE)
Calbicans_karyo + layout_karyogram(data=clients_GR, geom = 'rect') +
  layout_karyogram(data=URA1_GR, aes(x=start, y=y), geom = 'point', shape = 25, colour='red')

