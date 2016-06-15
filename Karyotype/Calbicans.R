library(ggbio)
library(GenomicRanges)
# add 30 kb to each side of the centromere to make them visible
chromosomes <- Seqinfo(c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chrR"), 
                          seqlengths=c(3188363, 2231838, 1799339, 1603308, 1190869, 1033476, 949590, 2286315), 
                          isCircular= rep(FALSE, 8), genome=rep('Calbicans', 8)
)

gr <- GRanges(seqnames = Rle(c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chrR"), rep(4,8)),
              ranges = IRanges(c(1, 1532988, 1564452, 1595917,
                                 1, 1897153, 1928632, 1960113,
                                 1, 793224, 824796, 856369,
                                 1, 962473, 994291, 1026111,
                                 1, 438680, 470194, 501710,
                                 1, 949801, 981677, 1013555,
                                 1, 395806, 427256, 458707,
                                 1, 1713087, 1745296, 1777507), 
                               end=c(1532987, 1564452, 1595916, 3188363, 
                                     1897152, 1928632, 1960112, 2231838,
                                     793223, 824796, 856368, 1799339,
                                     962472, 994291, 1026110, 1603308,
                                     438679, 470194, 501709, 1190869,
                                     949800, 981677, 1013554, 1033476,
                                     395805, 427256, 458706, 949590,
                                     1713086, 1745296, 1777506, 2286315)),
              strand = rep('*', 32),
              name = factor(rep(c('p1.1', 'p11.1', 'q11', 'q1'), 8)),
              gieStain = factor(rep(c('gneg', 'acen', 'acen', 'gneg'), 8)),
              seqinfo = chromosomes
)

Calbicans_karyo <- autoplot(gr,layout="karyogram", cytoband = TRUE)+ theme(legend.position="none")

