## link
bed1 = generateRandomBed(nr = 100)
bed1 = bed1[sample(100, 20), ]
bed2 = generateRandomBed(nr = 100)
bed2 = bed2[sample(100, 20), ]
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

circos.genomicLink(bed1, bed2)
circos.clear()

