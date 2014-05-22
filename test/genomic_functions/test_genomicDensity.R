
par(mar = c(1, 1, 1, 1))
# density
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 10000)
circos.genomicDensity(bed, bg.border = NA)
circos.genomicDensity(bed, type = "s", bg.border = NA)

bed = bed[bed[[1]] != "chr1", , drop = FALSE]
circos.genomicDensity(bed, bg.border = NA)

bed1 = generateRandomBed(nr = 10000)
bed2 = generateRandomBed(nr = 10000)
bed_list = list(bed1, bed2)
circos.genomicDensity(bed_list, col = c("#FF000040", "#0000FF40"), bg.border = NA)

circos.clear()
