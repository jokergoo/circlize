
par(mar = c(1, 1, 1, 1))

# rainfall
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 1000)
circos.genomicRainfall(bed, pch = 16, cex = 0.6, bg.border = NA)

bed1 = generateRandomBed(nr = 1000)
bed2 = generateRandomBed(nr = 1000)
bed_list = list(bed1, bed2)
circos.genomicRainfall(bed_list, pch = 16, cex = 0.2, col = c("#FF000040", "#0000FF40"), bg.border = NA)

bed = data.frame(chr = rep("chr1", 4),
                 start = c(1000, 2000, 3000, 4000),
				 end = c(3000, 4000, 5000, 6000))
circos.genomicRainfall(bed, pch = 16, cex = 0.6, bg.border = NA)

circos.clear()
