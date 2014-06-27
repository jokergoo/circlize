
pdf("genomicDensity.pdf", width = 8, height = 8)
par(mar = c(1, 1, 1, 1))
# density
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram(plotType = c("axis", "labels"))

bed = generateRandomBed(nr = 10000)
circos.genomicDensity(bed, bg.border = NA)

bed1 = generateRandomBed(nr = 10000)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)
circos.genomicDensity(bed_list, col = c("#FF000040", "#0000FF40"), bg.border = NA)

circos.clear()
dev.off()
