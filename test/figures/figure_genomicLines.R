pdf("genomic_lines.pdf", width = 6, height = 6)
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.1, start.degree = 90,
	canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270)
circos.initializeWithIdeogram(chromosome.index = "chr1")

bed = generateRandomBed(nr = 500)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "l", ...)
})

bed1 = generateRandomBed(nr = 500)
bed2 = generateRandomBed(nr = 500)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 500, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, col = 1:4, ...)
})

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 200)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
})

circos.clear()
dev.off()
