pdf("genomic_points.pdf", width = 6, height = 6)
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.1, start.degree = 90,
	canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270)
circos.initializeWithIdeogram(chromosome.index = "chr1")


# data frame with single value column
bed = generateRandomBed(nr = 300)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "A", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
	i = getI(...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "B", adj = c(1, 0.5))

bed1 = generateRandomBed(nr = 300)
bed2 = generateRandomBed(nr = 300)
bed_list = list(bed1, bed2)

# data frame list
circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "C", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "D", adj = c(1, 0.5))


bed = generateRandomBed(nr = 300, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	circos.genomicPoints(region, value, cex = 0.5, pch = 16, col = 1:4, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "E", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "F", adj = c(1, 0.5))

circos.clear()

dev.off()
