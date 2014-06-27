
pdf("genomic_rect.pdf", width = 6, height = 6)
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0), start.degree = 90,
	canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270)
circos.initializeWithIdeogram(chromosome.index = "chr1")

f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = f(value[[1]]), border = f(value[[1]]), ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "A", adj = c(1, 0.5))

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = f(value[[1]]), border = f(value[[1]]), ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "B", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed_list, ylim = c(0, 3), panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicRect(region, value, ytop = i+0.4, ybottom = i-0.4, col = f(value[[1]]), border = f(value[[1]]), ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "C", adj = c(1, 0.5))

bed1= generateRandomBed(nr = 200)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, ytop.column = 1, ybottom = 0, col = ifelse(value[[1]] > 0, "red", "green"), ...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(0, 0), lty = 2, col = "#00000040")
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "D", adj = c(1, 0.5))

circos.clear()

dev.off()

