library(circlize)

par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
### rect matrix
circos.par(cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100, nc = 4)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, horizontalLine = "top", track.height = 0.1)

f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = f(value[[1]]), 
		border = f(value[[1]]), posTransform = posTransform.default, ...)
}, bg.border = NA)

circos.clear()


circos.par(cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 20, nc = 4)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 0, adj = c(1, 0.5), labels = "gene", facing = "reverse.clockwise",
	    posTransform = posTransform.default)
}, bg.border = NA)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, horizontalLine = "bottom", type = "reverse", track.height = 0.1)

cytoband = read.cytoband()$df
circos.genomicTrackPlotRegion(cytoband, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = cytoband.col(value$V5), border = NA, ...)
}, track.height = 0.05)

circos.clear()

