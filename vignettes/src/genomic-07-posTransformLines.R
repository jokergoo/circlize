library(circlize)

op = par(no.readonly = TRUE)

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

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = f(value[[1]]), 
		border = f(value[[1]]), posTransform = posTransform.default, ...)
}, bg.border = NA)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, direction = "outside", horizontalLine = "bottom", track.height = 0.1)

cytoband = read.cytoband()$df
circos.genomicTrackPlotRegion(cytoband, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = cytoband.col(value[[2]]), border = NA)
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], border = "black")
}, track.height = 0.05, bg.border = NA)

circos.clear()

par(op)
