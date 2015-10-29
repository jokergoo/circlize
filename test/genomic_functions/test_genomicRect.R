
############################
### rect matrix
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, ...)
	i = getI(...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	#circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
}, bg.border = NA)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, horizontalLine = "top")

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, posTransform = posTransform.default, ...)
	i = getI(...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	#circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
}, bg.border = NA)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, type = "reverse", horizontalLine = "bottom")

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, ...)
	i = getI(...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	#circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
}, bg.border = NA)

circos.clear()

##########################
### rect from bed list
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = NULL)

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)
f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	
	circos.genomicRect(region, value, col = f(value[[1]]), 
		border = NA, ...)
	i = getI(...)
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(i, i), lty = 2, col = "#000000")
})

circos.genomicTrackPlotRegion(bed_list, ylim = c(0, 3), panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicRect(region, value, ytop = i+0.4, ybottom = i-0.4, col = f(value[[1]]), 
		border = NA, ...)
	
	cell.xlim = get.cell.meta.data("cell.xlim")
	circos.lines(cell.xlim, c(i, i), lty = 2, col = "#000000")
})

circos.genomicTrackPlotRegion(bed1, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = "red", border = NA, ...)

})

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicRect(region, value, col = i, border = NA, ...)

})

circos.clear()


