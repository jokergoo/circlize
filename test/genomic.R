

df2GRanges = function(df, ...) {
	GRanges(seqnames = df[[1]],
	        ranges = IRanges(start = df[[2]],
			                 end = df[[3]]),
			mcols = df[, -(1:3), drop = FALSE],
			...)
}



### test bed
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	iStack = getIStack(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = iStack, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	circos.genomicPoints(region, value, cex = 1, col = 1:4, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	iStack = getIStack(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = iStack, ...)
})

circos.clear()

## test line

### test bed
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "l", ...)
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	iStack = getIStack(...)
	circos.genomicLines(region, value, col = iStack, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, col = 1:4, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	iStack = getIStack(...)
	circos.genomicLines(region, value, col = iStack, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "segment", ...)
})

circos.clear()


### rect
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, ...)
}, bg.border = NA)

circos.genomicPosTransformLine(bed, posTransform = posTransform.default)

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, posTransform = posTransform.default, ...)
}, bg.border = NA)

circos.genomicPosTransformLine(bed, posTransform = posTransform.default, type = "reverse")

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicRect(region, value, col = sample(1:10, nrow(region), replace = TRUE), 
		border = NA, ...)
}, bg.border = NA)


circos.clear()
