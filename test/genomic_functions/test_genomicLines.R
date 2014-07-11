
par(mar = c(1, 1, 1, 1))


## test line

### test bed
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "l", ...)
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, col = 1:4, ...)
})

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
})

circos.clear()

##############################################################
### test numeric column
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 20)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, ...)
})

bed = generateRandomBed(nr = 100)
bed = cbind(bed[1:3], rep(1, nrow(bed)), bed[4])
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, ...)
})
circos.genomicTrackPlotRegion(bed, numeric.column = 5, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, ...)
})
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, numeric.column = 1, ...)
})
circos.clear()

#####################################################################
### test error situation
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100, nc = 0)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed = cbind(bed, rep("text", nrow(bed)))
circos.genomicTrackPlotRegion(bed, numeric.column = 4, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed1 = generateRandomBed(nr = 100, nc = 1)
bed2 = generateRandomBed(nr = 100, nc = 0)
bed_list = list(bed1, bed2)
circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})
circos.clear()


#####################################################################
### test error situation
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100, nc = 0)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

bed = cbind(bed, rep("text", nrow(bed)))
circos.genomicTrackPlotRegion(bed, numeric.column = 4, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

bed1 = generateRandomBed(nr = 100, nc = 1)
bed2 = generateRandomBed(nr = 100, nc = 0)
bed_list = list(bed1, bed2)
circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})
circos.clear()

