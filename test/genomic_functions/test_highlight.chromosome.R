
# highlight 
circos.par("default.track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = c("axis", "labels"))

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})


highlight.chromosome("chr1", col = "red")
highlight.chromosome("chr2", col = "yellow", track.index = 2)
circos.clear()
