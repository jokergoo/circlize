
set.seed(12345)

par(mar = c(1, 1, 1, 1))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100, fun = function(k) rep("text", k))

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 0, labels.column = 1, facing = "clockwise", adj = c(0, 0.5),
		posTransform = posTransform.text, cex = 0.4, niceFacing = F)
}, track.height = 0.1)
i_track = get.cell.meta.data("track.index")


circos.genomicPosTransformLines(bed, 
	posTransform = quote(posTransform.text(region, y = 0, labels = value[[1]], cex = 0.4, track.index = i_track)),
	direction = "outside"
)

circos.par(cell.padding = c(0, 0, 0, 0))
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)


bed = generateRandomBed(nr = 100, fun = function(k) rep("text", k))

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), track.height = 0.1, bg.border = NA)
i_track = get.cell.meta.data("track.index")

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 1, labels.column = 1, facing = "clockwise", adj = c(1, 0.5),
		posTransform = posTransform.text, cex = 0.4, niceFacing = F, padding = 0.1)
}, track.height = 0.1)


circos.genomicPosTransformLines(bed, 
	posTransform = quote(posTransform.text(region, y = 1, labels = value[[1]], cex = 0.4, track.index = i_track+1, padding = 0.1)),
	direction = "inside", track.index = i_track
)

