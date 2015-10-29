op = par(no.readonly = TRUE)

set.seed(123458)

par(mfrow = c(2, 2))
par(mar = c(1, 1, 1, 1))


bed = generateRandomBed(nr = 400, fun = function(k) rep("text", k))
bed = bed[-(9:13), ]
##########################################
circos.par("start.degree" = 90, canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270, cell.padding = c(0, 0, 0, 0), track.margin = c(0.005, 0.005))
circos.initializeWithIdeogram(plotType = c("axis"), chromosome.index = "chr1")
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 0, labels.column = 1, facing = "clockwise", adj = c(0, 0.5),
		posTransform = posTransform.text, cex = 0.8, niceFacing = F)
}, track.height = 0.1, bg.border = NA)
i_track = get.cell.meta.data("track.index")

circos.genomicPosTransformLines(bed, 
	posTransform = function(region, value) posTransform.text(region, y = 0, labels = value[[1]], cex = 0.8, track.index = i_track),
	direction = "outside"
)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)

circos.clear()

text(0, 0.05, "posTransform.text\ndirection = 'outside'", adj = c(0, 0))

############################################
circos.par("start.degree" = 90, canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270, cell.padding = c(0, 0, 0, 0), track.margin = c(0.005, 0.005))
circos.initializeWithIdeogram(plotType = c("axis"), chromosome.index = "chr1")
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 0, labels.column = 1, facing = "clockwise", adj = c(0, 0.5),
		posTransform = posTransform.default, cex = 0.8, niceFacing = F)
}, track.height = 0.1, bg.border = NA)
i_track = get.cell.meta.data("track.index")

circos.genomicPosTransformLines(bed, posTransform = posTransform.default, direction = "outside"
)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)

circos.clear()
text(0, 0.05, "posTransform.default\ndirection = 'outside'", adj = c(0, 0))

##############################################################################
circos.par("start.degree" = 90, canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270, cell.padding = c(0, 0, 0, 0), track.margin = c(0.005, 0.005))
circos.initializeWithIdeogram(plotType = c("axis"), chromosome.index = "chr1")
circos.par(cell.padding = c(0, 0, 0, 0))
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), track.height = 0.1, bg.border = NA)
i_track = get.cell.meta.data("track.index")

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 1, labels.column = 1, facing = "clockwise", adj = c(1, 0.5),
		posTransform = posTransform.text, cex = 0.8, niceFacing = F)
}, track.height = 0.1, bg.border = NA)

circos.genomicPosTransformLines(bed, 
	posTransform = function(region, value) posTransform.text(region, y = 1, labels = value[[1]], cex = 0.8, track.index = i_track+1),
	direction = "inside", track.index = i_track
)
circos.clear()
text(0, 0.05, "posTransform.text\ndirection = 'inside'", adj = c(0, 0))

##############################################################################
circos.par("start.degree" = 90, canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270, cell.padding = c(0, 0, 0, 0), track.margin = c(0.005, 0.005))
circos.initializeWithIdeogram(plotType = c("axis"), chromosome.index = "chr1")
circos.par(cell.padding = c(0, 0, 0, 0))
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.points( (region[[1]] + region[[2]])/2, rep(0.5, nrow(region)), pch = 16)
}, track.height = 0.02, bg.border = NA)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), track.height = 0.1, bg.border = NA)
i_track = get.cell.meta.data("track.index")

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 1, labels.column = 1, facing = "clockwise", adj = c(1, 0.5),
		posTransform = posTransform.text, cex = 0.8, niceFacing = F, padding = 0.2)
}, track.height = 0.1, bg.border = NA)


circos.genomicPosTransformLines(bed, 
	posTransform = function(region, value) posTransform.text(region, y = 1, labels = value[[1]], cex = 0.8, track.index = i_track+1, padding = 0.2),
	direction = "inside", track.index = i_track
)
circos.clear()
text(0, 0.05, "posTransform.text\ndirection = 'inside'\npadding = 0.2", adj = c(0, 0))

par(op)
