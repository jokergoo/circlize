
library(circlize)

par(mfrow = c(2, 2))

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


par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.1, start.degree = 90,
	canvas.xlim = c(0, 1), canvas.ylim = c(0, 1), gap.degree = 270)
circos.initializeWithIdeogram(chromosome.index = "chr1")

bed = generateRandomBed(nr = 500)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "l", ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "A", adj = c(1, 0.5))

bed1 = generateRandomBed(nr = 500)
bed2 = generateRandomBed(nr = 500)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "B", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "C", adj = c(1, 0.5))

bed = generateRandomBed(nr = 500, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, col = 1:4, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "D", adj = c(1, 0.5))

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "E", adj = c(1, 0.5))

bed = generateRandomBed(nr = 200)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
})
pos = get.cell.meta.data("yplot")
text(0, mean(pos), "F", adj = c(1, 0.5))

circos.clear()



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

