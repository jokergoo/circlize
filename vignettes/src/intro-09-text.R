library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(2, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
    circos.text(3, 9, "inside", facing = "inside", cex = 0.8)
    circos.text(7, 9, "outside", facing = "outside", cex = 0.8)
    circos.text(0, 5, "reverse.clockwise", facing = "reverse.clockwise", adj = c(0.5, 0), cex = 0.8)
    circos.text(10, 5, "clockwise", facing = "clockwise", adj = c(0.5, 0), cex = 0.8)
    circos.text(5, 5, "downward", facing = "downward", cex = 0.8)
    circos.text(5, 1, "bending", facing = "bending", cex = 0.8)
})
circos.clear()

factors = LETTERS[1:20]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), track.height = 0.5, 
	panel.fun = function(x, y) {
		xlim = get.cell.meta.data("xlim")
		ylim = get.cell.meta.data("ylim")
		theta = mean(get.cell.meta.data("xplot")) %% 360
		sector.index = get.cell.meta.data("sector.index")
		if(theta < 90 || theta > 270) {
			text.facing = "clockwise"
			text.adj = c(0, 0.5)
		} else {
			text.facing = "reverse.clockwise"
			text.adj = c(1, 0.5)
		}
		circos.text(mean(xlim), ylim[1], labels = paste(rep(sector.index, 8), collapse = ""),
			facing = text.facing, adj = text.adj, cex = 0.8)
})
circos.clear()