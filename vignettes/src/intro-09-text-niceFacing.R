
op = par(no.readonly = TRUE)

library(circlize)

###################################################################################
par(mar = c(1, 1, 1, 1), mfrow = c(3, 2), xpd = NA)
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'clockwise'\nadj = c(0, 0.5)", facing = "inside", cex = 0.7)
	} else {
		circos.points(3, 5, pch = 16, col = "red", cex = 1)
		circos.text(3, 5, "rawText", facing = "clockwise", adj = c(0, 0.5), cex = 0.7)
		circos.points(7, 5, pch = 16, col = "red", cex = 1)
		circos.text(7, 5, "niceFacing", facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'reverse.clockwise'\nadj = c(0, 0.5)", facing = "inside", cex = 0.7)
	} else {
		circos.points(3, 5, pch = 16, col = "red", cex = 1)
		circos.text(3, 5, "rawText", facing = "reverse.clockwise", adj = c(0, 0.5), cex = 0.7)
		circos.points(7, 5, pch = 16, col = "red", cex = 1)
		circos.text(7, 5, "niceFacing", facing = "reverse.clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()

factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'reverse.clockwise'\nadj = c(1, 0.5)", facing = "inside", cex = 0.7)
	} else {
		circos.points(3, 5, pch = 16, col = "red", cex = 1)
		circos.text(3, 5, "rawText", facing = "reverse.clockwise", adj = c(1, 0.5), cex = 0.7)
		circos.points(7, 5, pch = 16, col = "red", cex = 1)
		circos.text(7, 5, "niceFacing", facing = "reverse.clockwise", niceFacing = TRUE, adj = c(1, 0.5), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'clockwise'\nadj = c(1, 0.5)", facing = "inside", cex = 0.7)
	} else {
		circos.points(3, 5, pch = 16, col = "red", cex = 1)
		circos.text(3, 5, "rawText", facing = "clockwise", adj = c(1, 0.5), cex = 0.7)
		circos.points(7, 5, pch = 16, col = "red", cex = 1)
		circos.text(7, 5, "niceFacing", facing = "clockwise", niceFacing = TRUE, adj = c(1, 0.5), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()

factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'inside'\nadj = c(0.5, 0)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawText", facing = "inside", adj = c(0.5, 0), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacing", facing = "inside", niceFacing = TRUE, adj = c(0.5, 0), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'outside'\nadj = c(0.5, 0)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawText", facing = "outside", adj = c(0.5, 0), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacing", facing = "outside", niceFacing = TRUE, adj = c(0.5, 0), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()

factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'outside'\nadj = c(0.5, 1)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawText", facing = "outside", adj = c(0.5, 1), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacing", facing = "outside", niceFacing = TRUE, adj = c(0.5, 1), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "f") {
		circos.text(5, 5, "facing = 'inside'\nadj = c(0.5, 1)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawText", facing = "inside", adj = c(0.5, 1), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacing", facing = "inside", niceFacing = TRUE, adj = c(0.5, 1), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()

#########################################################
## bending
factors = letters[1:4]
circos.par(start.degree = 90)
circos.initialize(factors = factors, xlim = c(0, 10), sector.width = c(7/3, 7/3, 7/3, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "d") {
		circos.text(5, 5, "facing = 'bending.inside'\nadj = c(0.5, 0)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawTextrawTextrawTextrawTextrawText", facing = "bending.inside", adj = c(0.5, 0), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacingniceFacingniceFacingniceFacing", facing = "bending.inside", niceFacing = TRUE, adj = c(0.5, 0), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "d") {
		circos.text(5, 5, "facing = 'bending.outside'\nadj = c(0.5, 0)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawTextrawTextrawTextrawText", facing = "bending.outside", adj = c(0.5, 0), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacingniceFacingniceFacing", facing = "bending.outside", niceFacing = TRUE, adj = c(0.5, 0), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()

factors = letters[1:4]
circos.par(start.degree = 90)
circos.initialize(factors = factors, xlim = c(0, 10), sector.width = c(7/3, 7/3, 7/3, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "d") {
		circos.text(5, 5, "facing = 'bending.outside'\nadj = c(0.5, 1)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawTextrawTextrawTextrawTextrawText", facing = "bending.outside", adj = c(0.5, 1), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacingniceFacingniceFacingniceFacing", facing = "bending.outside", niceFacing = TRUE, adj = c(0.5, 1), cex = 0.7)
	}
}, track.height = 0.2)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), panel.fun = function(x, y) {
	si = get.cell.meta.data("sector.index")
	if(si == "d") {
		circos.text(5, 5, "facing = 'bending.inside'\nadj = c(0.5, 1)", facing = "inside", cex = 0.7)
	} else {
		circos.points(5, 3, pch = 16, col = "red", cex = 1)
		circos.text(5, 3, "rawTextrawTextrawTextrawText", facing = "bending.inside", adj = c(0.5, 1), cex = 0.7)
		circos.points(5, 7, pch = 16, col = "red", cex = 1)
		circos.text(5, 7, "niceFacingniceFacingniceFacing", facing = "bending.inside", niceFacing = TRUE, adj = c(0.5, 1), cex = 0.7)
	}
}, track.height = 0.2)
circos.clear()


par(op)
