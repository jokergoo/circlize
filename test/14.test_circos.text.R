source("R/plot.R")
source("R/utils.R")
source("R/global.R")
source("R/link.R")

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y, ...) {
	circos.text(0.5, 0.5, labels = "abcdefghijklmnopqrstuvwxyz", direction = "arc", adj = c(0.5, 0.5), cex = 1.4)
	circos.points(0.5, 0.5, pch = 16, col = "red")
})



par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
	circos.text(3, 9, "inside", facing = "inside")
	circos.text(7, 9, "outside", facing = "outside")
	circos.text(10, 5, "reverse.clockwise", facing = "reverse.clockwise")
	circos.text(0, 5, "clockwise", facing = "clockwise")
	circos.text(5, 5, "downward", facing = "downward")
	circos.text(5, 1, "bending", facing = "bending")
})
circos.clear()

par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
	circos.text(3, 9, "inside", facing = "inside", niceFacing = TRUE)
	circos.text(7, 9, "outside", facing = "outside", niceFacing = TRUE)
	circos.text(9, 5, "reverse.clockwise", facing = "reverse.clockwise", niceFacing = TRUE)
	circos.text(1, 5, "clockwise", facing = "clockwise", niceFacing = TRUE)
	circos.text(5, 5, "downward", facing = "downward", niceFacing = TRUE)
	circos.text(5, 1, "bending", facing = "bending", niceFacing = TRUE)
})
circos.clear()

###################################################################################
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2), xpd = NA)
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
