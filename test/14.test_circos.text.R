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
	circos.text(3, 9, "default", direction = "default")
	circos.text(7, 9, "default2", direction = "default2")
	#circos.points(5, 9, pch = 16, col = "red") 
	circos.text(0, 5, "vertical_left", direction = "vertical_left")
	#circos.points(0, 5, pch = 16, col = "red") 
	circos.text(10, 5, "vertical_right", direction = "vertical_right")
	#circos.points(10, 5, pch = 16, col = "red") 
	circos.text(5, 5, "horizontal", direction = "horizontal")
	#circos.points(5, 5, pch = 16, col = "red") 
	circos.text(5, 1, "arc_arc_arc_arc_arc", direction = "arc")
	#circos.points(5, 1, pch = 16, col = "red") 
})
circos.clear()
