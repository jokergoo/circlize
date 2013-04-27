
source("R/global.R")
source("R/plot.R")
source("R/utils.R")

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y, ...) {
	circos.text(0.5, 0.5, labels = "abcdefghijklmnopqrstuvwxyz", direction = "arc", adj = c(0.5, 0.5), cex = 1.4)
	circos.points(0.5, 0.5, pch = 16, col = "red")
})

circos.clear()
