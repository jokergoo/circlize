
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

library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:2]
for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
	})
	show.index()
	circos.clear()
	Sys.sleep(1)
}
