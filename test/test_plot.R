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
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0))
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
                circos.lines(c(0,1), c(0,0), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
                circos.lines(c(1,0), c(0,0), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(1)
}

for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), clock.wise=FALSE)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
                circos.lines(c(0,1), c(0,0), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
                circos.lines(c(1,0), c(0,0), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(1)
}
