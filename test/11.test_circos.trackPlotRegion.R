source("R/plot.R")
source("R/utils.R")
source("R/global.R")
source("R/link.R")

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 5), factors = sample(1:4, 100, replace = TRUE), x = runif(100), y = runif(100), panel.fun = function(x, y) {
	circos.points(x, y)
})
circos.trackPlotRegion(ylim = c(0, 5))
circos.trackPlotRegion(ylim = cbind(1:4, 1:4*2), panel.fun = function(x, y) {
	circos.points(runif(20), runif(20)+2)
})
circos.clear()
