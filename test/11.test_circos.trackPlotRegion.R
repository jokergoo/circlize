
factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 5), factors = sample(1:4, 100, replace = TRUE), x = runif(100), y = runif(100), panel.fun = function(x, y) {
	circos.points(x, y)
})
