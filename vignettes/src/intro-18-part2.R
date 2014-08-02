
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
x1 = runif(100)
y1 = runif(100)
circos.points(x1, y1, pch = 16, cex = 0.5)

circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
x1 = runif(100)
y1 = runif(100)
circos.points(x1, y1, pch = 16, cex = 0.5)

circos.trackPlotRegion(factors = factors, ylim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1))
circos.clear()

par(op)
