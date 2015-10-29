
op = par(no.readonly = TRUE)

library(circlize)
par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
circos.par("clock.wise" = FALSE, "gap.degree" = 0)
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1))
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
x1 = runif(100)
y1 = runif(100)
circos.points(x1, y1, pch = 16, cex = 0.5)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.lines(1:100/100, y1, pch = 16, cex = 0.5)
circos.clear()

rect(0, 0, 1, 1)
text(0, 0, 0, adj = c(0.5, 1))
text(1, 0, 1, adj = c(0.5, 1))
text(0, 1, 1, adj = c(0.5, 0))


par(mar = c(1, 1, 1, 1))
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1),"clock.wise" = FALSE, "gap.degree" = 0, points.overflow.warning = FALSE)
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.points(x1, y1, pch = 16, cex = 0.5)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.lines(1:100/100, y1, pch = 16, cex = 0.5)
circos.clear()
box()
par(xpd = NA)
text(0, 0, 0, adj = c(0.5, 1))
text(1, 0, 1, adj = c(0.5, 1))
text(0, 1, 1, adj = c(0.5, 0))

par(op)
