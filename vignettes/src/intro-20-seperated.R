
op = par(no.readonly = TRUE)

library(circlize)
layout(rbind(c(1,1,2,2), c(0, 3, 3,0)))
par(mar = c(2, 2, 2, 2))


factors = letters[1:4]
circos.par("canvas.xlim" = c(-1, 1.5), "canvas.ylim" = c(-1, 1.5), start.degree = -45)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a")
circos.text(0.5, 0.5, "first one", niceFacing = TRUE)
circos.updatePlotRegion(sector.index = "b")
circos.text(0.5, 0.5, "first one", niceFacing = TRUE)

circos.clear()
box()
axis(side = 1)
axis(side = 2)

circos.par("canvas.xlim" = c(-1.5, 1), "canvas.ylim" = c(-1.5, 1), start.degree = -45)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "d")
circos.text(0.5, 0.5, "second one", niceFacing = TRUE)
circos.updatePlotRegion(sector.index = "c")
circos.text(0.5, 0.5, "second one", niceFacing = TRUE)

circos.clear()
box()
axis(side = 1)
axis(side = 2)

factors = letters[1:4]
circos.par("canvas.xlim" = c(-1, 1.5), "canvas.ylim" = c(-1, 1.5), start.degree = -45, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a")
circos.text(0.5, 0.5, "first one", niceFacing = TRUE)
circos.updatePlotRegion(sector.index = "b")
circos.text(0.5, 0.5, "first one", niceFacing = TRUE)

circos.clear()

par(new = TRUE)
circos.par("canvas.xlim" = c(-1.5, 1), "canvas.ylim" = c(-1.5, 1), start.degree = -45)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "d")
circos.text(0.5, 0.5, "second one", niceFacing = TRUE)
circos.updatePlotRegion(sector.index = "c")
circos.text(0.5, 0.5, "second one", niceFacing = TRUE)

circos.clear()

par(op)
