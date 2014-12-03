
op = par(no.readonly = TRUE)

library(circlize)
set.seed(12345)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par("canvas.xlim" = c(-1.5, 1.5), "canvas.ylim" = c(-1.5, 1.5), "gap.degree" = 10)
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})
circos.lines(c(1/20, 0.5), c(1/20, 3), sector.index = "d", straight = TRUE)
circos.text(0.5, 3, "mark", sector.index = "d", adj = c(0.5, 0))

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})

text(0, 0, "this is\nthe center", cex = 1.5)
legend("bottomleft", pch = 1, legend = "this is the legend")
circos.clear()

par(op)
