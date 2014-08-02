
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
    circos.text(3, 9, "inside", facing = "inside", cex = 0.8)
    circos.text(7, 9, "outside", facing = "outside", cex = 0.8)
    circos.text(0, 5, "reverse.clockwise", facing = "reverse.clockwise", adj = c(0.5, 0), cex = 0.8)
    circos.text(10, 5, "clockwise", facing = "clockwise", adj = c(0.5, 0), cex = 0.8)
    circos.text(5, 5, "downward", facing = "downward", cex = 0.8)
    circos.text(5, 1, "bending", facing = "bending", cex = 0.8)
})
circos.clear()

par(op)
