
op = par(no.readonly = TRUE)

library(circlize)
layout(rbind(1:4, 5:8, 9:12), width = c(1.5, 2, 1.5, 2))
par(mar = c(1, 1, 1, 1), xpd = NA)

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "circos.initialize", adj = c(0.5, 0.5), cex = 1.2)

circos.par(gap.degree = 5)
circos.initialize(1:4, xlim = rbind(c(0, 1), c(0, 2), c(0, 3), c(0, 4)))
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
for(i in 1:4) {
    de = get.cell.meta.data("cell.start.degree", i, 1) - circos.par("gap.degree")/2
    lines(c(0, cos(de/180*pi)), c(0, sin(de/180*pi)), lty = 2, col = "#999999")
}
draw.sector(start.degree = 0, end.degree = 360, rou1 = 1, col = NA, lty = 2, border = "#999999")
circos.clear()

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "circos.trackPlotRegion", adj = c(0.5, 0.5), cex = 1.2)

circos.par(gap.degree = 5)
circos.initialize(1:4, xlim = rbind(c(0, 1), c(0, 2), c(0, 3), c(0, 4)))
circos.trackPlotRegion(ylim = c(0, 1))
circos.clear()

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "circos.points\ncircos.lines\ncircos.text\n...", adj = c(0.5, 0.5), cex = 1.2)

circos.par(gap.degree = 5)
circos.initialize(1:4, xlim = rbind(c(0, 1), c(0, 2), c(0, 3), c(0, 4)))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    circos.points(runif(20)*(xlim[2] - xlim[1])+xlim[1], runif(20), pch = 16)
})
circos.clear()

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "circos.trackPlotRegion", adj = c(0.5, 0.5), cex = 1.2)

circos.par(gap.degree = 5)
circos.initialize(1:4, xlim = rbind(c(0, 1), c(0, 2), c(0, 3), c(0, 4)))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    circos.points(runif(20)*(xlim[2] - xlim[1])+xlim[1], runif(20), pch = 16)
})
circos.trackPlotRegion(ylim = c(0, 1))
circos.clear()

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "circos.points\ncircos.lines\ncircos.text\n...", adj = c(0.5, 0.5), cex = 1.2)

circos.par(gap.degree = 5)
circos.initialize(1:4, xlim = rbind(c(0, 1), c(0, 2), c(0, 3), c(0, 4)))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    circos.points(runif(20)*(xlim[2] - xlim[1])+xlim[1], runif(20), pch = 16)
})
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    circos.points(runif(20)*(xlim[2] - xlim[1])+xlim[1], runif(20), pch = 16)
})
circos.clear()

plot(c(0, 1), c(0, 1), type = "n", ann = FALSE, axes = FALSE)
text(0.5, 0.5, "...\ncircos.clear", adj = c(0.5, 0.5), cex = 1.2)

layout(rbind(1))
par(xpd = FALSE)

par(op)
