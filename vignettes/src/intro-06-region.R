
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1), "xaxs" = "i", "yaxs" = "i")
factors = letters[1:8]
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 3, "start.degree" = 20, 
"track.margin" = c(0.05, 0.05), "cell.padding" = c(0.03, 2, 0.03, 2), "clock.wise" = FALSE,
points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))

circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 1)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.3, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 1)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/6, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/6, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 1)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2)
})

x = seq(0, 1, length = 1000)
y = sqrt(1^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.8, length = 1000)
y = sqrt(0.8^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.4, length = 1000)
y = sqrt(0.4^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.2, length = 1000)
y = sqrt(0.2^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

draw.sector(center = c(0, 0), start = 17, end = 20, rou1 = 1, rou2 = 0.2, col = "#4DAF4A", clock.wise = FALSE)
draw.sector(center = c(0, 0), start = 62, end = 65, rou1 = 1, rou2 = 0.2, col = "#4DAF4A", clock.wise = FALSE)

circos.text(5, 5, "plotting region", sector.index = "a", track.index = 2)
circos.text(5, 10.5, "cell.padding[3]", sector.index = "a", track.index = 2)
circos.text(5, -0.5, "cell.padding[1]", sector.index = "a", track.index = 2)
circos.text(-0.3, 5, "cell.padding[2]", facing = "clockwise", sector.index = "a", track.index = 2)
circos.text(10.1, 5, "cell.padding[4]", facing = "clockwise", sector.index = "a", track.index = 2)
circos.text(5, -2, "track.margin[1]", sector.index = "a", track.index = 2)
circos.text(5, 12, "track.margin[2]", sector.index = "a", track.index = 2)
circos.text(-1.2, 5, "gap.degree", facing = "clockwise", sector.index = "a", track.index = 2)
circos.text(10.8, 5, "gap.degree", facing = "clockwise", sector.index = "a", track.index = 2)
circos.clear()

par(op)
