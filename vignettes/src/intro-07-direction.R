
op = par(no.readonly = TRUE)

par(mfrow = c(1, 2))
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("track.margin" = c(0.1, 0.1), "clock.wise" = FALSE, start.degree = 30,
    "gap.degree" = rep(c(2, 10), 4), "cell.padding" = c(0.05, 3, 0.05, 3))
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])

text(0, 0, 'circos.par("clock.wise" = FALSE,\nstart.degree = 30)', cex = 0.6)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("track.margin" = c(0.1, 0.1), "clock.wise" = TRUE, start.degree = -30,
    "gap.degree" = rep(c(2, 10), 4), "cell.padding" = c(0.05, 3, 0.05, 3))
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])


x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])
text(0, 0, 'circos.par("clock.wise" = TRUE,\nstart.degree = -30)', cex = 0.6)

par(op)
