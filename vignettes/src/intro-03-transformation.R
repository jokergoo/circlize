
op = par(no.readonly = TRUE)

library(circlize)
layout(cbind(c(1, 0, 2, 0, 3)), height = c(1,0.25,1, 0.25, 2))
par(mar = c(2, 2, 2, 2))
x = 1:10
y = runif(10)*10
plot(x, y, xlim = c(0, 10), ylim = c(0, 10), type = "l", axes = FALSE, ann = FALSE)
text(2, 0, "text", cex = 2)
rect(5, 2, 7, 8)
box()
axis(side = 1)

par(mar = c(1, 1, 1, 1))
factors = letters[1:3]
circos.par("canvas.xlim" = c(-sqrt(3)/2, sqrt(3)/2), "canvas.ylim" = c(1/2*0.6, 1), start.degree = 30, "track.margin" = c(0, 0), "gap.degree" = 0, "clock.wise" = FALSE, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "a", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, 2, 7, 8)
circos.axis(h = "bottom", major.at = seq(0, 10, by = 2))
circos.clear()

par(xpd = NA)
arrows(0, 1.33, 0, 1.07, code = 2)

par(mar = c(3, 3, 3, 3))
factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4)
circos.updatePlotRegion(sector.index = "c", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "c", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, 2, 7, 8)
circos.axis(h = "bottom", major.at = seq(0, 10, by = 2))
circos.clear()
box()
axis(side = 1)
axis(side = 2)
arrows(0, 2, 0, 1.3, code = 2)

par(op)
