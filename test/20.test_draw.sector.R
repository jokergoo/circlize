source("R/plot.R")
source("R/utils.R")
source("R/global.R")
source("R/link.R")

###############################
#  test draw.sector
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", axes = FALSE, ann = FALSE)
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5, col = "red", border = "black")
draw.sector(c(0, 0), start = 70, end = 180, rou1 = 0.8, col = "blue", border = NA)
draw.sector(c(0, 0), start = 220, end = 270, rou1 = 0.4, rou2 = 0.6, col = "yellow", border = "black")

draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3, col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2, col = "green", border = "black")


#########################

factors = letters[1:8]

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))

xplot.a1 = get.cell.meta.data("xplot", "a", 1)
yplot.a1 = get.cell.meta.data("yplot", "a", 1)
draw.sector(start.degree = xplot.a1[1], end.degree = xplot.a1[2], rou1 = yplot.a1[2], border = NA, col = "#FF000040")

xplot.b2 = get.cell.meta.data("xplot", "b", 2)
yplot.b2 = get.cell.meta.data("yplot", "b", 2)
draw.sector(start.degree = xplot.b2[1], end.degree = xplot.b2[2], rou1 = yplot.b2[2], border = NA, col = "#FF00FF40")

draw.sector(start.degree = 0, end.degree = 360, rou1 = yplot.a1[2], rou2 = yplot.a1[1], border = NA, col = "#00FF0040")

xplot.c2 = get.cell.meta.data("xplot", "c", 2)
yplot.c2 = get.cell.meta.data("yplot", "c", 2)
xplot.d2 = get.cell.meta.data("xplot", "d", 3)
yplot.d2 = get.cell.meta.data("yplot", "d", 3)
draw.sector(start.degree = xplot.c2[1], end.degree = xplot.d2[2], rou1 = yplot.c2[2], rou2 = yplot.c2[1], border = NA, col = "#0000FF40")

xplot.g2 = get.cell.meta.data("xplot", "g", 2)
yplot.g2 = get.cell.meta.data("yplot", "g", 2)
xplot.g3 = get.cell.meta.data("xplot", "g", 3)
yplot.g3 = get.cell.meta.data("yplot", "g", 3)
draw.sector(start.degree = xplot.g2[1], end.degree = xplot.g2[2], rou1 = yplot.g2[2], rou2 = yplot.g3[1], border = NA, col = "#00FFFF40")

xplot.e2 = get.cell.meta.data("xplot", "e", 2)
yplot.e2 = get.cell.meta.data("yplot", "e", 2)
xplot.e3 = get.cell.meta.data("xplot", "e", 3)
yplot.e3 = get.cell.meta.data("yplot", "e", 3)
xplot.f2 = get.cell.meta.data("xplot", "f", 2)
yplot.f2 = get.cell.meta.data("yplot", "f", 2)
xplot.f3 = get.cell.meta.data("xplot", "f", 3)
yplot.f3 = get.cell.meta.data("yplot", "f", 3)
draw.sector(start.degree = xplot.e2[1], end.degree = xplot.f2[2], rou1 = yplot.e2[2], rou2 = yplot.e3[1], border = NA, col = "#FFFF0040")

circos.clear()

