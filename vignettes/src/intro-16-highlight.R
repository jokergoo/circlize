
op = par(no.readonly = TRUE)

library(circlize)

factors = letters[1:8]

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))

#start.a1 = get.cell.meta.data("cell.start.degree", "a", 1)
start.a1 = circlize(0.2, 1.2, "a", 1)[1, 1]
#end.a1 = get.cell.meta.data("cell.end.degree", "a", 1)
end.a1 = circlize(0.8, 1.2, "a", 1)[1, 1]
top.a1 = get.cell.meta.data("cell.top.radius", "a", 1)
draw.sector(start.degree = start.a1, end.degree = end.a1, rou1 = top.a1, border = NA, col = "#FF000040")

start.b2 = get.cell.meta.data("cell.start.degree", "b", 2)
end.b2 = get.cell.meta.data("cell.end.degree", "b", 2)
top.b2 = get.cell.meta.data("cell.top.radius", "b", 2)
draw.sector(start.degree = start.b2, end.degree = end.b2, rou1 = top.b2, border = NA, col = "#FF00FF40")

bottom.a1 = get.cell.meta.data("cell.bottom.radius", "a", 1)
draw.sector(start.degree = 0, end.degree = 360, rou1 = top.a1, rou2 = bottom.a1, border = NA, col = "#00FF0040")

start.c2 = get.cell.meta.data("cell.start.degree", "c", 2)
end.d2 = get.cell.meta.data("cell.end.degree", "d", 2)
top.c2 = get.cell.meta.data("cell.top.radius", "c", 2)
bottom.c2 = get.cell.meta.data("cell.bottom.radius", "c", 2)
draw.sector(start.degree = start.c2, end.degree = end.d2, rou1 = top.c2, rou2 = bottom.c2, border = NA, col = "#0000FF40")


start.g2 = get.cell.meta.data("cell.start.degree", "g", 2)
end.g2 = get.cell.meta.data("cell.end.degree", "g", 2)
top.g2 = get.cell.meta.data("cell.top.radius", "g", 2)
bottom.g3 = get.cell.meta.data("cell.bottom.radius", "g", 3)
draw.sector(start.degree = start.g2, end.degree = end.g2, rou1 = top.g2, rou2 = bottom.g3, border = NA, col = "#00FFFF40")


start.e2 = get.cell.meta.data("cell.start.degree", "e", 2)
end.f2 = get.cell.meta.data("cell.end.degree", "f", 2)
top.e2 = get.cell.meta.data("cell.top.radius", "e", 2)
bottom.e3 = get.cell.meta.data("cell.bottom.radius", "e", 3)
draw.sector(start.degree = start.e2, end.degree = end.f2, rou1 = top.e2, rou2 = bottom.e3, border = NA, col = "#FFFF0040")
circos.info(plot = TRUE)
circos.clear()

par(op)
