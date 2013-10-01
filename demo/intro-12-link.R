library(circlize)
op = par(no.readonly = FALSE)
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0.1, 0))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", c(2, 3), "f", c(4, 6), border = "black")

# the following codes calculate the position for the 'little rectangle'
cell.ylim = get.cell.meta.data("cell.ylim", "a")
d1 = circlize(2, cell.ylim[1], "a")
theta1 = d1[1, 1]
rou1 = d1[1, 2]

d2 = circlize(3, cell.ylim[1], "a")
theta2 = d2[1, 1]
rou2 = d2[1, 2] - circos.par("track.margin")[1]

# draw the 'little rectangle'
draw.sector(start.degree = theta1, end.degree = theta2, rou1 = rou1, rou2 = rou2, col = "black", border = "black")
circos.clear()
par(op)
