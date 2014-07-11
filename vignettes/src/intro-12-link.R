library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))

circos.clear()

par(mar = c(1, 1, 1, 1))
plot(c(-1, 1), c(-1, 1), axes = FALSE, ann = FALSE ,type = "n")
draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360, rou1 = 1, col = "white", border = "black")
d= circlize:::rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 1, rou.ratio = 0.5)
lines(rbind(d, d[1, ]))
lines(c(cos(300/180*pi), cos(120/180*pi)), c(sin(300/180*pi), sin(120/180*pi)))
points(0, 0, pch = 16)
lines(c(0, sqrt(3)/4)+0.01, c(0, -3/4)+0.01, lwd = 4, col = "red")
lines(c(0, sqrt(3)/4/2)-0.01, c(0, -3/4/2)-0.01, lwd = 4, col = "blue")


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
draw.sector(start.degree = theta1, end.degree = theta2, rou1 = rou1, rou2 = rou2, col = "red", border = "red")
circos.clear()
