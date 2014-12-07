
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5, border = 1)
circos.link("b", 5, "d", c(4, 6), border = 1)
circos.link("a", c(2, 3), "f", c(4, 6), border = 1)

circos.clear()
text(0.9, 0.9, "A", cex = 1.5)

circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5, rou2 = 0.5, border = 1)
circos.link("b", 5, "d", c(4, 6), rou2 = 0.5, border = 1)
circos.link("a", c(2, 3), "f", c(4, 6), rou2 = 0.5, border = 1)

circos.clear()
text(0.9, 0.9, "B", cex = 1.5)


circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 10, "b", c(1, 9), border = 1);
circos.text(9, -8, "default `h`", adj = c(0, 0.5), sector.index = "a", facing = "downward") 
circos.link("c", 10, "d", c(1, 9), h = 0.5, h2 = 0.2, border = 1)
circos.text(1, -3, "h = 0.5\nh2 = 0.2", adj = c(0, 0.5), sector.index = "e", facing = "downward")
circos.clear()
text(0.9, 0.9, "C", cex = 1.5)


circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "b", 5); circos.text(10, -5, "default", facing = "outside", sector.index = "a")
circos.link("b", 5, "c", 5, h = 0.2); circos.text(10, -5, "h=0.2", facing = "outside", sector.index = "b")
circos.link("c", 5, "d", 5, h = 0.8); circos.text(10, -5, "h=0.8", facing = "outside", sector.index = "c")
circos.link("d", 5, "e", 5, w = 2); circos.text(10, -5, "w=2", facing = "downward", sector.index = "d")
circos.link("e", 5, "f", 5, w = -0.5); circos.text(10, 5, "w=-0.5", sector.index = "e")
circos.link("f", 5, "g", 5, w = 0.1, h = 0.3); circos.text(10, -5, "w=0.1\nh=0.3", sector.index = "f")
circos.clear()
text(0.9, 0.9, "D", cex = 1.5)

par(op)
