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


circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5, rou2 = 0.5, border = 1)
circos.link("b", 5, "d", c(4, 6), rou2 = 0.5, border = 1)
circos.link("a", c(2, 3), "f", c(4, 6), rou2 = 0.5, border = 1)

circos.clear()



circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 10, "b", c(1, 9), border = 1)
circos.link("c", 10, "d", c(1, 9), h = 0.5, h2 = 0.2, border = 1)

circos.clear()
