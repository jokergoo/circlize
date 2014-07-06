library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))

circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("b", 1, "b", c(2, 8), top.ratio = 0.5)
circos.link("c", 1, "c", c(2, 8), top.ratio = 0.5, top.ratio.low = 0.8)

circos.clear()
