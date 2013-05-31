source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")




par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("c", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "e", c(4, 6))

circos.link("a", c(-10, 10), "f", c(-10, 10))

circos.clear()

