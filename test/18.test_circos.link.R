source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")
source("R/link-bezier.R")




par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link2("a", 5, "c", 5, col = "#00000040")
circos.link2("a", 5, "g", 5, col = "#00000040")
circos.link2("c", 10, "d", c(1, 4), col = "#00000040")
circos.link2("a", c(2, 8), "g", c(4, 4.5), height = 0.9, rou1 = 0.9, rou2 = 0.8, col = "#00000040")
circos.link2("b", c(1, 10), "f", c(1, 10), col = "#00000040")

circos.clear()


par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0.2, 0))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("c", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "e", c(4, 6))

circos.link("a", c(-10, 10), "f", c(-10, 10))

circos.clear()



par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0, 0.2))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("c", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "e", c(4, 6))

circos.link("a", c(-10, 10), "f", c(-10, 10))

circos.clear()