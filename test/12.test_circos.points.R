source("R/plot.R")
source("R/utils.R")
source("R/global.R")
source("R/link.R")

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPoints(factors = sample(1:4, 10, replace = TRUE), x = runif(10), y = runif(10))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPoints(factors = sample(1:2, 10, replace = TRUE), x = runif(10), y = runif(10))
circos.clear()
