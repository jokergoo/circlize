source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")

# circos.par
circos.par()
circos.par("gap.degree")
circos.par("gap.degree", "cell.padding")
circos.par(c("gap.degree", "cell.padding"))
circos.par("gap.degree", "start.degree")
circos.par(c("gap.degree", "start.degree"))
circos.par("gap.degree" = 10)
circos.par("gap.degree")
circos.par("gap.degree" = 20, "cell.padding" = c(0, 0, 0, 0))
circos.par("gap.degree")
circos.par("cell.padding")
circos.par("gap.degree" = -10)
circos.par("gap.degree" = 380)

circos.par("gap.degree" = c(1, 2, 3, 4 ,5))
get(".CIRCOS.PAR", envir = .CIRCOS.ENV)
circos.par("gap.degree")

circos.par("aa")
circos.par("aa" = 1)
circos.par("gap.degree" = 20, "aa" = 1)

# warnings
circos.initialize(factors = 1:10, xlim = c(0,1))
circos.par("gap.degree" = 10)
circos.par("gap.degree")
circos.par("start.degree" = 10)
circos.par("start.degree")

circos.clear()
###########################
# track.margin
factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.par(default.track.height = 0.1)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.par("track.margin" = c(0.1, 0.2))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.par("track.margin" = c(0.01, 0.01))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()


############################
# cell.padding
factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.par("cell.padding" = c(0.4, 40, 0, 0))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y) {
	circos.rect(0, 0, 1, 1, col = "red")
}, track.height = 0.6)
circos.clear()

#############################
# gap.degree
factors = 1:8

par(mar = c(1, 1, 1, 1))
circos.par(gap.degree = 5)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = rep(5, 2))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = rep(5, 10))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = rep(5, 8))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = 1:8*2)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = 1:8*2, clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = sample(1:8, 8)*2)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = sample(1:8, 8)*2, start.degree = 90)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = sample(1:8, 8)*2, clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()

circos.par(gap.degree = sample(1:8, 8)*2, start.degree = 90, clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors)
circos.clear()