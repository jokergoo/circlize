
source("R/global.R")
source("R/plot.R")
source("R/utils.R")





circos.par("cell.padding" = c(0, 0, 0, 0), gap.degree = 0)
circos.initialize(factors = "a", xlim = c(0,1))
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.4)
lines.expand(c(0,1), c(0,1), sector.index = "a", track.index = 1)
circos.lines(c(0,1), c(1,0), sector.index = "a", track.index = 1, col=1)
lines.expand(c(1,0), c(1,0), sector.index = "a", track.index = 1)
circos.lines(c(1,0), c(1,0), sector.index = "a", track.index = 1, col =2)
lines.expand(0.5, 0.5, sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501), c(0.499,0.501), sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1)
circos.lines(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1, col = 3, lwd = 2)
circos.clear()

circos.par("cell.padding" = c(0, 0, 0, 0), gap.degree = 0, start.degree = 90)
circos.initialize(factors = "a", xlim = c(0,1))
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.4)
lines.expand(c(0,1), c(0,1), sector.index = "a", track.index = 1)
circos.lines(c(0,1), c(1,0), sector.index = "a", track.index = 1, col=1)
lines.expand(c(1,0), c(1,0), sector.index = "a", track.index = 1)
circos.lines(c(1,0), c(1,0), sector.index = "a", track.index = 1, col =2)
lines.expand(0.5, 0.5, sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501), c(0.499,0.501), sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1)
circos.lines(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1, col = 3)
circos.clear()

circos.par("cell.padding" = c(0, 0, 0, 0), gap.degree = 0, start.degree = -90)
circos.initialize(factors = "a", xlim = c(0,1))
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.4)
lines.expand(c(0,1), c(0,1), sector.index = "a", track.index = 1)
circos.lines(c(0,1), c(1,0), sector.index = "a", track.index = 1, col=1)
lines.expand(c(1,0), c(1,0), sector.index = "a", track.index = 1)
circos.lines(c(1,0), c(1,0), sector.index = "a", track.index = 1, col =2)
lines.expand(0.5, 0.5, sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501), c(0.499,0.501), sector.index = "a", track.index = 1)
lines.expand(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1)
circos.lines(c(0.499,0.501, 1), c(0.499,0.501, 0.8), sector.index = "a", track.index = 1, col = 3)
circos.clear()




par(mar = c(1, 1, 1, 1), lwd = 2)
factors = letters[1:2]
for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), gap.degree = 0)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(21)
		circos.lines(0:20/20, y, col = "red")
        circos.lines(c(0,1), c(0,1), col="green")
		
		y = runif(21)
		circos.lines(20:0/20, y, col = "blue")
        circos.lines(c(0,1), c(1,0), col="orange")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}

for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), clock.wise=FALSE, gap.degree = 0)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(21)
		circos.lines(0:20/20, y, col = "red")
        circos.lines(c(0,1), c(0,1), col="green")
		
		y = runif(21)
		circos.lines(20:0/20, y, col = "blue")
        circos.lines(c(0,1), c(1,0), col="orange")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}


par(mar = c(1, 1, 1, 1))
factors = letters[1]
for (theta in seq(-360, 360, by = 60)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), gap.degree = 0)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(21)
		circos.lines(0:20/20, y, col = "red")
        circos.lines(c(0,1), c(0,1), col="green")
		
		y = runif(21)
		circos.lines(20:0/20, y, col = "blue")
        circos.lines(c(0,1), c(1,0), col="orange")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}

for (theta in seq(-360, 360, by = 60)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), clock.wise=FALSE, gap.degree = 0)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(21)
		circos.lines(0:20/20, y, col = "red")
                circos.lines(c(0,1), c(0,1), col="green")
		
		y = runif(21)
		circos.lines(20:0/20, y, col = "blue")
                circos.lines(c(0,1), c(1,0), col="orange")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}



#######################
# test one sector
factors = 1

par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 0)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.lines(c(0, 1), c(0, 1), col = "red")
circos.clear()

par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 0, start.degree = 90)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.lines(c(0, 1), c(0, 1))
circos.clear()

par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 0, start.degree = -90)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.lines(c(0, 1), c(0, 1))
circos.clear()
