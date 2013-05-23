source("R/global.R")
source("R/plot.R")
source("R/utils.R")

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y, ...) {
	circos.text(0.5, 0.5, labels = "abcdefghijklmnopqrstuvwxyz", direction = "arc", adj = c(0.5, 0.5), cex = 1.4)
	circos.points(0.5, 0.5, pch = 16, col = "red")
})

circos.clear()

library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:2]
for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0))
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
        circos.lines(c(0,1), c(0.1,0.1), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
        circos.lines(c(1,0), c(0.9,0.9), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}

for (theta in seq(-360, 360, by = 30)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), clock.wise=FALSE)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
                circos.lines(c(0,1), c(0.1,0.1), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
                circos.lines(c(1,0), c(0.9,0.9), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}


###########################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1]
for (theta in seq(-360, 360, by = 60)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0))
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
        circos.lines(c(0,1), c(0.1,0.1), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
        circos.lines(c(1,0), c(0.9,0.9), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}

for (theta in seq(-360, 360, by = 60)) {
	circos.par(start.degree = theta, cell.padding=c(0,0,0,0), clock.wise=FALSE)
	circos.initialize(factors = factors, xlim = c(0, 1))
	circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
		y = runif(20)
		circos.lines(1:20/20, y, col = "red")
                circos.lines(c(0,1), c(0.1,0.1), col="green")
		
		y = runif(20)
		circos.lines(20:1/20, y, col = "blue")
                circos.lines(c(1,0), c(0.9,0.9), col="yellow")
	})
	show.index()
	circos.clear()
	Sys.sleep(2)
}


#############
# test histgrame
library(circlize)
par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")

circos.clear()


############
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
############

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y) {
	circos.rect(0, 0, 1, 1, col = "red")
})
circos.par("cell.padding" = c(0.1, 0.1, 0.1, 0.1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y) {
	circos.rect(0, 0, 1, 1, col = "blue")
})
circos.par("cell.padding" = c(0.4, 0.4, 0.4, 0.4))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y) {
	circos.rect(0, 0, 1, 1, col = "green")
})
circos.par("cell.padding" = c(0.8, 0.8, 0.8, 0.8))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, panel.fun = function(x, y) {
	circos.rect(0, 0, 1, 1, col = "green")
})
circos.clear()

################################################################

factors = 1:4

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 5), factors = sample(1:4, 100, replace = TRUE), x = runif(100), y = runif(100), panel.fun = function(x, y) {
	circos.points(x, y)
})


###############################
#  test draw.sector
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5, col = "red", border = "black")
draw.sector(c(0, 0), start = 70, end = 180, rou1 = 0.8, col = "blue", border = NA)
draw.sector(c(0, 0), start = 220, end = 270, rou1 = 0.4, rou2 = 0.6, col = "yellow", border = "black")

draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3, col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2, col = "green", border = "black")

