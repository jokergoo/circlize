
op = par(no.readonly = TRUE)

set.seed(12345)
n = 1000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
    x = rnorm(n), y = runif(n))
library(circlize)
par(mar = c(1, 1, 1, 1), lwd = 0.1)
layout(rbind(1:2, 3:4, 5:6))

######################################
circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
circos.clear()

###########################################

circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.clear()

###########################################
circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
  panel.fun = function(x, y) {
      grey = c("#FFFFFF", "#CCCCCC", "#999999")
      i = get.cell.meta.data("sector.numeric.index")
      circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
      circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
      circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
  })
circos.clear()

##############################################
circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
  panel.fun = function(x, y) {
      grey = c("#FFFFFF", "#CCCCCC", "#999999")
      i = get.cell.meta.data("sector.numeric.index")
      circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
      circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
      circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
  })
circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = -2:2, y = rep(0, 5))
circos.clear()

################################################

circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
  panel.fun = function(x, y) {
      grey = c("#FFFFFF", "#CCCCCC", "#999999")
      i = get.cell.meta.data("sector.numeric.index")
      circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
      circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
      circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
  })
circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = -2:2, y = rep(0, 5))
circos.trackPlotRegion(factors = a$factor, y = a$y)
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")
circos.clear()

###################################################

circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
  panel.fun = function(x, y) {
      grey = c("#FFFFFF", "#CCCCCC", "#999999")
      i = get.cell.meta.data("sector.numeric.index")
      circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
      circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
      circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
  })
circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = -2:2, y = rep(0, 5))
circos.trackPlotRegion(factors = a$factor, y = a$y)
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")
circos.link("a", 0, "b", 0, h = 0.4)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
    border = "blue", h = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)
circos.clear()

layout(rbind(1))

par(op)
