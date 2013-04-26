
source("R/global.R")
source("R/plot.R")
source("R/utils.R")
# library(circlize)


################################################################################
# a ugly figure, but contains everthing the package can support
################################################################################

require(RColorBrewer)

col2 = brewer.pal(8, "Set1")
n = 10000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))
for(le in levels(a$factor)) {
    a$x[a$factor == le] = a$x[a$factor == le] * runif(1)
}

par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.15)
circos.initialize(a$x, a$factor)

bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
col = rep(c("#FF000010", "#00FF0010"), 4)
circos.trackPlotRegion(y = a$y, factors = a$factor, track.index = 1)
circos.trackPoints(a$x, a$y, a$factor, track.index = 1, col = col, pch = 16, cex = 0.5)
circos.trackHist(a$x, a$factor, bg.col = bgcol, col = NA)

circos.trackPlotRegion(x = a$x, y = a$y, factors = a$factor, panel.fun = function(x, y, ...) {
    grey = c("#FFFFFF", "#CCCCCC", "#999999")
    circos.updatePlotRegion(bg.col = grey[get.sector.numeric.index() %% 3 + 1])
    circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
    circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
})

circos.updatePlotRegion(sector.index = "d", track.index = 3)
circos.points(x = runif(100), y = runif(100))

circos.trackPlotRegion(y = a$y, factors = a$factor)
show.index()
circos.trackLines(a$x[1:100], a$y[1:100], a$factor[1:100], type = "h")

circos.link("a", 0, "b", 0, top.ratio = 0.9)
circos.link("c", c(-0.2, 0.2), "d", c(-0.2,0.2), col = "red", border = "blue", top.ratio = 0.2)
circos.link("e", 0, "g", c(-0.2,0.2), col = "green", lwd = 2, lty = 2)

circos.clear()

################################################################################
#  taiji and bagua
################################################################################

m = rbind(c(1, 1, 1, 1, 2, 2, 2, 2),
          c(1, 1, 2, 2, 1, 1, 2, 2),
          c(1, 2, 1, 2, 1, 2, 1, 2))
m = t(m)
factors = apply(m, 1, paste, sep = "", collapse = "")
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.15)
circos.initialize(factors = factors, xlim = c(0, 1))

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y) {
        i = get.sector.numeric.index()
        if(i <= 4) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y, ...) {
        i = get.sector.numeric.index()
        if(i %in% c(1,2,5,6)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
    panel.fun = function(x, y, ...) {
        i = get.sector.numeric.index()
        if(i %in% c(1,3,5,7)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })


# by default, draw a filled circle
draw.sector = function (x, y, start=0, end=360, radius, col="black", border = "black") {
    d = NULL
    for (i in 1:500) {
        d = rbind(d, c(start + abs(end - start)/500*i, radius))
    }
    
    m = polar2Cartesian(d)
    m[, 1] = m[, 1] + x
    m[, 2] = m[, 2] + y
    polygon(m, col = col, border = border)
}

# draw taiji
draw.sector(x = 0, y = 0, start = -90, end = 90, radius = 0.4, col = "black", border = "black")
draw.sector(x = 0, y = 0, start = 90, end = 270, radius = 0.4, col = "white", border = "black")
draw.sector(x = 0, y = 0.2, start = 0, end = 360, radius = 0.2, col = "white", border = "white")
draw.sector(x = 0, y = -0.2, start = 0, end = 360, radius = 0.2, col = "black", border = "black")
draw.sector(x = 0, y = 0.2, start = 0, end = 360, radius = 0.05, col = "black", border = "black")
draw.sector(x = 0, y = -0.2, start = 0, end = 360, radius = 0.05, col = "white", border = "white")

circos.clear()

################################################################################
#  clock
################################################################################


factors = 1:12
par(mar = c(1, 1, 1, 1))
circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
    panel.fun = function(x, y, ...) {
        circos.lines(c(0, 1), c(1, 1))
        circos.lines(c(1, 1), c(1, 0.5))
        circos.lines(c(0.8, 0.8), c(1, 0.75))
        circos.lines(c(0.6, 0.6), c(1, 0.75))
        circos.lines(c(0.4, 0.4), c(1, 0.75))
        circos.lines(c(0.2, 0.2), c(1, 0.75))
        
        i = get.sector.numeric.index()
        i = (12 - i + 3) %% 12
        i = ifelse(i == 0, 12, i)
        circos.text(1, 0.3, labels = i, direction = "horizontal")
    })
arrows(0, 0, 0, 0.7)    
arrows(0, 0, 0.4, 0)

circos.clear()

##################################################################################
#  dartboard
##################################################################################


factors = 1:20
par(mar = c(1, 1, 1, 1))
circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), start.degree = 360/40, track.margin = c(0, 0))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.col = "black", track.height = 0.15)
circos.trackText(rep(0.5, 20), rep(0.5, 20), labels = c(13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10, 6), factors = factors, col = "#EEEEEE", font = 2, direction = "horizontal")
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.col = rep(c("#E41A1C", "#4DAF4A"), 10), bg.border = "#EEEEEE", track.height = 0.05)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.col = rep(c("black", "white"), 10), bg.border = "#EEEEEE", track.height = 0.275)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.col = rep(c("#E41A1C", "#4DAF4A"), 10), bg.border = "#EEEEEE", track.height = 0.05)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.col = rep(c("black", "white"), 10), bg.border = "#EEEEEE", track.height = 0.375)
draw.sector(x = 0, y = 0, start = 0, end = 360, radius = 0.1, col = "#4DAF4A", border = "#EEEEEE")
draw.sector(x = 0, y = 0, start = 0, end = 360, radius = 0.05, col = "#E41A1C", border = "#EEEEEE")

circos.clear()


##################################################################################
# correlations
##################################################################################

n = 9
m = matrix(rnorm(n^2), n, n)
colnames(m) = letters[1:n]
m2 = cor(m)
factors = rownames(m2)

xlim = cbind(rep(0, n), apply(m2, 2, function(x) sum(abs(x)) - 1))

colors = brewer.pal(n, "Set1")

par(mar = c(1, 1, 1, 1))
circos.par("start.degree" = 20, "cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, panel.fun = function(x, y, ...) {
    current.sector.index = get.current.sector.index()
    current.track.index = get.current.track.index()
    current.cell.data = get.cell.data(current.sector.index, current.track.index)
    circos.text(mean(current.cell.data$xlim), 0.75, labels = current.sector.index, direction = "horizontal")
    i = get.sector.numeric.index()
    circos.rect(min(current.cell.data$xlim), 0, max(current.cell.data$xlim), 0.25, col = colors[i])
})

rn = rownames(m2)
sector.sum = numeric(length(rn))
for(i in 2:n) {
    for(j in 1:(i-1)) {
        sector.index1 = rn[i]
        sector.index2 = rn[j]
        circos.link(sector.index1,
                    c(sector.sum[i], sector.sum[i] + abs(m2[i, j])),
                    sector.index2,
                    c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
                    col = ifelse(m2[i, j] > 0, "#E41A1CA0", "#4DAF4AA0"), border = "grey")
        sector.sum[i] = sector.sum[i] + abs(m2[i, j])
        sector.sum[j] = sector.sum[j] + abs(m2[i, j])
    }
}

circos.clear()
