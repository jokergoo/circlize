
source("R/global.R")
source("R/plot.R")
source("R/utils.R")
# library(circlize)


################################################################################
# a ugly figure, but contains everthing the package can support
################################################################################

require(RColorBrewer)


n = 10000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))
for(le in levels(a$factor)) {
    a$x[a$factor == le] = a$x[a$factor == le] * runif(1)
}

par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1, "clock.wise" = TRUE)
circos.initialize(factors = a$factor, x = a$x)

bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
col = rep(c("#FF000010", "#00FF0010"), 4)
circos.trackPlotRegion(factors = a$factor, y = a$y, track.index = 1, panel.fun = function(x, y) {
	circos.axis()
})
circos.trackPoints(a$factor, a$x, a$y, track.index = 1, col = col, pch = 16, cex = 0.5)
circos.text(-1,0.5, "left", sector.index = "a", track.index = 1)
circos.text(1,0.5, "right", sector.index = "a", track.index = 1)

circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)

circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y, panel.fun = function(x, y, ...) {
    grey = c("#FFFFFF", "#CCCCCC", "#999999")
    circos.updatePlotRegion(bg.col = grey[get.cell.meta.data("sector.numeric.index") %% 3 + 1])
    circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
    circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
})

circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = runif(100), y = runif(100))

circos.trackPlotRegion(factors = a$factor, y = a$y)
show.index()
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")

circos.link("a", 0, "b", 0, top.ratio = 0.9)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red", border = "blue", top.ratio = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)

circos.clear()

################################################################################
#  taiji and bagua
################################################################################


factors = letters[1:8]
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.15, "start.degree" = 22.5)
circos.initialize(factors = factors, xlim = c(0, 1))

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(1, 3, 5, 6)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y, ...) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(1, 2, 5, 8)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
    panel.fun = function(x, y, ...) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(1, 4, 5, 8)) {
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
	if( (end - start) >= 360 || (end - start) %% 360 == 0) {
		
	} else {
		m = rbind(m, c(0, 0))
	}
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


##################################################################################
#  dartboard
##################################################################################


factors = 1:20
par(mar = c(1, 1, 1, 1))
circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), start.degree = 360/40, track.margin = c(0, 0), "clock.wise" = FALSE)
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
    i = get.cell.meta.data("sector.numeric.index")
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


#####################################
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
	circos.text(5, 9, "default_default", direction = "default")
	#circos.points(5, 9, pch = 16, col = "red") 
	circos.text(0, 5, "vertical_left", direction = "vertical_left")
	#circos.points(0, 5, pch = 16, col = "red") 
	circos.text(10, 5, "vertical_right", direction = "vertical_right")
	#circos.points(10, 5, pch = 16, col = "red") 
	circos.text(5, 5, "horizontal", direction = "horizontal")
	#circos.points(5, 5, pch = 16, col = "red") 
	circos.text(5, 1, "arc_arc_arc_arc_arc", direction = "arc")
	#circos.points(5, 1, pch = 16, col = "red") 
})
circos.clear()

##########################################

par(mar = c(1, 1, 1, 1))
factors = letters[1:7]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5)
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
circos.text(5, 9, "type = 'l'", sector.index = "a")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
circos.text(5, 9, "type = 'o'", sector.index = "b")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
circos.text(5, 9, "type = 'h'", sector.index = "c")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "s")
circos.text(5, 9, "type = 's'", sector.index = "d")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", area = TRUE)
circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "e")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f", type = "o", area = TRUE)
circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "f")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g", type = "s", area = TRUE)
circos.text(5, 9, "type = 's', area = TRUE", sector.index = "g")
circos.clear()

#########################################
par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")

circos.clear()


#######################################
par(mar = c(1, 1, 1, 1), "xaxs" = "i", "yaxs" = "i")
factors = letters[1:8]
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 3, "start.degree" = 20, "track.margin" = c(0.05, 0.05), "clock.wise" = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))

circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
	cell.data = get.cell.data()
	cell.xlim = get.cell.meta.data("xlim")
	cell.ylim = get.cell.meta.data("ylim")
	circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], col = "#377EB8", border = "black", lwd = 2)
	circos.rect(cell.data$xlim[1], cell.data$ylim[2], cell.data$xlim[2], cell.data$ylim[2]+(cell.data$ylim[2]-cell.data$ylim[1])/2, col = "#984EA3", border = NA)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1]-(cell.data$ylim[2]-cell.data$ylim[1])/2, cell.data$xlim[2], cell.data$ylim[1], col = "#984EA3", border = NA)
	circos.lines(0:10, runif(11)*10)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1], cell.data$xlim[2], cell.data$ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.3, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
	
	cell.data = get.cell.data()
	cell.xlim = get.cell.meta.data("xlim")
	cell.ylim = get.cell.meta.data("ylim")
	circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], col = "#377EB8", border = "black", lwd = 2)
	circos.rect(cell.data$xlim[1], cell.data$ylim[2], cell.data$xlim[2], cell.data$ylim[2]+(cell.data$ylim[2]-cell.data$ylim[1])/6, col = "#984EA3", border = NA)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1]-(cell.data$ylim[2]-cell.data$ylim[1])/6, cell.data$xlim[2], cell.data$ylim[1], col = "#984EA3", border = NA)
	circos.lines(0:10, runif(11)*10)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1], cell.data$xlim[2], cell.data$ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
	cell.data = get.cell.data()
	cell.xlim = get.cell.meta.data("xlim")
	cell.ylim = get.cell.meta.data("ylim")
	circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], col = "#377EB8", border = "black", lwd = 2)
	circos.rect(cell.data$xlim[1], cell.data$ylim[2], cell.data$xlim[2], cell.data$ylim[2]+(cell.data$ylim[2]-cell.data$ylim[1])/2, col = "#984EA3", border = NA)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1]-(cell.data$ylim[2]-cell.data$ylim[1])/2, cell.data$xlim[2], cell.data$ylim[1], col = "#984EA3", border = NA)
	circos.lines(0:10, runif(11)*10)
	circos.rect(cell.data$xlim[1], cell.data$ylim[1], cell.data$xlim[2], cell.data$ylim[2], lwd = 2, lty = 2)
})

x = seq(0, 1, length = 1000)
y = sqrt(1^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.8, length = 1000)
y = sqrt(0.8^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.4, length = 1000)
y = sqrt(0.4^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.2, length = 1000)
y = sqrt(0.2^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

draw.sector(x = 0, y = 0, start = 20, end = 23, radius = 1, col = "#4DAF4A")
draw.sector(x = 0, y = 0, start = 65, end = 68, radius = 1, col = "#4DAF4A")
draw.sector(x = 0, y = 0, start = 0, end = 90, radius = 0.2, col = "#FFFFFF", border = NA)

circos.text(5, 5, "plotting region", sector.index = "a", track.index = 2)
circos.text(5, 10.5, "cell.padding[3]", sector.index = "a", track.index = 2)
circos.text(5, -0.5, "cell.padding[1]", sector.index = "a", track.index = 2)
circos.text(-0.5, 5, "cell.padding[2]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(10.5, 5, "cell.padding[4]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(5, -2, "track.margin[1]", sector.index = "a", track.index = 2)
circos.text(5, 12, "track.margin[2]", sector.index = "a", track.index = 2)
circos.text(-1.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(11.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.clear()

############################################################

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))


circos.clear()

##########################################################
plot(c(-1, 1), c(-1, 1), axes = FALSE, ann = FALSE ,type = "n")
draw.sector(x = 0, y = 0, start = 0, end = 360, radius = 01, col = "white", border = "black")
d= rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 1, rou.ratio = 0.5)
lines(rbind(d, d[1, ]))
lines(c(cos(300/180*pi), cos(120/180*pi)), c(sin(300/180*pi), sin(120/180*pi)))
points(0, 0, pch = 16)
lines(c(0, sqrt(3)/4)+0.01, c(0, -3/4)+0.01, lwd = 4, col = "red")
lines(c(0, sqrt(3)/4/2)-0.01, c(0, -3/4/2)-0.01, lwd = 4, col = "blue")

############################################################
pdf(file = "transformation.pdf", height = 8.5, width = 4)
layout(cbind(c(1, 0, 2, 0, 3)), height = c(2,0.5,2, 0.5, 4))
par(mar = c(2, 2, 2, 2))
x = 1:10
y = rnorm(10)
plot(x, y, type = "l", axes = FALSE, ann = FALSE)
text(2, 0, "text", cex = 2)
rect(5, -1, 7, 1)
box()

par(mar = c(1, 1, 1, 1))
factors = letters[1:3]
circos.par("canvas.xlim" = c(-sqrt(3)/2, sqrt(3)/2), "canvas.ylim" = c(1/2*0.6, 1), start.degree = 30, "track.margin" = c(0, 0), "gap.degree" = 0, "clock.wise" = FALSE)
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "a", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.clear()

par(xpd = NA)
arrows(0, 1.33, 0, 1.07, code = 2)

factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4)
circos.updatePlotRegion(sector.index = "c", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "c", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.clear()
arrows(0, 1.5, 0, 1.07, code = 2)

dev.off()


#########################################
pdf("sector_direction.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par("track.margin" = c(0.1, 0.1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
	circos.text(5, 5, get.cell.meta.data("sector.index"))
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	circos.lines(xlim, c(0, 0))
	circos.lines(c(9, 10), c(0.5, 0))
	circos.lines(c(9, 10), c(-0.5, 0))
	circos.lines(c(0, 0), xlim)
	circos.lines(c(0.5, 0), c(9, 10))
	circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])

text(0, 0, 'circos.par("clock.wise" = FALSE)', cex = 0.6)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("track.margin" = c(0.1, 0.1), clock.wise = TRUE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
	circos.text(5, 5, get.cell.meta.data("sector.index"))
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	circos.lines(xlim, c(0, 0))
	circos.lines(c(9, 10), c(0.5, 0))
	circos.lines(c(9, 10), c(-0.5, 0))
	circos.lines(c(0, 0), xlim)
	circos.lines(c(0.5, 0), c(9, 10))
	circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])


x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])
text(0, 0, 'circos.par("clock.wise" = TRUE)', cex = 0.6)
dev.off()


##################################################
par(mar = c(1, 1, 1, 1))
factors = 1
circos.par("track.margin" = c(0, 0), "cell.padding" = c(0, 0, 0, 0), "gap.degree" = 0, "gap.degree" = 0)
circos.initialize(factors = factors, xlim = c(0, 10))
for(i in 1:50) {
	circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.04, bg.border = NA, panel.fun = function(x, y) {
		xlim = get.cell.meta.data("xlim")
		ylim = get.cell.meta.data("ylim")
		d = runif(10)
		d = cumsum(d)/sum(d) * xlim[2]

		#circos.lines(c(0, 0), c(0,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			circos.lines(c(0, d[1]), c(10,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			circos.lines(c(d[1], d[1]), c(0,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			circos.lines(c(0, d[1]), c(0,0), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
		for(i in 2:10) {
			
			circos.lines(c(d[i-1], d[i-1]), c(0,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			circos.lines(c(d[i-1], d[i]), c(10,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			if(i < 10) {
				circos.lines(c(d[i], d[i]), c(0,10), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
			}
			circos.lines(c(d[i-1], d[i]), c(0,0), col = rgb(runif(1), runif(1), runif(1)), lwd = 2)
		}
	})
}
circos.clear()


#####################################################
par(mar = c(1, 1, 1, 1))
factors = factor(letters[1:10], levels = sample(letters[1:10], 10))
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
for(l in letters[1:10]) {
	circos.rect(0,0,10,10,sector.index = l, track.index = 2, col = "#FF000040")
}

for(l in 1:4) {
	circos.rect(0,0,10,10,sector.index = "a", track.index = l, col = "#0000FF40")
}
show.index()
circos.clear()

	
##########################################
source("R/global.R")
source("R/plot.R")
source("R/utils.R")
# library(circlize)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, panel.fun = function(x, y) {
	circos.text(5, 10, get.cell.meta.data("sector.index"))
})

circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.axis(sector.index = "a")
circos.axis(sector.index = "b", direction = "inside")
circos.axis(sector.index = "c", h = "bottom")
circos.axis(sector.index = "d", h = "bottom", direction = "inside")
circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick = FALSE)
circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick.percentage = 0.3, labels.away.percentage = 0.2, minor.ticks = 2, labels.direction = "vertical_right")
circos.clear()

#############################################
# clock 2

source("R/global.R")
source("R/plot.R")
source("R/utils.R")
# library(circlize)

factors = letters[1]
par(mar = c(1, 1, 1, 1))
circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = -90)
circos.initialize(factors = factors, xlim = c(0, 12))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.axis(sector.index = "a", major.at = 0:12, labels = "", direction = "inside", labels.cex = 1.5, major.tick.percentage = 0.3)
circos.text(1:12, 0.5, 1:12, direction = "horizontal")
arrows(0, 0, 0, 0.7)    
arrows(0, 0, 0.4, 0)

circos.clear()


##################################
circos.initializeWithIdeogram(file = "inst/extdata/cytoBand.txt")
