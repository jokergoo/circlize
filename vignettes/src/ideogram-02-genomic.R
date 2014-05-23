library(circlize)

circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 40)
circos.genomicPosTransformLines(bed, posTransform = posTransform.default)
circos.par("cell.padding" = c(0, 0, 0, 0), track.margin = c(0, 0))
circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), bg.border = NA, panel.fun = function(region, value, ...) {
	circos.genomicText(region, value, y = 1, adj = c(0, 0.5), labels = "gene", direction = "vertical_left", cex = 0.6, posTransform = posTransform.default, ...)
}, track.height = 0.1)
circos.par(track.margin = c(0.01, 0.01))

bed = generateRandomBed(nr = 500, fun = function(k) runif(k)*sample(c(-1, 1), k, replace = TRUE))
circos.genomicTrackPlotRegion(bed, ylim = c(-1, 1), panel.fun = function(region, value, ...) {
	col = ifelse(value[[1]] > 0, "red", "green")
	circos.genomicPoints(region, value, col = col, cex = 0.25, pch = 16)
	cell.xlim = get.cell.meta.data("cell.xlim")
	for(h in c(-1, -0.5, 0, 0.5, 1)) {
		circos.lines(cell.xlim, c(h, h), col = "#00000040")
	}
}, track.height = 0.1)

bed = generateRandomBed(nr = 500)
circos.genomicTrackPlotRegion(bed, ylim = c(-1, 1), panel.fun = function(region, value, ...) {
	l = value[[1]] > 0
}, track.height = 0.1)

circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, bg.col ="#EEEEEE",
    track.height = 0.1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        for(i in -2:2) {
            circos.lines(xlim, c(i, i)/2, col = "#999999", lwd = 0.2)
        }
        xrange = get.cell.meta.data("xrange")
        x = NULL
        y = NULL
        for(i in 1:5) {
            
            x2 = seq(xlim[1] + (i-1)/5*xrange, xlim[1] + (i)/5*xrange, by = 1000000)
            x = c(x, x2)
            y = c(y, runif(length(x2))^2*sample(c(-1, 1), 1))
        }
        col = ifelse(y > 0, "#E41A1C", "#4DAF4A")
        circos.points(x, y, col = col, cex = 0.2, pch = 16)
    })

circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, track.height = 0.1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        y = runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FF7F00")
        y = -runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FFFF33")
    })


circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        for(i in seq_along(x)) {
            if(i == 1) {
                next
            }
            circos.rect(x[i-1], 0, x[i], 1, col = rgb(runif(1), runif(1), runif(1)), 
                border = NA)
        }
    })

chromosome = paste("chr", c(1:22, "X", "Y"), sep = "")
for(i in 1:50) {
    chr = sample(chromosome, 2)
    xlim1 = get.cell.meta.data("xlim", sector.index = chr[1], track.index = 1)
    xrange1 = get.cell.meta.data("xrange", sector.index = chr[1], track.index = 1)
    xlim2 = get.cell.meta.data("xlim", sector.index = chr[2], track.index = 1)
    xrange2 = get.cell.meta.data("xrange", sector.index = chr[2], track.index = 1)
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x1 = c(xlim1[1] + r[1]*xrange1, xlim1[1] + r[2]*xrange1)
    } else {
        x1 = c(xlim1[1] + r[1]*xrange1)
    }
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x2 = c(xlim2[1] + r[1]*xrange2, xlim2[1] + r[2]*xrange2)
    } else {
        x2 = c(xlim2[1] + r[1]*xrange2)
    }
    
    
    circos.link(chr[1], x1, chr[2], x2, col = sample(c('#9E0142', '#D53E4F', '#F46D43',
          '#FDAE61', '#FEE08B', '#FFFFBF', '#E6F598',
          '#ABDDA4', '#66C2A5', '#3288BD', '#5E4FA2'), 1))
}

degree = get.cell.meta.data("xplot", sector.index = "chr1", track.index = 1)
start.degree = degree[1]
end.degree = degree[2]
rou1 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 1)[2]
rou2 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 5)[1]

draw.sector(center = c(0, 0), start.degree = start.degree, end.degree = end.degree,
            rou1 = rou1+0.05, rou2 = rou2-0.01, col = "#FF000020", border = NA)
circos.clear()