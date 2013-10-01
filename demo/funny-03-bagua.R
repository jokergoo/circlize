library(circlize)
op = par(no.readonly = FALSE)
factors = letters[1:8]
par(mar = c(1, 1, 1, 1))
circos.par("default.track.height" = 0.15, "start.degree" = 22.5, "gap.degree" = 6)
circos.initialize(factors = factors, xlim = c(0, 1))

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(2, 5, 7, 8)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y, ...) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(1, 6, 7, 8)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })

circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
    panel.fun = function(x, y, ...) {
        i = get.cell.meta.data("sector.numeric.index")
        if(i %in% c(4, 5, 6, 7)) {
            circos.rect(0,0,1,1, col = "black")
        } else {
            circos.rect(0,0,0.45,1, col = "black")
            circos.rect(0.55,0,1,1, col = "black")
        }
    })
    
# draw taiji
draw.sector(center = c(0, 0), start.degree = -90, end.degree = 90,
    rou1 = 0.4, col = "black", border = "black")
draw.sector(center = c(0, 0), start.degree = 90, end.degree = 270,
    rou1 = 0.4, col = "white", border = "black")
draw.sector(center = c(0, 0.2), start.degree = 0, end.degree = 360,
    rou1 = 0.2, col = "white", border = "white")
draw.sector(center = c(0, -0.2), start.degree = 0, end.degree = 360,
    rou1 = 0.2, col = "black", border = "black")
draw.sector(center = c(0, 0.2), start.degree = 0, end.degree = 360,
    rou1 = 0.05, col = "black", border = "black")
draw.sector(center = c(0, -0.2), start.degree = 0, end.degree = 360,
    rou1 = 0.05, col = "white", border = "white")

circos.clear()
par(op)