library(circlize)
factors = sample(letters[1:6], 100, replace = TRUE)
x = rnorm(100)
y = rnorm(100)

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, x = x)
circos.trackPlotRegion(factors = factors, x = x, y = y, bg.col = "#EEEEEE",
    bg.border = NA, track.height = 0.4, panel.fun = function(x, y) {
    
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    # reference lines
    for(xi in seq(cell.xlim[1], cell.xlim[2], length.out = 10)) {
        circos.lines(c(xi, xi), cell.ylim, lty = 2, col = "white") 
    }
    for(yi in seq(cell.ylim[1], cell.ylim[2], length.out = 5)) {
        circos.lines(cell.xlim, c(yi, yi), lty = 2, col = "white") 
    }
    
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.rect(xlim[1], 1, xlim[2], ylim[2], col = "#FF000020", border = NA)
    circos.rect(xlim[1], ylim[1], xlim[2], -1, col = "#00FF0020", border = NA)

    circos.points(x[y >= 1], y[y >= 1], pch = 16, cex = 0.8, col = "red")
    circos.points(x[y <= -1], y[y <= -1], pch = 16, cex = 0.8, col = "green")
    circos.points(x[y > -1 & y < 1], y[y > -1 & y < 1], pch = 16, cex = 0.5)
})

circos.clear()