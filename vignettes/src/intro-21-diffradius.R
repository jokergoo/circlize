
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
lim = c(1, 1.1, 1.2, 1.3)
for(i in 1:4) {
    circos.par("canvas.xlim" = c(-lim[i], lim[i]), "canvas.ylim" = c(-lim[i], lim[i]), "track.height" = 0.4)
    circos.initialize(factors = factors, xlim = c(0, 1))
    circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
    circos.updatePlotRegion(sector.index = factors[i], bg.border = "black")
    circos.points(runif(10), runif(10), pch = 16)
    circos.clear()
    par(new = TRUE)
}
par(new = FALSE)

par(op)
