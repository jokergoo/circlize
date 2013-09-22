library(circlize)

par(bg = "black", mar = c(1, 1, 1, 1))

factors = 1:36
circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0, 0, 0), gap.degree = 0)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.11, bg.border = NA,
    panel.fun = function(x, y) {
        cell.xlim = get.cell.meta.data("cell.xlim")
        cell.ylim = get.cell.meta.data("cell.ylim")
        circos.lines(cell.xlim, c(cell.ylim[1], cell.ylim[1]), col="#FFFFFF")
        circos.lines(cell.xlim, c(cell.ylim[2], cell.ylim[2]), col="#FFFFFF")
    })

for(i in 1:5) {
    circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.13, bg.border = "#FFFFFF",
        panel.fun = function(x, y) {
            cell.xlim = get.cell.meta.data("cell.xlim")
            cell.ylim = get.cell.meta.data("cell.ylim")
            for(j in 1:4) {
                x = cell.xlim[1] + (cell.xlim[2]-cell.xlim[1])/5*j
                circos.lines(c(x, x), cell.ylim, col = "#FFFFFF", lwd = 0.3)
            }
            for(j in 1:9) {
                y = cell.ylim[1] + (cell.ylim[2]-cell.ylim[1])/10*j
                circos.lines(cell.xlim, c(y, y), col = "#FFFFFF", lwd = 0.3)
            }
        })
}

track.pos = get.cell.meta.data("cell.bottom.radius")
r = c(1.5, 2, 2, 1, 2, 8)
col = c("#B99450", "#7D3342", "black", "#37465D", "black", "#762F2D")

for(i in seq_along(r)) {
    r1 = 1 - sum(r[seq_len(i-1)])/sum(r)
    r2 = 1 - sum(r[seq_len(i)])/sum(r)
    draw.sector(rou1 = track.pos*r1, rou2 = track.pos*r2,col = col[i], border = col[i])
}

text(0, 1.02, "cz.IiII", adj = c(0.5, 0), col = "#FFFFFF")

circos.clear()
