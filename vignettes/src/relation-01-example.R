
par(mfrow = c(1, 2))

set.seed(12345)
n = 9
m = matrix(rnorm(n^2), n, n)
colnames(m) = letters[1:n]
m2 = cor(m)
xlim = cbind(rep(0, n), apply(m2, 2, function(x) sum(abs(x)) - 1))

library(circlize)
factors = rownames(m2)
colors = 1:n

############################################
# first figure
par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        current.sector.index = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), 0.75, labels = current.sector.index,
            direction = "horizontal")
        i = get.cell.meta.data("sector.numeric.index")
        circos.rect(min(xlim), 0, max(xlim), 0.25, col = colors[i])
    })

	
rn = rownames(m2)
sector.sum = numeric(length(rn))
for(i in 2:n) {
    for(j in 1:(i-1)) {
        sector.index1 = rn[i]
        sector.index2 = rn[j]
        circos.link(sector.index1, c(sector.sum[i],sector.sum[i] + abs(m2[i, j])),
            sector.index2, c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
            col = ifelse(m2[i, j] > 0, "#FF0000A0", "#00FF00A0"), border = "grey")
        sector.sum[i] = sector.sum[i] + abs(m2[i, j])
        sector.sum[j] = sector.sum[j] + abs(m2[i, j])
    }
}

circos.clear()


#############################################
# second figure
colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    current.sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0.75, labels = current.sector.index, direction = "horizontal")
    i = get.cell.meta.data("sector.numeric.index")
    circos.rect(min(xlim), 0, max(xlim), 0.25, col = colors[i])
})

rn = rownames(m2)
sector.sum = numeric(length(rn))
col_fun = colorRamp2(breaks = seq(-1, 1, length=5), col = c("#1A9641", "#A6D96A", "#FFFFBF", "#FDAE61", "#D7191C"))
for(i in 2:n) {
    for(j in 1:(i-1)) {
        sector.index1 = rn[i]
        sector.index2 = rn[j]
        circos.link(sector.index1, c(sector.sum[i], sector.sum[i] + abs(m2[i, j])),
                    sector.index2, c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
                    col = col_fun(m2[i, j]), 
                    border = "grey", lwd = 0.5)
        sector.sum[i] = sector.sum[i] + abs(m2[i, j])
        sector.sum[j] = sector.sum[j] + abs(m2[i, j])
    }
}

circos.clear()