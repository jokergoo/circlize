

op = par(no.readonly = TRUE)

library(circlize)
mat = matrix(rnorm(100*10), nrow = 10, ncol = 100)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "black", "red"))
factors = rep(letters[1:2], 50)
par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(factors, xlim = c(0, 50))
maxy = 0
circos.trackPlotRegion(ylim = c(0, 10), bg.border = NA, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
    m = mat[, factors == sector.index]
    
    dend.col = as.dendrogram(hclust(dist(t(m))))

    maxy = ifelse(maxy > attr(dend.col, "height"), maxy, attr(dend.col, "height"))
    assign("maxy", maxy, envir = .GlobalEnv)

    m2 = m[, labels(dend.col)]
	col_mat = col_fun(m2)
    nr = nrow(m2)
    nc = ncol(m2)
    for(i in 1:nr) {
        for(j in 1:nc) {
            circos.rect(j-1, nr-i, j, nr-i+1, border = col_mat[i, j], col = col_mat[i, j])
        }
    }
    
})
circos.trackPlotRegion(ylim = c(0, maxy), bg.border = NA, track.height = 0.3, panel.fun = function(x, y) {
    sector.index = get.cell.meta.data("sector.index")
    m = mat[, factors == sector.index]
    
    dend.col = as.dendrogram(hclust(dist(t(m))))

    circos.dendrogram(dend.col, max_height = maxy)
    
})
circos.clear()


par(op)
