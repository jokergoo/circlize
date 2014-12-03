
op = par(no.readonly = TRUE)

library(circlize)
factor = sample(letters[1:8], 100, replace = TRUE)
x = rnorm(100)
range = tapply(x, factor(factor), function(x) max(x) - min(x))
y = rnorm(100)

zoom = factor[factor %in% c("a", "b")]
zoom[zoom == "a"] = "a_zoom"
zoom[zoom == "b"] = "b_zoom"
zoom.x = x[factor %in% c("a", "b")]
zoom.y = y[factor %in% c("a", "b")]
factor2 = c(factor, zoom)
factor2 = factor(factor2, levels = c(letters[1:8], "a_zoom", "b_zoom"))
x2 = c(x, zoom.x)
y2 = c(y, zoom.y)

sector.width = c(range/sum(range), range[1:2]/sum(range[1:2]))
par(mar = c(1, 1, 1, 1))
circos.par(start.degree = 90)
circos.initialize(factors = factor2, x = x2, sector.width = sector.width)
circos.trackPlotRegion(factors = factor2, x = x2, y = y2, panel.fun = function(x, y) {
    circos.points(x, y, col = "orange", pch = 16)
})
circos.link("a", get.cell.meta.data("cell.xlim", "a", 1), "a_zoom", get.cell.meta.data("cell.xlim", "a_zoom", 1), col = "#0000FF20", border = NA)
circos.link("b", get.cell.meta.data("cell.xlim", "b", 1), "b_zoom", get.cell.meta.data("cell.xlim", "b_zoom", 1), col = "#FF000020", border = NA)
circos.clear()

par(op)
