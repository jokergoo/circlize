
op = par(no.readonly = TRUE)

library(circlize)
par(mfrow = c(2, 1), mar = c(1, 1, 1, 1))
mat1 = matrix(sample(20, 25, replace = TRUE), 5)

gap.degree = c(rep(2, 4), 10, rep(2, 4), 10)
circos.par(gap.degree = gap.degree, start.degree = -5)
chordDiagram(mat1, directional = TRUE, grid.col = rep(1:5, 2),
    transparency = 0.5)
for(si in get.all.sector.index()) {
	circos.axis(labels.cex = 0.5, sector.index = si, track.index = 2)
}
circos.clear()

mat2 = mat1/2
percent = sum(mat2)/sum(mat1)
blank_degree = (360 - sum(gap.degree)) * (1-percent)

gap.degree = c(rep(2, 4), blank_degree/2+10, rep(2, 4), blank_degree/2+10)
circos.par(gap.degree = gap.degree, start.degree = -blank_degree/4 - 5)
chordDiagram(mat2, directional = TRUE, grid.col = rep(1:5, 2),
    transparency = 0.5)
for(si in get.all.sector.index()) {
	circos.axis(labels.cex = 0.5, sector.index = si, track.index = 2)
}
circos.clear()

par(op)
