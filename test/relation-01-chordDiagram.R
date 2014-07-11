
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]



### basic settings
par(mfrow = c(3, 2))
chordDiagram(mat)
circos.clear()
circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat)
circos.clear()
circos.par(start.degree = 90)
chordDiagram(mat)
circos.clear()
chordDiagram(mat, directional = TRUE)
chordDiagram(mat, order = c("A", "B", "a", "C", "D", "b", "E", "F", "c"))

### colors settings
rand_color = function(n, alpha = 1) {
    return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
}

par(mfrow = c(3, 3))
grid.col = NULL
grid.col[letters[1:3]] = c("red", "green", "blue")
grid.col[LETTERS[1:6]] = "grey"
chordDiagram(mat, grid.col = grid.col)
chordDiagram(mat, grid.col = grid.col, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
col_mat = rand_color(length(mat), alpha = 0.5)
dim(col_mat) = dim(mat)
chordDiagram(mat, grid.col = grid.col, col = col_mat)
chordDiagram(mat, grid.col = grid.col, col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)), rev(heat.colors(11))))



### track settings
par(mfrow = c(2, 2))
chordDiagram(mat, grid.col = grid.col, annotationTrack = "grid")

chordDiagram(mat, annotationTrack = NULL,
	preAllocateTracks = list(track.height = 0.3))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	sector.name = get.cell.meta.data("sector.index")
	if(sector.name %in% rn) {
		label = paste0(rep(sector.name, 5), collapse="")
		circos.text(mean(xlim), ylim[1], label, facing = "bending", adj = c(0.5, 0))
	}
	if(sector.name %in% cn) {
		label = paste0(rep(sector.name, 5), collapse="")
		circos.text(mean(xlim), ylim[1], label, facing = "clockwise", adj = c(0, 0.5))
	}
}, bg.border = NA)

circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat, annotationTrack = "grid",
	preAllocateTracks = list(track.height = 0.1))
for(si in get.all.sector.index()) {
	circos.axis(h = "top", labels.cex = 0.5, sector.index = si, track.index = 2)
}
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	sector.name = get.cell.meta.data("sector.index")
	
	circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
	for(p in seq(0, 1, by = 0.25)) {
		circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim), p, cex = 0.4, adj = c(0.5, -0.2))
	}
	circos.text(mean(xlim), ylim[2], sector.name)
}, bg.border = NA)
circos.clear()

### correlation matrix and 
mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]
par(mfrow = c(2, 1))
chordDiagram(cor(mat), symmetric = TRUE, col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")), transparency = 0.5)
chordDiagram(mat, directional = TRUE)

