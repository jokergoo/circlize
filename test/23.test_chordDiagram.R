 
##################################################
## test chordDiagram

par(mar = c(1, 1, 1, 1))
mat = matrix(rnorm(100), 10)
rownames(mat) = letters[1:10]
colnames(mat) = letters[1:10]
chordDiagram(mat)
chordDiagram(mat, symmetric = TRUE)
chordDiagram(cor(mat), symmetric = TRUE)
chordDiagram(cor(mat), symmetric = TRUE, col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))

mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = LETTERS[1:3]
colnames(mat) = letters[1:6]
chordDiagram(mat)
circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat)
circos.clear()

chordDiagram(mat, annotationTrack = "grid")
chordDiagram(mat, directional = TRUE)

chordDiagram(mat, order = c("a", "b", "A", "c", "d", "B", "e", "f", "C"))

chordDiagram(mat, row.col = 1:3)
chordDiagram(mat, column.col = 1:6)

chordDiagram(mat, annotationTrack = "grid", transparency = 0,
	preAllocateTracks = list(track.height = 0.05))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
	xlim = get.cell.meta.data("xlim")
	ylim = get.cell.meta.data("ylim")
	sector.name = get.cell.meta.data("sector.index")
	sector.name = paste0(rep(sector.name, 8), collapse = "")
	circos.text(mean(xlim), mean(ylim), sector.name, direction = "arc")
}, bg.border = NA)
	
chordDiagram(mat, annotationTrack = "grid", transparency = 0,
	preAllocateTracks = list(ylim = c(0, 2),
	                         track.height = 0.1))

chordDiagram(mat, annotationTrack = "grid", transparency = 0,
	preAllocateTracks = list(list(ylim = c(0, 2),
	                              track.height = 0.1),
							 list(ylim = c(0, 4),
	                              track.height = 0.05)))		

pdf("test.pdf")
par(mar = c(1, 1, 1, 1))
chordDiagram(mat, directional = TRUE)
dev.off()
