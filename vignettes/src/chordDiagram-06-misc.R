library(circlize)

mat = matrix(rnorm(36), 6, 6)
rownames(mat) = paste0("R", 1:6)
colnames(mat) = paste0("C", 1:6)
mat[2, ] = 1e-10
mat[, 3] = 1e-10

par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
chordDiagram(mat)
chordDiagram(mat, row.col = c("red", "blue", "green", "yellow", "orange", "pink"))

grid.col = 1:12
names(grid.col) = c(rownames(mat), colnames(mat))
chordDiagram(mat, grid.col = grid.col)
