library(circlize)

mat = matrix(rnorm(36), 6, 6)
rownames(mat) = paste0("R", 1:6)
colnames(mat) = paste0("C", 1:6)
mat[2, ] = 1e-10
mat[, 3] = 1e-10

par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
chordDiagram(mat)
chordDiagram(mat, row.col = rep(c("red", "blue"), 3))
chordDiagram(mat, grid.col = rep(c("red", "blue"), 6))

circos.par("gap.degree" = rep(c(2, 10), 6))
chordDiagram(mat)
circos.clear()
