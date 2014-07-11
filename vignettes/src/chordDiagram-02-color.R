set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]


### colors settings
rand_color = function(n, alpha = 1) {
    return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
}

par(mfrow = c(3, 3))
par(mar = c(1, 1, 1, 1))
grid.col = NULL
grid.col[letters[1:3]] = c("red", "green", "blue")
grid.col[LETTERS[1:6]] = "grey"
chordDiagram(mat, grid.col = grid.col)
chordDiagram(mat, grid.col = grid.col, transparency = 0.5)
col_mat = rand_color(length(mat), alpha = 0.5)
dim(col_mat) = dim(mat)
chordDiagram(mat, grid.col = grid.col, col = col_mat)
chordDiagram(mat, grid.col = grid.col, col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)), rev(heat.colors(11))), transparency = 0.5)

chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
circos.clear()
