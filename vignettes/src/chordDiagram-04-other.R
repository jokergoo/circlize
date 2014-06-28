set.seed(123)

mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]
par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
chordDiagram(cor(mat), symmetric = TRUE, col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")), transparency = 0.5)
chordDiagram(mat, directional = TRUE, transparency = 0.5)
circos.clear()
