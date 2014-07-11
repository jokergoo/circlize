
library(circlize)

set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]

rn = rownames(mat)
cn = colnames(mat)

factors = c(letters[1:3], LETTERS[1:6])
factors = factor(factors, levels = factors)

col_sum = apply(mat, 2, sum)
row_sum = apply(mat, 1, sum)
xlim = cbind(rep(0, 9), c(row_sum, col_sum))

par(mar = c(1, 1, 1, 1))
circos.clear()
circos.par(cell.padding = c(0, 0, 0, 0),
    gap.degree = c(2, 2, 10, 2, 2, 2, 2, 2, 10), start.degree = 5)
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, 
    bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05, 
    panel.fun = function(x, y) {
		sector.name = get.cell.meta.data("sector.index")
		xlim = get.cell.meta.data("xlim")
		circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))

		if(sector.name %in% rn) {
			for(i in seq_len(ncol(mat))) {
				circos.lines(rep(sum(mat[sector.name, seq_len(i)]), 2), c(0, 1), 
                    col = "white")
			}
		} else if(sector.name %in% cn) {
			for(i in seq_len(nrow(mat))) {
				circos.lines(rep(sum(mat[ seq_len(i), sector.name]), 2), c(0, 1), 
                   col = "white")
			}
		}
	})


col = c("#FF000020", "#00FF0020", "#0000FF20")
for(i in seq_len(nrow(mat))) {
	for(j in rev(seq_len(ncol(mat)))) {
		circos.link(rn[i], c(sum(mat[i, seq_len(j-1)]), sum(mat[i, seq_len(j)])),
                    cn[j], c(sum(mat[seq_len(i-1), j]), sum(mat[seq_len(i), j])), 
                    col = col[i], border = "white")
	}
}

circos.clear()
