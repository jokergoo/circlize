
# things needed to be tested
# 1. the order
# 2. if the sum of sizes are larger than xlim


make_plot = function(pos1, pos2, range) {
	oxpd = par("xpd")
	par(xpd = NA)
	plot(NULL, xlim = c(0, 4), ylim = range, ann = FALSE)
	col = rand_color(nrow(pos1), transparency = 0.5)
	rect(0.5, pos1[, 1], 1.5, pos1[, 2], col = col)
	rect(2.5, pos2[, 1], 3.5, pos2[, 2], col = col)
	segments(1.5, rowMeans(pos1), 2.5, rowMeans(pos2))
	par(xpd = oxpd)
}

range = c(0, 10)
pos1 = rbind(c(1, 2), c(5, 6))
make_plot(pos1, smartAlign(pos1[, 1], pos1[, 2], xlim = range), range)

range = c(0, 10)
pos1 = rbind(c(-0.5, 2), c(5, 6))
make_plot(pos1, smartAlign(pos1[, 1], pos1[, 2], xlim = range), range)


pos1 = rbind(c(1, 2), c(3, 4), c(5, 6), c(7, 8))
par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for(i in 1:9) {
	ind = sample(4, 4)
	make_plot(pos1[ind, ], smartAlign(pos1[ind, 1], pos1[ind, 2], xlim = range), range)
}
par(mfrow = c(1, 1))

pos1 = rbind(c(3, 6), c(4, 7))
make_plot(pos1, smartAlign(pos1[, 1], pos1[, 2], xlim = range), range)

pos1 = rbind(c(1, 8), c(3, 10))
make_plot(pos1, smartAlign(pos1[, 1], pos1[, 2], xlim = range), range)

pos1 = cbind(c(-0.0005832292,  0.2559116024,  0.6293272145,  0.9496636073),
	         c(0.3105832, 0.4940884, 0.7906728, 1.0303364))
make_plot(pos1, smartAlign(pos1[, 1], pos1[, 2], xlim = c(0, 1)), c(0, 1))



pos = cbind(c(-0.01287589, 0.02396249, 0.12195961, 0.35193075, 0.55257434, 0.63590767,
               0.68991344, 0.74391921, 0.79792498, 0.83726697, 0.87660896, 0.91595095, 0.95779655),
            c( 0.04220345, 0.08404905, 0.18204616, 0.41201731, 0.61266090, 0.69599423,
               0.75000000, 0.80400577, 0.85801154, 0.89735353, 0.93669552, 0.97603751, 1.01287589))
make_plot(pos, smartAlign(pos[, 1], pos[, 2], xlim = c(0, 1)), c(0, 1))

pos = cbind(c(-1.27780262, -0.04374432,  4.62284175, 15.68043329, 28.20910817), 
             c(2.683642,  8.821311, 11.526453, 22.584044, 32.170553))
make_plot(pos, smartAlign(pos[, 1], pos[, 2], xlim = c(-1.3, 33)), c(-1.3, 33))

