
op = par(no.readonly = TRUE)

library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(1, 2))
fa = c("d", "f", "e", "c", "g", "b", "a")
f1 = factor(fa)
circos.initialize(factors = f1, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.info(plot = TRUE)
text(0, 0, "factor(fa)", adj = c(0.5, 0.5), cex = 1.3)
circos.clear()

f2 = factor(fa, levels = fa)
circos.initialize(factors = f2, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.info(plot = TRUE)
text(0, 0, "factor(fa, levels = fa)", adj = c(0.5, 0.5), cex = 1.3)
circos.clear()

par(op)
