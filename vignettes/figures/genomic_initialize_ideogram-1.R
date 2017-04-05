pdf("genomic_initialize_ideogram-1.pdf", width = 20, height = 20)

par(mfrow = c(3, 3))
circos.initializeWithIdeogram()
text(0, 0, "default", cex = 0.7)
text(-0.9, 0.9, "A", cex = 1.5)

circos.initializeWithIdeogram(chromosome.index = paste0("chr", 10:1))
text(0, 0, "subset of chromosomes", cex = 0.7)
text(-0.9, 0.9, "B", cex = 1.5)

cytoband = cytoband.df
circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)
text(0, 0, "read from cytoband df\nsort.chr = TRUE", cex = 0.7)
text(-0.9, 0.9, "C", cex = 1.5)

cytoband = cytoband.df
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
text(0, 0, "read from a data frame\nunique(cytoband[[1]])", cex = 0.7)
text(-0.9, 0.9, "D", cex = 1.5)

cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
text(0, 0, "read from cytoband file\nfirst column converted to factor\nlevels = paste0('chr', c(22:1, 'X', 'Y'))", cex = 0.7)
text(-0.9, 0.9, "E", cex = 1.5)

circos.initializeWithIdeogram(plotType = c("axis", "labels"))
text(0, 0, "plotType = c('axis', 'labels')", cex = 0.7)
text(-0.9, 0.9, "F", cex = 1.5)
circos.initializeWithIdeogram(plotType = NULL)
text(0, 0, "plotType = NULL", cex = 0.7)
text(-0.9, 0.9, "G", cex = 1.5)

circos.clear()

circos.par("start.degree" = 90)
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'start.degree' = 90", cex = 0.7)
text(-0.9, 0.9, "H", cex = 1.5)

circos.par("gap.degree" = rep(c(2, 4), 12))
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'gap.degree' = rep(c(2, 4), 12)", cex = 0.7)
text(-0.9, 0.9, "I", cex = 1.5)

dev.off()
