op = par(no.readonly = TRUE)
library(circlize)

layout(matrix(1:9, 3, 3, byrow = TRUE))
par(mar = c(1, 1, 1, 1))

circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "default", cex = 0.7)

circos.initializeWithIdeogram(chromosome.index = paste("chr", 1:10, sep = ""))
circos.clear()
text(0, 0, "subset of chromosomes", cex = 0.7)

cytoband.file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep="")
cytoband = read.table(cytoband.file , colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
circos.clear()
text(0, 0, "read from cytoband file", cex = 0.7)

cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
circos.clear()
text(0, 0, "read from cytoband file\nfirst column converted to factor\nlevels = paste0('chr', c(22:1, 'X', 'Y'))", cex = 0.7)

cytoband = read.table(cytoband.file , colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)
circos.clear()
text(0, 0, "read from cytoband file\nsort.chr = TRUE", cex = 0.7)

circos.initializeWithIdeogram(plotType = c("axis", "labels"))
circos.clear()
text(0, 0, "plotType = c('axis', 'labels')", cex = 0.7)

circos.initializeWithIdeogram(plotType = NULL)
circos.clear()
text(0, 0, "plotType = NULL", cex = 0.7)

circos.par("start.degree" = 90)
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'start.degree' = 90", cex = 0.7)

circos.par("gap.degree" = rep(c(2, 4), 12))
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'gap.degree' = rep(c(2, 4), 12)", cex = 0.7)

par(op)
