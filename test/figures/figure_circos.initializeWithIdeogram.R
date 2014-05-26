pdf("circos.initializeWithIdeogram_example.pdf", width = 8, height = 8)

par(mfrow = c(3, 3))
par(mar = c(1, 1, 1, 1))

circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "default")

circos.initializeWithIdeogram(chromosome.index = paste("chr", 1:10, sep = ""))
circos.clear()
text(0, 0, "subset of chromosomes")

cytoband.file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep="")
cytoband = read.table(cytoband.file , colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
circos.clear()
text(0, 0, "read from cytoband file")

cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
circos.clear()
text(0, 0, "read from cytoband file\nfirst column converted to factor\nlevels = paste0('chr', c(22:1, 'X', 'Y'))")

cytoband = read.table(cytoband.file , colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)
circos.clear()
text(0, 0, "read from cytoband file\nsort.chr = TRUE")

circos.initializeWithIdeogram(plotType = c("axis", "labels"))
circos.clear()
text(0, 0, "plotType = c('axis', 'labels')")

circos.initializeWithIdeogram(plotType = NULL)
circos.clear()
text(0, 0, "plotType = NULL")

circos.par("start.degree" = 90)
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'start.degree' = 90")

circos.par("gap.degree" = rep(c(2, 4), 12))
circos.initializeWithIdeogram()
circos.clear()
text(0, 0, "'gap.degree' = rep(c(2, 4), 12)")

dev.off()


pdf("genomicInitialize_TP.pdf", width = 8, height = 8)
par(mar = c(1, 1, 1 ,1))
load("gencode_TP_gene.RData")
df = data.frame( names(gencode),
                 sapply(gencode, function(x) x$start ),
				 sapply(gencode, function(x) x$end ) )
circos.genomicInitialize(df, sector.name = sapply(gencode, function(x) x$name))
n = max(sapply(gencode, function(x) length(x$transcript)))

circos.genomicTrackPlotRegion(ylim = c(0, 1), bg.col = c("#FF000040", "#00FF0040", "#0000FF40"), bg.border = NA, track.height = 0.05)

circos.genomicTrackPlotRegion(ylim = c(0.5, n + 0.5), panel.fun = function(region, value, ...) {
	gi = get.cell.meta.data("sector.index")
	tr = gencode[[gi]]$transcript
	for(i in seq_along(tr)) {
		region = data.frame(sapply(tr[[i]]$exon, function(x) x$start),
		                    sapply(tr[[i]]$exon, function(x) x$end))
		circos.lines(c(tr[[i]]$start, tr[[i]]$end), c(n-i, n-i), col = "#CCCCCC")
		circos.genomicRect(region, ytop = n-i+0.4, ybottom = n-i-0.4, col = "orange", border = NA)
	}
}, bg.border = NA, track.height = 0.3)
circos.clear()

dev.off()


