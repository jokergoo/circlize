
par(mar = c(1, 1, 1, 1))

circos.initializeWithIdeogram()
circos.clear()

df = read.cytoband()$df
circos.initializeWithIdeogram(df)
circos.clear()

df[[1]] = qsub("chr", "", df[[1]])
circos.initializeWithIdeogram(df)
circos.clear()

circos.initializeWithIdeogram(species = "hg19")
circos.clear()

df[[1]] = factor(df[[1]], levels = unique(df[[1]]))
circos.initializeWithIdeogram(df)
circos.clear()

circos.initializeWithIdeogram(sort.chr = FALSE)
circos.clear()

circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))
circos.clear()

circos.initializeWithIdeogram(plotType = c("axis", "labels"))
circos.clear()

########################
# general genomic initialize

df = read.cytoband()$df
circos.genomicInitialize(df)

df = data.frame(name = c("TP53", "TP63", "TP73"),
	             start = c(7565097, 189349205, 3569084),
	             end = c(7590856, 189615068, 3652765),
	             stringsAsFactors = FALSE)
circos.genomicInitialize(df)
circos.clear()

circos.genomicInitialize(df, major.by = 10000)
circos.clear()

circos.genomicInitialize(df, plotType = "labels")
circos.clear()

circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))
circos.clear()

circos.genomicInitialize(df, sector.names = c("tp53x", "tp63x", "tp73"))
circos.clear()

df[[1]] = factor(df[[1]], levels = c("TP73", "TP63", "TP53"))
circos.genomicInitialize(df)
circos.clear()


df = data.frame(name = c("TP53", "TP63", "TP73"),
	start = c(7565097, 189349205, 3569084),
	end = c(7590856, 189615068, 3652765),
	stringsAsFactors = FALSE)
par(mar = c(1, 1, 1, 1))
circos.genomicInitialize(df, major.by = 10000)
circos.clear()

circos.genomicInitialize(df, major.by = 20000)
circos.clear()

circos.genomicInitialize(df)
circos.clear()

circos.genomicInitialize(df, tickLabelsStartFromZero=FALSE)
circos.clear()

circos.genomicInitialize(df, tickLabelsStartFromZero=FALSE, major.by = 20000)
circos.clear()
