

df2GRanges = function(df, ...) {
	GRanges(seqnames = df[[1]],
	        ranges = IRanges(start = df[[2]],
			                 end = df[[3]]),
			mcols = df[, -(1:3), drop = FALSE],
			...)
}

random_bed = function(nr = 10000, nc = 1) {
	cyto = read.cytoband()
	chr.len = cyto$chr.len
	chromosome = cyto$chromosome
	dl = lapply(seq_along(chr.len), function(i) {
		k = round(nr*2 * chr.len[i] / sum(chr.len))
		k = ifelse(k %% 2, k + 1, k)
		breaks = sort(sample(chr.len[i], k))
		res = data.frame(chr = rep(chromosome[i], length(breaks)/2),
						  start = breaks[seq_along(breaks) %% 2 == 1],
						  start = breaks[seq_along(breaks) %% 2 == 0],
						  stringsAsFactors = FALSE)
		for(k in seq_len(nc)) {
			res = cbind(res, value = rnorm(length(breaks)/2, 0.5))
		}
		res
	})

	df = NULL
	for(i in seq_along(dl)) {
		df = rbind(df, dl[[i]])
	}
	return(df)
}


### test bed
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram()

bed = random_bed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed1 = random_bed(nr = 100)
bed2 = random_bed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicPoints(region, value, cex = cex, pch = 16, col = i,  ...)
})

bed = random_bed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	circos.genomicPoints(region, value, cex = 1, col = 1:4, ...)
})

bed = random_bed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, containMatrix = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicPoints(region, value, cex = cex, col = i, ...)
})

circos.clear()


# test GRanges
library(GenomicRanges)
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram()

bed = df2GRanges(random_bed(nr = 100))
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

bed1 = df2GRanges(random_bed(nr = 100))
bed2 = df2GRanges(random_bed(nr = 100))
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

bed = df2GRanges(random_bed(nr = 100, nc = 4))
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

bed = df2GRanges(random_bed(nr = 100, nc = 4))
circos.genomicTrackPlotRegion(bed, containMatrix = TRUE, panel.fun = function(region, value, ...) {
	circos.genomicPoints(region, value, ...)
})

circos.clear()


############ test lines
### test bed
circos.par("default.track.height" = 0.1)
circos.initializeWithIdeogram()

bed = random_bed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	circos.genomicLines(region, value, ...)
})

bed1 = random_bed(nr = 100)
bed2 = random_bed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicLines(region, value, col = i,  ...)
})

bed = random_bed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	circos.genomicLines(region, value, col = 1:4, ...)
})

bed = random_bed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, containMatrix = TRUE, panel.fun = function(region, value, ...) {
	cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
	i = getI(...)
	circos.genomicLines(region, value, col = i, ...)
})

circos.clear()
