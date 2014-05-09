
# == title
# Initialize the circos layout with an ideogram
#
# == param
# -file             cytoband file. By default it is the cytoband data for human
# -species abbrevations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically.
# -chromosome.index index for chromosome. The index is used only for subsetting, not for re-ordering.
#                   The value should be 1, 2, ... or chr1, chr2, ...
# -track.height     height for the track
# -major.by         increment of major ticks
#
# == details
# This is not a full functional function. It just provides a way to show how to
# draw genomics ideogram by this package. How to embed the ideogram into the
# circos layout is really subjective and should be applied according to specific situation.
#
# In fact, drawing ideogram with this package is really simple, you can look at the source code
# of this function to get a clue.
circos.initializeWithIdeogram = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), species = NULL,
    chromosome.index = NULL, track.height = 0.1, major.by = 50000000) {
	
	cytoband = read.cytoband(file, species = species)
	d = cytoband$df
	chromosome = cytoband$chromosome
	
	if(! is.null(chromosome.index)) {
		chromosome.index = gsub("chr", "", chromosome.index)
		chromosome.index = paste("chr", chromosome.index, sep = "")
		chromosome = chromosome[chromosome %in% chromosome.index]
	}
	
	xlim = matrix(nrow = 0, ncol = 2)
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
	}
	
	par(mar = c(1, 1, 1, 1), lwd = 0.5)
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
	circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
	circos.trackPlotRegion(factors = chromosome, ylim = c(0, 1), bg.border = NA, track.height = track.height)
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		n = nrow(d2)
		col = cytoband.col(d2[[5]])
		for(i in seq_len(n)) {
			circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr, col = col[i], border = NA)
		}
		circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
		major.at = seq(0, 10^nchar(max(xlim[, 2])), by = major.by)
		circos.axis(h = 0.5, major.at = major.at, labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, labels.cex = 0.3, labels.direction = "vertical_right")
		cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
		circos.text(mean(cell.xlim), 1.3, labels = gsub("chr", "", chr), sector.index = chr, cex = 1)
	}
	circos.par("cell.padding" = o.cell.padding, "points.overflow.warning" = TRUE)
	return(invisible(NULL))
}

approximate = function(df, window.size = NULL, numbericColumn.fun = mean) {

}

# data contains column data which can be data frame or matrix
# data is a list of data frame
circos.genomicTrackPlotRegion = function(data, matrix = FALSE, valueColumns = NULL, panel.fun = function(region, value)  NULL }) {
	
	# call circos.trackPlotRegion
	
	# re-define panel.fun
	genomicPanelFun = panel.fun
	
	if(is.dataFrameList(data)) {
		n = length(data)
		circos.trackPlotRegion(ylim = c(0.5, n + 0.5), panel.fun = function(x, y) {
			for(i in seq_len(n)) {
				df = data[[i]]
				genomicPanelFun(df[2:3, drop = FALSE], df[-(1:3)], hline = i)
			}
		})
	}
}

is.dataFrameList = function(data) {
	is.list(data) && all(sapply(data, is.data.frame))
}


# df contains three columns
circos.genomicPoints = function(region, value, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
	
	if(
}

circos.genomicLines = function(df, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
	
	if(
}	


circos.genomicRect = function(df, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
}	

