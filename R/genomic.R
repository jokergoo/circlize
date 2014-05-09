
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

blur = function(df, window.size = NULL, returnDensity = FALSE, numbericColumn.fun = mean) {

}

# data contains column data which can be data frame or matrix
# data is a list of data frame
circos.genomicTrackPlotRegion = function(data, ylim = NULL, containMatrix = FALSE, numeric.columns = NULL, 
	track.height = circos.par("default.track.height"), track.index = NULL,
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"), panel.fun = function(region, value)  {NULL} ) {
	
	# call circos.trackPlotRegion
	
	# re-define panel.fun
	genomicPanelFun = panel.fun

	# now `data` is either a data frame or a list of data frame
	data = normalizeToDataFrame(data, containMatrix = containMatrix)
	
	if(is.dataFrameList(data)) {
		n = length(data)

		args = formals(genomicPanelFun)
		if(!(length(args) == 3 && names(args)[3] == "...")) {
			stop("Since your input data is a list of data frame or you are going to plot a data matrix, the `panel.fun` need a third argument `...` to pass specicial settings to graphic functions.\n")
		}

		# if it is a data frame list, the graph would look like grid-like
		# which means, grahics for each data frame would be plot in a line
		if(containMatrix) {
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					chr = get.cell.meta.data("sector.index")
					l = data[[1]][[1]] == chr
					for(i in seq_len(n)) {
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							if(i == 1) {
								genomicPanelFun(df[2:3], df[-(1:3)], hline = i+0) # lazy loading?
							} else {
								genomicPanelFun(data[[1]][l, 2:3, drop = FALSE], df[seq_along(df)], hline = i+0)
							}
						}
					}

				})
		} else {
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					chr = get.cell.meta.data("sector.index")
				
					for(i in seq_len(n)) {
						l = data[[i]][[1]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], hline = i+0)
						}
					}
				})
		}
		
	} else {
		if(is.null(ylim)) {
			if(is.null(numeric.columns)) {
				numeric.columns = which(sapply(data[-(1:3)], is.numeric))
			}
			ylim = range(unlist(lapply(data[-(1:3)], range)))
		}
		circos.trackPlotRegion(ylim = ylim, bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd,
			track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
				chr = get.cell.meta.data("sector.index")
				data = data[data[[1]] == chr, , drop = FALSE]
				if(nrow(data)) {
					genomicPanelFun(data[2:3], data[-(1:3)])
				}
			})
	}
}


is.dataFrameList = function(data) {
	is.list(data) && all(sapply(data, is.data.frame))
}

# if the data frame contains a numeric matrix
# the function would return a data frame list in which
# only the first data frame contains region
normalizeToDataFrame = function(data, containMatrix = FALSE) {
	if(is.data.frame(data)) {
		if(ncol(data) < 3) {
			stop("Your data frame is less than 3 columns!.\n")
		}
		if(containMatrix) {
			if(ncol(data) < 4) {
				stop("Since you claimed your data frame contains a numeric matrix, but the columns are less than 4!\n")
			}
			df_list[[1]] = data[1:4]
			for(i in seq_len(ncol(data)-4)) {
				df_list[[i + 1]] = data[, i + 4, drop = FALSE]
			}
			names(df_list) = colnames(data)[-(1:3)]
			return(df_list)
		} else {
			return(data)
		}
	} else if(class(data) == "GRanges") {
		data = cbind(as.data.frame(data)[1:3], as.data.frame(mcols(data)))
		if(containMatrix) {
			if(ncol(data) < 4) {
				stop("Since you claimed your data frame contains a numeric matrix, but no meta column in your GRange object!\n")
			}
			df_list[[1]] = data[1:4]
			for(i in seq_len(ncol(data)-4)) {
				df_list[[i + 1]] = data[, i + 4, drop = FALSE]
			}
			names(df_list) = colnames(data)[-(1:3)]
			return(df_list)
		} else {
			return(data)
		}
	} else if(class(data) == "GRangesList") {
		df = lapply(data, function(gr) {
			cbind(as.data.frame(gr)[1:3], as.data.frame(mcols(gr)))
		})
	} else if(is.list(data) && all(sapply(data, function(x) is.data.frame(x) || class(x) == "GRanges"))) {
		df = lapply(data, function(gr) {
			if(class(gr) == "GRanges") {
				cbind(as.data.frame(gr)[1:3], as.data.frame(mcols(gr)))
			} else {
				if(ncol(gr) < 3) {
					stop("Your data frame is less than 3 columns!.\n")
				}
				return(gr)
			}
		})
	} else {
		stop("wrong")
	}
}


# ==param
# -region a data frame contains 2 columns
# -value  a data frame contains values and other stuff
circos.genomicPoints = function(region, value, hline = NULL, numeric.columns = NULL, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, pch = par("pch"), col = par("col"), cex = par("cex")) {
	
	nr = nrow(region)
	if(!is.null(hline)) {
		value = data.frame(hline = rep(hline, nr))
	}

	if(!is.data.frame(value)) {
		value = as.data.frame(value)
	}

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	if(is.null(numeric.columns)) {
		numeric.columns = which(sapply(value, is.numeric))
	}

	nc = length(numeric.columns)

	if(nc == 1) {
		if(!(length(pch) == 1 || length(pch) == nr)) {
			stop("The length of `pch` should be equal to 1 or the number of your regions.\n")
		} else if(length(pch) == 1) {
			pch = rep(pch, nr)
		}
	} else {
		if(!(length(pch) == 1 || length(pch) == nc)) {
			stop("The length of `pch` should be equal to 1 or the number of your data columns.\n")
		} else if(length(pch) == 1) {
			pch = rep(pch, nc)
		}
	}

	if(nc == 1) {
		circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.columns[i] ]], 
			pch = pch, col = col, cex = cex, 
			sector.index = sector.index, track.index = track.index )
	} else {
		for(i in seq_len(nc)) {
			circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.columns[i] ]], 
				pch = pch[i], col = col[i], cex = cex[i], 
				sector.index = sector.index, track.index = track.index ) 
		}
	}
}



circos.genomicLines = function(df, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
	
	if(
}	


circos.genomicRect = function(df, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
}	

posTransform.default = function(pos) {
	xlim = get.cell.meta.data("xlim")
	segment = seq(xlim[1], xlim[2], by = nrow(pos) + 1)
	return(data.frame(start = segment[-length(segment)], end = segment[-1]))
}

highlight.chromosome = function() {

}

# pre-defined high-level functions
heatmap = function(x) {

}

posTransformLines = function(x) {

}