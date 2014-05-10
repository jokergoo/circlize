
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
# -major.by         increment of major ticks
# -plot             whether plot ideogram or just the genomic axis
#
# == details
# This is not a full functional function. It just provides a way to show how to
# draw genomics ideogram by this package. How to embed the ideogram into the
# circos layout is really subjective and should be applied according to specific situation.
#
# In fact, drawing ideogram with this package is really simple, you can look at the source code
# of this function to get a clue.
circos.initializeWithIdeogram = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), species = NULL,
    chromosome.index = NULL, major.by = 50000000, plot = TRUE) {
	
	cytoband = read.cytoband(file, species = species)
	df = cytoband$df
	chromosome = cytoband$chromosome
	
	if(! is.null(chromosome.index)) {
		chromosome.index = gsub("chr", "", chromosome.index)
		chromosome.index = paste("chr", chromosome.index, sep = "")
		chromosome = chromosome[chromosome %in% chromosome.index]
	}
	
	xlim = matrix(nrow = 0, ncol = 2)
	for(chr in chromosome) {
		i = which(cytoband$chromosome == chr)
		xlim = rbind(xlim,c(0, cytoband$chr.len[i]))
	}
	
	major.at = seq(0, 10^nchar(max(xlim[, 2])), by = major.by)
	
	par(mar = c(1, 1, 1, 1), lwd = 0.5)
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
	circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
	
	# axis and chromosome names
	circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = 0.05,
		panel.fun = function(region, value) {
			chr = get.current.chromosome()
			circos.axis(h = 0, major.at = major.at, labels = paste(major.at/1000000, "MB", sep = ""), labels.cex = 0.3, labels.direction = "vertical_right")
			xlim = get.cell.meta.data("xlim")
			circos.text(mean(xlim), 1.2, labels = gsub("chr", "", chr), cex = 1, adj = c(0.5, 0))
		}
	)
	
	# ideogram
	if(plot) {
		circos.genomicTrackPlotRegion(df, by.line = TRUE, bg.border = NA, track.height = 0.05,
			panel.fun = function(region, value, ...) {
				col = cytoband.col(value[[2]])
				circos.genomicRect(region, value, col = col, border = NA, ...)
				xlim = get.cell.meta.data("xlim")
				iline = getI(...)
				circos.rect(xlim[1], iline-0.5, xlim[2], iline+0.5, border = "black")
			}
		)
	}
	
	circos.par("cell.padding" = o.cell.padding, "points.overflow.warning" = TRUE)
	return(invisible(NULL))
}

# data contains column data which can be data frame or matrix
# data is a list of data frame
circos.genomicTrackPlotRegion = function(data, ylim = NULL, by.line = FALSE, numeric.columns = NULL, 
	track.height = circos.par("default.track.height"), track.index = NULL,
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"), panel.fun = function(region, value)  {NULL} ) {
	
	# call circos.trackPlotRegion
	
	# re-define panel.fun
	genomicPanelFun = panel.fun

	# now `data` is either a data frame or a list of data frame
	data = normalizeToDataFrame(data)
	
	if(is.dataFrameList(data)) {
		args = formals(genomicPanelFun)
		if(!(length(args) == 3 && names(args)[3] == "...")) {
			stop("Since your input data is a list of data frame or you are going to plot a data matrix, the `panel.fun` need a third argument `...` to pass specicial settings to graphic functions.\n")
		}
	}
	
	if(by.line) {
	
		# transform all data frame as list of data frame
		if(is.dataFrameList(data)) {
			n = length(data)
			
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
						
					chr = get.cell.meta.data("sector.index")
					for(i in seq_len(n)) {
						l = data[[i]][[1]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], hline = i+0, i = i+0)
						}
					}

				})
		} else {
			numeric.columns = which(sapply(data[-(1:3)], is.numeric))
			n = length(numeric.columns)
			
			if(is.null(numeric.columns)) {
				n = 1
				data = cbind(data, rep(1, nrow(data)))
				numeric.columns = ncol(data)
			} else {
				n = length(numeric.columns)
			}
			
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
						
					chr = get.cell.meta.data("sector.index")
					for(i in seq_len(n)) {
						l = data[[1]] == chr
						df = data[l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[numeric.columns[i]], hline = i+0, i = i+0)
						}
					}

				})
		}			
		
	} else {
		# auto calculate ylim
		if(is.null(ylim)) {
			if(is.dataFrameList(data)) {
				ylim = range(unlist(lapply(data, function(gr) {
						numeric.columns = which(sapply(gr[-(1:3)], is.numeric))[1]   # first numeric column in each data frame
						range(unlist(lapply(data[-(1:3)][numeric.columns], range)))
					})))
			} else {
				if(is.null(numeric.columns)) {
					numeric.columns = which(sapply(data[-(1:3)], is.numeric))
				}
				ylim = range(unlist(lapply(data[-(1:3)][numeric.columns], range)))
			}
		}
		
		if(is.dataFrameList(data)) {
			circos.trackPlotRegion(ylim = ylim, bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd,
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					
					chr = get.cell.meta.data("sector.index")
					for(i in seq_along(data)) {
						df = data[data[[i]] == chr, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], i = i+0)
						}
					}
				})
		} else {
			circos.trackPlotRegion(ylim = ylim, bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd,
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					
					chr = get.cell.meta.data("sector.index")
					df = data[data[[1]] == chr, , drop = FALSE]
					if(nrow(df)) {
						genomicPanelFun(df[2:3], df[-(1:3)])
					}
				})
		}
	}
	
}

# ==param
# -... invisible arguments that users do not need to care
#
getI = function(...) {
	args = list(...)
	return(args$i)
}


# ==param
# -region a data frame contains 2 columns
# -value  a data frame contains values and other stuff
# -numeric.columns
# -sector.index
# -track.index
# -posTransfrom
# -pch
# -col
# -cex
# -...
circos.genomicPoints = function(region, value, numeric.columns = NULL, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	pch = par("pch"), col = par("col"), cex = par("cex"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$hline)) {
		value = data.frame(hline = rep(args$hline, nr))
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

	pch = .normalizeGraphicalParam(pch, nc, nr, "pch")
	col = .normalizeGraphicalParam(col, nc, nr, "col")
	cex = .normalizeGraphicalParam(cex, nc, nr, "cex")

	if(nc == 1) {
		circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.columns ]], 
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


circos.genomicLines = function(region, value, numeric.columns = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l",
    area = FALSE, area.baseline = "bottom", border = "black",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$hline)) {
		value = data.frame(hline = rep(args$hline, nr))
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

	col = .normalizeGraphicalParam(col, nc, 1, "col")
	lwd = .normalizeGraphicalParam(lwd, nc, 1, "col")
	lty = .normalizeGraphicalParam(lty, nc, 1, "col")
	type = .normalizeGraphicalParam(type, nc, 1, "col")
	area = .normalizeGraphicalParam(area, nc, 1, "col")
	area.baseline = .normalizeGraphicalParam(area.baseline, nc, 1, "col")
	border = .normalizeGraphicalParam(border, nc, 1, "col")
	pt.col = .normalizeGraphicalParam(pt.col, nc, 1, "col")
	cex = .normalizeGraphicalParam(cex, nc, 1, "col")
	pch = .normalizeGraphicalParam(pch, nc, 1, "col")
	
	if(!is.null(args$hline)) {
		for(i in seq_len(nr)) {
			circos.lines( c(region[i, 1], region[i, 2]), c(value[i, numeric.columns], value[i, numeric.columns]),
				col = col, lwd = lwd, lty = lty,
				sector.index = sector.index, track.index = track.index )
		}
	} else if(nc == 1) {
		circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.columns ]], 
			col = col, lwd = lwd, lty = lty, type = type, 
			area = area, area.baseline = area.baseline, 
			border = border, pt.col = pt.col, cex = cex, pch = pch,
			sector.index = sector.index, track.index = track.index )
	} else {
		for(i in seq_len(nc)) {
			circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.columns[i] ]], 
				col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
				area = area[i], area.baseline = area.baseline[i], 
				border = border[i], pt.col = pt.col[i], cex = cex[i], pch = pch[i],
				sector.index = sector.index, track.index = track.index )
		}
	}
}	


circos.genomicRect = function(region, value, 
	ytop = NULL, ybottom = NULL, ybottom.column = NULL, ytop.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	col = NA, border = "black", lty = par("lty"), lwd = par("lwd"), ...) {
	
	nr = nrow(region)
	if(!is.data.frame(value)) {
		value = as.data.frame(value)
	}
	
	args = list(...)
	if(!is.null(args$hline)) {
		ytop = args$hline + 0.5
		ybottom = args$hline - 0.5
	}
	
	# 1. check ytop and ybottom
	# 2. check ytop.colum and ybottom.column
	
	if(!is.null(ytop)) {
		if(length(ytop) == 1) {
			ytop = rep(ytop, nr)
		}
		value = cbind(value, ytop)
		ytop.column = ncol(value)
	}
	
	if(!is.null(ybottom)) {
		if(length(ybottom) == 1) {
			ybottom = rep(ybottom, nr)
		}
		value = cbind(value, ybottom)
		ybottom.column = ncol(value)
	}
	
	ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
	if(is.null(ybottom.column) && is.null(ytop.column)) {
		# if no ybottom and ytop column are set, the rect will draw along whole ylim
		value = cbind(value, rep(ylim[2], nr), rep(ylim[1], nr))
		ytop.column = ncol(value) - 1
		ybottom.column = ncol(value)
	} else if(is.null(ybottom.column)) {
		value = cbind(value, rep(ylim[1], nr))
		ybottom.column = ncol(value)
	} else if(is.null(ytop.column)) {
		value = cbind(value, rep(ylim[2], nr))
		ytop.column = ncol(value)
	}

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	ytop.column = ytop.column[1]
	ybottom.column = ybottom.column[1]

	col = .normalizeGraphicalParam(col, 1, nr, "col")
	border = .normalizeGraphicalParam(border, 1, nr, "border")
	lwd = .normalizeGraphicalParam(lwd, 1, nr, "lwd")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	
	for(i in seq_len(nr)) {
		circos.rect(region[i, 1], value[i, ybottom.column], region[i, 2], value[i, ytop.column],
		            sector.index = sector.index, track.index = track.index,
					col = col[i], border = border[i], lwd = lwd[i], lty = lty[i])
	}

}

circos.genomicLink = function(df1, df2, 
	rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, n = 101,
    top.ratio.low = NULL) {
	df1 = normalizeToDataFrame(df1)
	df2 = normalizeToDataFrame(df2)
	
	if(is.dataFrameList(df1)) {
		stop("`df1` can not be a region list.\n")
	}
	
	if(is.dataFrameList(df2)) {
		stop("`df2` can not be a region list.\n")
	}
	
	if(nrow(df1) != nrow(df2)) {
		stop("nrow of `df1` and `df2` differ.\n")
	}
	
	for(i in seq_len(nrow(df1))) {
		circos.link(df1[i, 1], c(df1[i, 2], df1[i, 3]),
		            df2[i, 1], c(df2[i, 2], df2[i, 3]))
	}
}

circos.genomicText = function(df1, df2) {

}

# high-level
circos.genomicPosTransformLine = function(df, track.height, posTransform = NULL, type = c("in", "out")) {
	df = normalizeToDataFrame(df)

	if(is.null(posTransform)) {
		df_new = df
	} else {
		df_new = cbind(df[[1]], posTransform(df[2:3]))
	}
	
	circos.trackPlotRegion(df[[1]], ylim = c(0, 1), border = NA, panel.fun = function(x, y) {
		chr = get.chromosome()
		df_subset = df[df[[1]] == chr, , drop = FALSE]
		df_new_subset = df_new[df_new[[1]] == chr, , drop = FALSE]
		for(i in seq_len(nrow(df_subset))) {
			if(type == "in") {
				circos.lines(c(df_subset[i, 2], df_subset[i, 3]), c(1, 1))
				circos.lines(c(df_new_subset[i, 2], df_new_subset[i, 3]), c(0, 0))
				mid = (df_subset[i, 2] + df_subset[i, 3])/2
				mid_new = (df_new_subset[i, 2] + df_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(1, 2/3, 1/3, 0))
			} else {
				circos.lines(c(df_subset[i, 2], df_subset[i, 3]), c(0, 0))
				circos.lines(c(df_new_subset[i, 2], df_new_subset[i, 3]), c(1, 1))
				mid = (df_subset[i, 2] + df_subset[i, 3])/2
				mid_new = (df_new_subset[i, 2] + df_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(0, 1/3, 2/3, 1))
			}
		}
	})
}

circos.genomicDensity = function(data, window.size = 10000000, overlap = TRUE) {
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		data = list(data)
	}
	
	for(i in seq_along(data)) {
		circos.genomicTrackPlotRegion(data[[i]], ylim = c(0, 1), panel.fun = function(region, value) {
			den = .genomicDensity(region, window.size = window.size, overlap = overlap)
			circos.genomicLines(den[1:2], den[3], area = TRUE, area.baseline = 0) 
		})
	}
}

.genomicDensity = function(ir, window.size = 10000000, overlap = TRUE) {
	
	if(class(ir) != "IRanges") {
		ir = IRanges(start = ir[[1]], end = ir[[2]])
	}
	
	ir = Reduce(sort(ir))

	# make a segmentation
	if(overlap) {
		b = seq(1, max(end(gr)), by = window.size/2)
		s = b[-length(b)]
		s = s[-length(s)]
		e = s + window.size - 1
		
	} else {
		b = seq(1, max(end(gr)), by = window.size)
		s = b[-length(b)]
		e = s + window.size - 1
	}

	s = as.integer(s)
	e = as.integer(e)

	y = rep(0, length(s))
	names(y) = paste(s, e, sep = ",")
	
	windows = IRanges(start = s, end = e)
	
	ol = findOverlap(windows, ir)
	intersect = pintersect(windows[o[, 1]], ir[o[, 2]])
	pct = (end(intersect) - start(intersect) + 1) / (end(windows[o[, 1]]) - start(windows[o[, 1]]) + 1)
	fn = paste(start(windows[o[, 1]]), end(windows[o[, 1]]), sep = ",")
	unified_pct = tapply(pct, fn, sum)
	y[names(unified_pct)] = unified_pct

	return(data.frame(start = s, end = e, pct = y))
}

posTransform.default = function(pos) {
	xlim = get.cell.meta.data("xlim")
	segment = seq(xlim[1], xlim[2], by = nrow(pos) + 1)
	return(data.frame(start = segment[-length(segment)], end = segment[-1]))
}

continuousIndexSegment = function(x) {
	if(length(x) == 1) {
		return(list(x))
	} else {
		k = c(0, which(diff(x) > 1), length(x))
		lt = vector("list", length = length(k) - 1)
		for(i in seq_along(k)[-length(k)]) {
			lt[[i]] = x[(k[i] + 1):(k[i+1])]
		}
		return(x)
	}
}

get.current.chromosome = function() {
	get.cell.meta.data("sector.index")
}




is.dataFrameList = function(data) {
	is.list(data) && all(sapply(data, is.data.frame))
}


normalizeToDataFrame = function(data) {
	if(is.data.frame(data)) {
		if(ncol(data) < 3) {
			stop("Your data frame is less than 3 columns!.\n")
		}
		return(data)
	} else if(class(data) == "GRanges") {
		data = cbind(as.data.frame(data)[1:3], as.data.frame(mcols(data)))
		return(data)
	} else if(class(data) == "GRangesList") {
		df = lapply(data, function(gr) {
			value = as.data.frame(mcols(gr))
			if(ncol(value) == 0) {
				as.data.frame(gr)[1:3]
			} else {
				numeric.columns = which(sapply(value, is.numeric))
				if(length(numeric.columns) > 1) {
					warning("You have more than one numeric columns, only take the first one.\n")
				}
				cbind(as.data.frame(gr)[1:3], value[numeric.columns[1]])
			}
		})
		return(df)
	} else if(is.list(data) && all(sapply(data, function(x) is.data.frame(x) || class(x) == "GRanges"))) {
		df = lapply(data, function(gr) {
			if(class(gr) == "GRanges") {
				gr = cbind(as.data.frame(gr)[1:3], as.data.frame(mcols(gr)))
			} else {
				if(ncol(gr) < 3) {
					stop("Your data frame is less than 3 columns!.\n")
				}
			}
			value = gr[-(1:3)]
			if(ncol(value) == 0) {
				gr[1:3]
			} else {
				numeric.columns = which(sapply(value, is.numeric))
				if(length(numeric.columns) > 1) {
					warning("You have more than one numeric columns, only take the first one.\n")
				}
				cbind(gr[1:3], value[numeric.columns[1]])
			}
		})
		return(df)
	} else {
		stop("wrong")
	}
}


.normalizeGraphicalParam = function(x, nc, nr, name) {
	if(nc == 1) {
		if(!(length(x) == 1 || length(x) == nr)) {
			stop(qq("The length of `@{name}` should be equal to 1 or the number of your regions @{nr}.\n"))
		} else if(length(x) == 1) {
			x = rep(x, nr)
		}
	} else {
		if(!(length(x) == 1 || length(x) == nc)) {
			stop(qq("The length of `@{name}` should be equal to 1 or the number of your data columns (@{nc}).\n"))
		} else if(length(x) == 1) {
			x = rep(x, nc)
		}
	}
	return(x)
}
