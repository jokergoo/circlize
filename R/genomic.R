
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
# -plotIdeogram             whether plot ideogram or just the genomic axis
#
# == details
# This is not a full functional function. It just provides a way to show how to
# draw genomics ideogram by this package. How to embed the ideogram into the
# circos layout is really subjective and should be applied according to specific situation.
#
# In fact, drawing ideogram with this package is really simple, you can look at the source code
# of this function to get a clue.
circos.initializeWithIdeogram = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), species = NULL,
    chromosome.index = NULL, major.by = 50000000, plotIdeogram = TRUE) {
	
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
		panel.fun = function(region, value, ...) {
			chr = get.current.chromosome()
			circos.axis(h = 0, major.at = major.at, labels = paste(major.at/1000000, "MB", sep = ""), labels.cex = 0.3, labels.direction = "vertical_right")
			xlim = get.cell.meta.data("xlim")
			circos.text(mean(xlim), 1.2, labels = gsub("chr", "", chr), cex = 1, adj = c(0.5, 0))
		}
	)
	
	# ideogram
	if(plotIdeogram) {
		circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = 0.05,
			panel.fun = function(region, value, ...) {
				col = cytoband.col(value[[2]])
				circos.genomicRect(region, value, ybottom = 0, ytop = 1, col = col, border = NA, ...)
				xlim = get.cell.meta.data("xlim")
				circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
			}
		)
	}
	
	circos.par("cell.padding" = o.cell.padding, "points.overflow.warning" = TRUE)
	return(invisible(NULL))
}

# == title
# Create or update a track plotting genomic graphics
#
# == param
# -data genomic data, 
# -ylim
# -stack
# -numeric.column
# -track.height
# -track.index
# -bg.col
# -bg.color
# -bg.lty
# -bg.lwd
# -panel.fun
circos.genomicTrackPlotRegion = function(data, ylim = NULL, stack = FALSE, numeric.column = NULL, 
	track.height = circos.par("default.track.height"), track.index = NULL,
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"), panel.fun = function(region, value, ...)  {NULL} ) {

	# re-define panel.fun
	genomicPanelFun = panel.fun

	# now `data` is either a data frame or a list of data frame
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		# check numeric.column
		if(!is.null(numeric.column)) {
			if(!all(sapply(data[-(1:3)][numeric.column], is.numeric))) {
				stop("Some of your `column.column` are not numeric.\n")
			}
		}
	}
	
	args = formals(genomicPanelFun)
	if(!(length(args) == 3 && names(args)[3] == "...")) {
		stop("The `panel.fun` need a third argument `...` to pass specicial settings to graphic functions.\n")
	}
	
	if(stack) {
	
		if(is.dataFrameList(data)) {
			n = length(data)
			
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
						
					chr = get.current.chromosome()
					for(i in seq_len(n)) {
						l = data[[i]][[1]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], hline = i+0, iStack = i+0)
						}
					}

				})
		} else {
			if(is.null(numeric.column)) {
				numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
			}
			n = length(numeric.column)
			non.numeric.column = setdiff(seq_along(data[-(1:3)]), numeric.column)
			
			# if there is no numeric column
			if(n == 0) {
				circos.trackPlotRegion(ylim = c(0.5, 1 + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
					track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
							
						chr = get.current.chromosome()
						l = data[[1]] == chr
						df = data[l, , drop = FALSE]
						i = 1
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)][non.numeric.column], hline = i+0, iStack = i+0)
						}

					})
			} else {
				circos.trackPlotRegion(ylim = c(0.5, n + 0.5), bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd, 
					track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
							
						chr = get.current.chromosome()
						for(i in seq_len(n)) {
							l = data[[1]] == chr
							df = data[l, , drop = FALSE]
							if(nrow(df)) {
								genomicPanelFun(df[2:3], df[-(1:3)][c(numeric.column[i], non.numeric.column)], hline = i+0, iStack = i+0, numeric.column = 1)
							}
						}

					})
			}
		}			
		
	} else {
	
		# numeric.column
		if(is.dataFrameList(data)) {
			nuc = lapply(data, function(gr) {
							numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
							if(length(numeric.column) == 0) {
								numeric(0)
							} else {
								numeric.column[1]
							}
						})
		} else {
			if(is.null(numeric.column)) {
				numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
			}
			nuc = numeric.column
		}
		
		# auto calculate ylim
		if(is.null(ylim)) {
			if(is.dataFrameList(data)) {
				ylim = range(unlist(lapply(seq_along(data), function(i) {
					gr = data[[i]]
					if(length(nuc[[i]]) == 0) {
						stop("There is no numeric column in one of your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.\n")
					}
					range(unlist(lapply(gr[-(1:3)][ nuc[[i]] ], range)))
				})))
			} else {
				if(is.null(numeric.column)) {
					
					if(length(nuc) == 0) {
						stop("There is no numeric column in your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.\n")
					}
				}
				ylim = range(unlist(lapply(data[-(1:3)][nuc], range)))
			}
		}
		
		if(is.dataFrameList(data)) {
			circos.trackPlotRegion(ylim = ylim, bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd,
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					for(i in seq_along(data)) {
						l = data[[i]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], numeric.column = eval(nuc[[i]]))
						}
					}
				})
		} else {
			circos.trackPlotRegion(ylim = ylim, bg.col = bg.col, bg.border = bg.border, bg.lwd = bg.lwd,
				track.height = track.height, track.index = track.index, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					df = data[data[[1]] == chr, , drop = FALSE]
					if(nrow(df)) {
						genomicPanelFun(df[2:3], df[-(1:3)], numeric.column = eval(nuc))
					}
				})
		}
	}
	
}

# == title
# Which stack the graphics are on
#
# == param
# -... invisible arguments that users do not need to care
#
# == details
# The function should only be put inside ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
getIStack = function(...) {
	args = list(...)
	return(args$iStack)
}


# ==param
# -region a data frame contains 2 column
# -value  a data frame contains values and other stuff
# -numeric.column
# -sector.index
# -track.index
# -posTransfrom
# -pch
# -col
# -cex
# -...
circos.genomicPoints = function(region, value, numeric.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	pch = par("pch"), col = par("col"), cex = par("cex"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$hline)) {
		value = data.frame(hline = rep(args$hline, nr))
		numeric.column = 1
	}
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
		numeric.column = 1
	}
	if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
		numeric.column = 1
	}

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
		if(length(numeric.column) == 0) {
			stop("Cannot find numeric column.\n")
		}
	}

	nc = length(numeric.column)

	pch = .normalizeGraphicalParam(pch, nc, nr, "pch")
	col = .normalizeGraphicalParam(col, nc, nr, "col")
	cex = .normalizeGraphicalParam(cex, nc, nr, "cex")

	if(nc == 1) {
		circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], 
			pch = pch, col = col, cex = cex, 
			sector.index = sector.index, track.index = track.index )
	} else {
		for(i in seq_len(nc)) {
			circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.column[i] ]], 
				pch = pch[i], col = col[i], cex = cex[i], 
				sector.index = sector.index, track.index = track.index ) 
		}
	}
}


circos.genomicLines = function(region, value, numeric.column = NULL, 
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
		numeric.column = 1
	}
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
		numeric.column = 1
	}
	if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
		numeric.column = 1
	}

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
		if(length(numeric.column) == 0) {
			stop("Cannot find numeric column.\n")
		}
	}

	nc = length(numeric.column)

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
			circos.lines( c(region[i, 1], region[i, 2]), c(value[i, numeric.column], value[i, numeric.column]), 
				col = col, lwd = lwd, lty = lty, type = type, 
				area = area, area.baseline = area.baseline, 
				border = border, pt.col = pt.col, cex = cex, pch = pch,
				sector.index = sector.index, track.index = track.index )
		}
	} else if(nc == 1) {
		circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], 
			col = col, lwd = lwd, lty = lty, type = type, 
			area = area, area.baseline = area.baseline, 
			border = border, pt.col = pt.col, cex = cex, pch = pch,
			sector.index = sector.index, track.index = track.index )
	} else {
		for(i in seq_len(nc)) {
			circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column[i] ]], 
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
	
	args = list(...)
	if(!is.null(args$hline)) {
		ytop = args$hline + 0.5
		ybottom = args$hline - 0.5
	}
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
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
	
	if(length(ytop.column) > 1) {
		stop("Only one ytop columns is allowed.\n")
	}
	if(length(ybottom.column) > 1) {
		stop("Only one ybottom columns is allowed.\n")
	}

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

circos.genomicLink = function(region1, region2, 
	rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA,
    top.ratio.low = NULL) {
	
	region1 = normalizeToDataFrame(region1)
	region2 = normalizeToDataFrame(region2)
	
	if(is.dataFrameList(region1)) {
		stop("`region1` can not be a region list.\n")
	}
	
	if(is.dataFrameList(region1)) {
		stop("`region1` can not be a region list.\n")
	}
	
	if(nrow(region1) != nrow(region2)) {
		stop("nrow of `region1` and `region2` differ.\n")
	}
	
	nr = nrow(region1)
	
	rou = .normalizeGraphicalParam(rou, 1, nr, "rou")
	top.ratio = .normalizeGraphicalParam(top.ratio, 1, nr, "top.ratio")
	col = .normalizeGraphicalParam(col, 1, nr, "col")
	lwd = .normalizeGraphicalParam(lwd, 1, nr, "lwd")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	border = .normalizeGraphicalParam(border, 1, nr, "border")
	top.ratio.low = .normalizeGraphicalParam(top.ratio.low, 1, nr, "top.ratio.low")
	
	for(i in seq_len(nrow(region1))) {
		circos.link(region1[i, 1], c(region1[i, 2], region1[i, 3]),
		            region2[i, 1], c(region2[i, 2], region2[i, 3]),
					rou = rou[i], top.ratio = top.ratio[i], col = col[i], lwd = lwd[i],
					lty = lty[i], border = border[i], top.ratio.low = top.ratio.low[i])
	}
}

circos.genomicText = function(region, value, labels = NULL, labels.column = NULL, numeric.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"), 
	track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	direction = c("default", "default2", "vertical_left",
                  "vertical_right", "horizontal", "arc"),
	adj = par("adj"), cex = 1, col = "black", font = par("font"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$hline)) {
		value = data.frame(hline = rep(args$hline, nr))
		numeric.column = 1
	}
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
		numeric.column = 1
	}
	if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
		numeric.column = 1
	}
	
	if(is.null(labels) && is.null(labels.column)) {
		stop("You should either specify `labels` or `labels.column`.\n")
	}
	
	if(!is.null(labels)) {
		if(is.vector(labels) && !is.list(labels) && length(labels) == 1) {
			value = cbind(value, labels = rep(labels, nr))
			labels.column = ncol(value)
		}
		if(is.vector(labels) && !is.list(labels) && length(labels) == nr) {
			value = cbind(value, labels = labels)
			labels.column = ncol(value)
		}
	}

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
		if(length(numeric.column) == 0) {
			stop("Cannot find numeric column.\n")
		}
	}
	
	if(length(numeric.column) > 1) {
		stop("You can only have one numeric column.\n")
	}

	nc = length(numeric.column)

	direction = .normalizeGraphicalParam(direction, nc, nr, "direction")
	col = .normalizeGraphicalParam(col, nc, nr, "col")
	cex = .normalizeGraphicalParam(cex, nc, nr, "cex")
	adj = .normalizeGraphicalParam(adj, nc, nr, "adj")
	font = .normalizeGraphicalParam(font, nc, nr, "font")

	circos.text( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], value[[labels.column]],
		direction = direction, adj = adj, cex = cex, col = col, font = font,
		sector.index = sector.index, track.index = track.index )

}

# high-level
circos.genomicPosTransformLine = function(region, track.height = 0.1, posTransform = NULL, 
	type = c("in", "out"), col = "black", lwd = par("lwd"), lty = par("lty")) {
	
	region = normalizeToDataFrame(region)
	
	type = match.arg(type)
	
	if(is.null(posTransform)) {
		region_new = region
	} else {
		region_new = cbind(region[[1]], posTransform(region[2:3]))
	}
	
	if(type == "in") {
		circos.trackPlotRegion(region[[1]], ylim = c(0, 1), border = NA, panel.fun = function(x, y) {
			chr = get.chromosome()
			region_subset = region[region[[1]] == chr, , drop = FALSE]
			region_new_subset = region_new[region_new[[1]] == chr, , drop = FALSE]
			for(i in seq_len(nrow(region_subset))) {
				circos.lines(c(region_subset[i, 2], region_subset[i, 3]), c(1, 1), col = col, lwd = lwd, lty = lty)
				circos.lines(c(region_new_subset[i, 2], region_new_subset[i, 3]), c(0, 0), col = col, lwd = lwd, lty = lty)
				mid = (region_subset[i, 2] + region_subset[i, 3])/2
				mid_new = (region_new_subset[i, 2] + region_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(1, 2/3, 1/3, 0), col = col, lwd = lwd, lty = lty)
			}
		})
	} else {
		circos.trackPlotRegion(region[[1]], ylim = c(0, 1), border = NA, panel.fun = function(x, y) {
			chr = get.chromosome()
			region_subset = region[region[[1]] == chr, , drop = FALSE]
			region_new_subset = region_new[region_new[[1]] == chr, , drop = FALSE]
			for(i in seq_len(nrow(region_subset))) {
				circos.lines(c(region_subset[i, 2], region_subset[i, 3]), c(0, 0), col = col, lwd = lwd, lty = lty)
				circos.lines(c(region_new_subset[i, 2], region_new_subset[i, 3]), c(1, 1), col = col, lwd = lwd, lty = lty)
				mid = (region_subset[i, 2] + region_subset[i, 3])/2
				mid_new = (region_new_subset[i, 2] + region_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(0, 1/3, 2/3, 1), col = col, lwd = lwd, lty = lty)
			}
		})
	}
}

circos.genomicDensity = function(data, window.size = 10000000, overlap = TRUE, area = TRUE, area.baseline = 0, ...) {
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		data = list(data)
	}
	
	for(i in seq_along(data)) {
		circos.genomicTrackPlotRegion(data[[i]], ylim = c(0, 1), panel.fun = function(region, value) {
			den = genomicDensity(region, window.size = window.size, overlap = overlap)
			circos.genomicLines(den[1:2], den[3], area = area, area.baseline = area.baseline, ...) 
		})
	}
}

genomicDensity = function(ir, window.size = 10000000, overlap = TRUE) {
	
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

highlight.chromosome = function(chromosome, track.index, col, border, lwd, lty) {

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

# == title
# get current chromosome name
#
# == details
# The function is a simple wrapper of ``get.cell.meta.data("sector.index")`` and
# should only be put inside ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
get.current.chromosome = function() {
	get.cell.meta.data("sector.index")
}




is.dataFrameList = function(data) {
	is.list(data) && all(sapply(data, is.data.frame))
}


normalizeToDataFrame = function(data) {

	all.chr = circlize:::get.all.sector.index()
	
	if(is.data.frame(data)) {
		if(ncol(data) < 3) {
			stop("Your data frame is less than 3 column!.\n")
		}
		data = data[data[[1]] %in% all.chr, , drop = FALSE]
		return(data)
	} else if(class(data) == "GRanges") {
		data = cbind(as.data.frame(data)[1:3], as.data.frame(mcols(data)))
		data = data[data[[1]] %in% all.chr, , drop = FALSE]
		return(data)
	} else if(class(data) == "GRangesList") {
		df = lapply(data, function(gr) {
			value = as.data.frame(mcols(gr))
			if(ncol(value) == 0) {
				res = as.data.frame(gr)[1:3]
			} else {
				numeric.column = which(sapply(value, is.numeric))
				if(length(numeric.column) > 1) {
					warning("You have more than one numeric column, only take the first one.\n")
				}
				res = cbind(as.data.frame(gr)[1:3], value[numeric.column[1]])
			}
			res = res[res[[1]] %in% all.chr, , drop = FALSE]
		})
		return(df)
	} else if(is.list(data) && all(sapply(data, function(x) is.data.frame(x) || class(x) == "GRanges"))) {
		df = lapply(data, function(gr) {
			if(class(gr) == "GRanges") {
				gr = cbind(as.data.frame(gr)[1:3], as.data.frame(mcols(gr)))
			} else {
				if(ncol(gr) < 3) {
					stop("Your data frame is less than 3 column!.\n")
				}
			}
			value = gr[-(1:3)]
			if(ncol(value) == 0) {
				res = gr[1:3]
			} else {
				numeric.column = which(sapply(value, is.numeric))
				if(length(numeric.column) > 1) {
					warning("You have more than one numeric column, only take the first one.\n")
				}
				res = cbind(gr[1:3], value[numeric.column[1]])
			}
			res = res[res[[1]] %in% all.chr, , drop = FALSE]
		})
		return(df)
	} else {
		stop("wrong")
	}
}


.normalizeGraphicalParam = function(x, nc, nr, name) {
	if(nc == 1) {
		if(!(length(x) == 1 || length(x) == nr)) {
			stop(qq("The length of `@{name}` (@{length(x)}) should be equal to 1 or the number of your regions (@{nr}).\n"))
		} else if(length(x) == 1) {
			x = rep(x, nr)
		}
	} else {
		if(!(length(x) == 1 || length(x) == nc)) {
			stop(qq("The length of `@{name}` (@{length(x)}) should be equal to 1 or the number of your data column (@{nc}).\n"))
		} else if(length(x) == 1) {
			x = rep(x, nc)
		}
	}
	return(x)
}


circos.genomicRainfall = function(data, col = "black", pch = par("pch"), cex = par("cex"), 
	track.height = circos.par("default.track.height"), track.index = NULL,
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd")) {
	
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		data = list(data)
	}
	
	if(length(col) == 1) {
		col = rep(col, length(data))
	}
	if(length(pch) == 1) {
		pch = rep(pch, length(data))
	}
	if(length(cex) == 1) {
		cex = rep(cex, length(data))
	}
	
	circos.genomicTrackPlotRegion(data, ylim = c(0, 9), panel.fun = function(region, value, ...) {
		ir = IRanges(start = region[[1]], end = region[[2]])
		ir = as.data.frame(sort(ir))
		n = nrow(ir)
		dist = numeric(n)
		
		if(n < 2) {
			return(NULL)
		}
		
		dist[1] = ir[2, 1] - ir[1, 2]
		dist[n] = ir[n, 1] - ir[n-1, 2]
			
		if(n > 2) {
			d1 = ir[3:n, 1] - ir[2:(n-1), 2]
			d2 = ir[2:(n-1), 1] - ir[1:(n-2), 2]
			dist[2:(n-1)] = pmin(d1, d2)
		}
		
		dist = ifelse(dist < 0, 0, dist)
		dist = log10(dist + 1)
		
		i = getIStack(...)
		circos.genomicPoints(ir, dist, col = col[i], cex = cex[i], pch = pch[i], ...)
	})
	
}
