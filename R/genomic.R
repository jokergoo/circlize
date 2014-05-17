
# == title
# Initialize the circos layout with an ideogram
#
# == param
# -file    cytoband file. By default it is the cytoband data for human
# -species abbrevations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically.
# -chromosome.index index for chromosome. The index is used only for subsetting, not for re-ordering.
#                   The value should be 1, 2, ... or chr1, chr2, ...
# -major.by     increment of major ticks
# -plotType     which part should be drawn. ``rect`` for ideogram rectangle, ``axis`` for genomic axis and ``labels`` for chromosome names
#
# == details
# The functions calls `circos.genomicInitialize`.
circos.initializeWithIdeogram = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), 
	species = NULL, chromosome.index = NULL, major.by = 50000000, plotType = c("rect", "axis", "labels")) {
	
	cytoband = read.cytoband(file, species = species)
	df = cytoband$df
	chromosome = cytoband$chromosome
	
	if(! is.null(chromosome.index)) {
		chromosome.index = gsub("chr", "", chromosome.index)
		chromosome.index = paste("chr", chromosome.index, sep = "")
		chromosome = chromosome[chromosome %in% chromosome.index]
	}
	
	df = df[df[[1]] %in% chromosome, , drop = FALSE]
	df[[1]] = factor(as.vector(df[[1]]), levels = chromosome)
	
	# sn for sector names, but not for sector index
	sn = unique(df[[1]])
	sn = gsub("chr", "", sn)
	
	circos.genomicInitialize(df, sector.names = sn, major.by = major.by, plotType = plotType, 
		colorMappingColumn = 5, colorMappingFun = function(x) cytoband.col(x))
}

# == title
# Initialize circos plot with genomic data
#
# == param
# -data a data frame containing genomic data or a ``GRanges`` object.
# -sector.names names for each sectors which will be drawn along each sector
# -major.by increment of major ticks
# -plotType which part should be drawn. ``rect`` for the rectangle, ``axis`` for genomic axis and ``labels`` for chromosome names
# -colorMappingColumn If you want to draw different color on different genomic regions, which columns should be refered to?
#                     If ``data`` is a data frame, the index starts from the first column. If ``data`` is a ``GRanges`` object,
#                     the index starts in the meta columns.
# -colorMappingFun How to map values in ``colorMappingColumn`` to different colors
#
# == details
# The function will initialize circos plot from genomic data provided. For values in ``plotType``, ``axis`` or ``labels`` will
# create a new track and ``rect`` will create a new track.
#
# For more details on initializing genomic plot, please refer to vignettes.
circos.genomicInitialize = function(data, sector.names = NULL, major.by = 50000000, plotType = c("rect", "axis", "labels"), 
	colorMappingColumn = NULL, colorMappingFun = function(x) rep("grey", length(x))) {
	
	if(is.data.frame(data)) {
		colorMappingColumn = colorMappingColumn - 3
	}
	
	data = normalizeToDataFrame(data)
	
	if(is.factor(data[[1]])) {
		fa = levels(data[[1]])
	} else {
		fa = unique(data[[1]])
	}
	
	if(!is.null(sector.names)) {
		if(length(sector.names) != length(fa)) {
			stop("length of `sector.names` and length of sectors differ.\n")
		}
	} else {
		sector.names = fa
	}
	
	names(sector.names) = fa
	
	# calculate xlim
	x1 = tapply(data[[2]], data[[1]], min)[fa]
	x2 = tapply(data[[3]], data[[1]], max)[fa]
	
	par(mar = c(1, 1, 1, 1))
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
	circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, x2))
	
	major.at = seq(0, 10^nchar(max(x2)), by = major.by)
	if(major.by > 1e6) {
		major.tick.labels = paste(major.at/1000000, "MB", sep = "")
	} else if(major.by > 1e3) {
		major.tick.labels = paste(major.at/100, "KB", sep = "")
	} else {
		major.tick.labels = paste(major.at, "bp", sep = "")
	}
	
	# axis and chromosome names
	if(plotType %in% c("axis", "labels")) {
		circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = 0.05,
			panel.fun = function(region, value, ...) {
				sector.index = get.cell.meta.data("sector.index")
				circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, labels.cex = 0.3, labels.direction = "vertical_right")
				xlim = get.cell.meta.data("xlim")
				circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], cex = 1, adj = c(0.5, 0))
			}
		)
	}
	
	# ideogram
	if(plotType %in% c("rect")) {
		circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = 0.05,
			panel.fun = function(region, value, ...) {
				col = colorMappingFun(value[[colorMappingColumn]])
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
# -data A bed-file-like data frame or a ``GRanges`` object. It can also be a list
#       containing data frames and ``GRanges`` objects.
# -ylim if it is ``NULL``, the value will be calculated from data. If ``stack`` is set to ``TRUE``, the value is disabled.
# -stack If ``data`` is a list of data frames or contains numeric columns more than one,
#        whether to plot in a "stack" mode
# -numeric.column Columns of numeric values in ``data`` that will be used for plotting. 
#                 If it is from a data frame, its value start from the first column.
#                 If it is from a ``GRanges`` object, its value start from meta-columns.
# -panel.fun self-defined function which will be applied on each sector. Please not it is different
#            from that in `circos.trackPlotRegion`. In this function, there are two arguments (``region`` and ``value``) plus ``...``.
#            In them, ``region`` is a two-column data frame with start positions and end positions in current genomic category (e.g. chromosome). 
#            ``value`` is a data frame which is derived from ``data`` but excluding the first three columns. Rows in ``value`` correspond to 
#            rows in ``region``. ``...`` is mandatory and is used to pass internal parameters to other functions.
# -... pass to `circos.trackPlotRegion`.
#
# == details
# The behavior would change depending on users' input data.
circos.genomicTrackPlotRegion = function(data, ylim = NULL, stack = FALSE, numeric.column = NULL, 
	panel.fun = function(region, value, ...)  {NULL}, ... ) {

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
			
			circos.trackPlotRegion(ylim = c(0.5, n + 0.5), panel.fun = function(x, y) {
						
					chr = get.current.chromosome()
					for(i in seq_len(n)) {
						l = data[[i]][[1]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], hline = i+0, i = i+0)
						}
					}

				}, ...)
		} else {
			if(is.null(numeric.column)) {
				numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
			}
			n = length(numeric.column)
			non.numeric.column = setdiff(seq_along(data[-(1:3)]), numeric.column)
			
			# if there is no numeric column
			if(n == 0) {
				circos.trackPlotRegion(ylim = c(0.5, 1 + 0.5), panel.fun = function(x, y) {
							
						chr = get.current.chromosome()
						l = data[[1]] == chr
						df = data[l, , drop = FALSE]
						i = 1
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)][non.numeric.column], hline = i+0, i = i+0)
						}

					}, ...)
			} else {
				circos.trackPlotRegion(ylim = c(0.5, n + 0.5), panel.fun = function(x, y) {
							
						chr = get.current.chromosome()
						for(i in seq_len(n)) {
							l = data[[1]] == chr
							df = data[l, , drop = FALSE]
							if(nrow(df)) {
								genomicPanelFun(df[2:3], df[-(1:3)][c(numeric.column[i], non.numeric.column)], hline = i+0, i = i+0, numeric.column = 1)
							}
						}

					}, ...)
			}
		}			
		
	} else {
	
		# numeric.column
		if(is.dataFrameList(data)) {
			if(is.null(numeric.column)) {
				if(length(numeric.column) == 1) {
					nuc = rep(list(numeric.column), length(data))
				} else if(length(numeric.column) == length(data)) {
					nuc = numeric.column
				} else {
					stop("Length of `numeric.column` should only be one or length of ``data`` if it is a region list.")
				}
			} else {
				nuc = lapply(data, function(gr) {
								numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
								if(length(numeric.column) == 0) {
									numeric(0)
								} else {
									numeric.column[1]
								}
							})
			}
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
			circos.trackPlotRegion(ylim = ylim, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					for(i in seq_along(data)) {
						l = data[[i]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							genomicPanelFun(df[2:3], df[-(1:3)], numeric.column = eval(nuc[[i]]), i = i + 0)
						}
					}
				}, ...)
		} else {
			circos.trackPlotRegion(ylim = ylim, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					df = data[data[[1]] == chr, , drop = FALSE]
					if(nrow(df)) {
						genomicPanelFun(df[2:3], df[-(1:3)], numeric.column = eval(nuc), i = 1)
					}
				}, ...)
		}
	}
	
}

# == title
# Which data that panel.fun is using
#
# == param
# -... invisible arguments that users do not need to care
#
# == details
# The function should only be put inside ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
#
# If ``stack`` is set to ``TRUE`` in `circos.genomicTrackPlotRegion`, the retured value
# indicates which stack the function will be applied to. If ``data`` is a list of regions, the value
# indicates which region data is being used. Please see the vignette to get a more clear explaination.
getI = function(...) {
	args = list(...)
	if(is.null(args$i)) {
		stop("Maybe you should call like `getI(...)`\n")
	}
	return(args$i)
}


# == title
# Add points to 
#
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

# == title
# add lines
#
# == param
# -region a data frame with two columns: start position and end position
# -value values corresponding to each genomic positions
# -numeric.column
# -sector.index
# -track.index
# -posTransform
# -col
# -lwd
# -lty
# -type
# -area
# -area.baseline
# -border
# -pt.col
# -cex
# -pch
# -... mysterious parameters
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
				col = col, lwd = lwd, lty = lty, type = "l",
				sector.index = sector.index, track.index = track.index )
		}
	} else if(nc == 1) {
		if(type == "segment") {
			for(i in seq_len(nr)) {
				circos.lines( c(region[i, 1], region[i, 2]), c(value[i, numeric.column], value[i, numeric.column]), 
					col = col, lwd = lwd, lty = lty, type = "l",
					sector.index = sector.index, track.index = track.index )
			}
		} else {
			circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], 
				col = col, lwd = lwd, lty = lty, type = type, 
				area = area, area.baseline = area.baseline, 
				border = border, pt.col = pt.col, cex = cex, pch = pch,
				sector.index = sector.index, track.index = track.index )
		}
	} else {
		for(i in seq_len(nc)) {
			if(type[i] == "segment") {
				for(k in seq_len(nr)) {
					circos.lines( c(region[k, 1], region[k, 2]), c(value[k, numeric.column[i] ], value[k, numeric.column[i] ]), 
						col = col[i], lwd = lwd[i], lty = lty[i], type = "l",
						sector.index = sector.index, track.index = track.index )
				}
			} else {
				circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column[i] ]], 
					col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
					area = area[i], area.baseline = area.baseline[i], 
					border = border[i], pt.col = pt.col[i], cex = cex[i], pch = pch[i],
					sector.index = sector.index, track.index = track.index )
			}
		}
	}
}	

# == title
# Draw rectangle-like grid
#
# == param
# -region a data frame with two columns: start position and end position
# -value values corresponding to each genomic positions
# -ytop
# -ybottom
# -ytop.column
# -ybottom.column
# -sector.index which sector to add text
# -track.index which track to add text
# -posTransform whether to do position transformation
# -col pass to `circos.rect`
# -border pass to `circos.rect`
# -lty pass to `circos.rect`
# -lwd pass to `circos.rect`
# -... mysterious parameters
circos.genomicRect = function(region, value, 
	ytop = NULL, ybottom = NULL, ytop.column = NULL, ybottom.column = NULL,
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	col = NA, border = "black", lty = par("lty"), lwd = par("lwd"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$hline)) {
		if(is.null(ytop)) ytop = args$hline + 0.5
		if(is.null(ybottom)) ybottom = args$hline - 0.5
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

# == title
# add text when drawing genomic graphics
#
# == param
# -region a data frame with two columns: start position and end position
# -value values corresponding to each genomic positions
# -labels labels of text corresponding to each genomic positions
# -labels.column index of column of the labels in ``value``
# -numeric.column index of column of the values in ``value``
# -sector.index which sector to add text
# -track.index which track to add text
# -posTransform whether to do position transformation
# -direction passing to `circos.text`
# -adj pass to `circos.text`
# -cex pass to `circos.text`
# -col pass to `circos.text`
# -font pass to `circos.font`
# -... mysterious parameters
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


# == title
# Add links from two sets of genomic positions
#
# == param
# -region1 a genomic data frame / an ``GRanges`` object
# -region2 a genomic data frame / an ``GRanges`` object
# -rou pass to `circos.link`
# -top.ratio pass to `circos.link`
# -col pass to `circos.link`, length can be either one or nrow of ``region1``
# -lwd pass to `circos.link`, length can be either one or nrow of ``region1``
# -lty pass to `circos.link`, length can be either one or nrow of ``region1``
# -border pass to `circos.link`, length can be either one or nrow of ``region1``
# -top.ratio.low pass to `circos.link`
#
# == details
# Of course, number of rows should be same in ``region1`` and ``region2``.
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
	
	top.ratio = .normalizeGraphicalParam(top.ratio, 1, nr, "top.ratio")
	col = .normalizeGraphicalParam(col, 1, nr, "col")
	lwd = .normalizeGraphicalParam(lwd, 1, nr, "lwd")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	border = .normalizeGraphicalParam(border, 1, nr, "border")
	top.ratio.low = .normalizeGraphicalParam(top.ratio.low, 1, nr, "top.ratio.low")
	
	for(i in seq_len(nrow(region1))) {
		circos.link(region1[i, 1], c(region1[i, 2], region1[i, 3]),
		            region2[i, 1], c(region2[i, 2], region2[i, 3]),
					rou = rou, top.ratio = top.ratio[i], col = col[i], lwd = lwd[i],
					lty = lty[i], border = border[i], top.ratio.low = top.ratio.low[i])
	}
}


# == title
# Add genomic position transformation lines between tracks
#
# == param
# -data a data frame or a ``GRanges`` object
# -track.height height of the track
# -posTransform genomic position transformation function, see `posTransform.default` for an example.
# -horizontalLine whether to draw horizontal lines which indicate width of each region
# -track.margin margin of tracks
# -type type of the transformation. ``default`` means position transformed track are located inside 
#       and ``reverse`` means position transformed track are located outside.
# -col color of lines, can be length of one or length of nrow of ``data``
# -lwd width of lines
# -lty style of lines
#
# == details
# There is one representative circumstances when such position transformation needs to be applied. 
# For example, there are two set of regions in a chromosome in which regions in one set are
# quite densely to each other and regions in other set are far from others. Heatmap or text is going
# to be drawn on the next track. If there is no position transformtion, heatmap or text for those
# dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a solution
# to transform original positions to new positions would help for the visualization. 
circos.genomicPosTransformLines = function(data, track.height = 0.1, posTransform = NULL, 
	horizontalLine = FALSE, track.margin = c(0, 0),
	type = c("default", "reverse"), col = "black", lwd = par("lwd"), lty = par("lty")) {
	
	data = normalizeToDataFrame(data)
	
	if(is.dataFrameList(data)) {
		stop("`data` can not be list of regions.\n")
	}
	
	nr = nrow(data)
	if(length(col) == 1) {
		col = rep(col, nr)
	}
	if(length(lwd) == 1) {
		lwd = rep(lwd, nr)
	}
	if(length(lty) == 1) {
		lty = rep(lty, nr)
	}
	
	o.track.margin = circos.par("track.margin")
	circos.par(track.margin = track.margin)
	
	type = match.arg(type)[1]
	
	if(type == "default") {
		circos.trackPlotRegion(data[[1]], ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			region_subset = data[l, , drop = FALSE]
			if(is.null(posTransform)) {
				region_new_subset = region_subset
			} else {
				region_new_subset = cbind(region_subset[[1]], posTransform(region_subset[2:3]))
			}
			
			for(i in seq_len(nrow(region_subset))) {
				if(horizontalLine) {
					circos.lines(c(region_subset[i, 2], region_subset[i, 3]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
					circos.lines(c(region_new_subset[i, 2], region_new_subset[i, 3]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
				}
				mid = (region_subset[i, 2] + region_subset[i, 3])/2
				mid_new = (region_new_subset[i, 2] + region_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(1, 2/3, 1/3, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			}
		})
	} else {
		circos.trackPlotRegion(data[[1]], ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			region_subset = data[l, , drop = FALSE]
			if(is.null(posTransform)) {
				region_new_subset = region_subset
			} else {
				region_new_subset = cbind(region_subset[[1]], posTransform(region_subset[2:3]))
			}
			
			for(i in seq_len(nrow(region_subset))) {
				if(horizontalLine) {
					circos.lines(c(region_subset[i, 2], region_subset[i, 3]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
					circos.lines(c(region_new_subset[i, 2], region_new_subset[i, 3]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
				}
				mid = (region_subset[i, 2] + region_subset[i, 3])/2
				mid_new = (region_new_subset[i, 2] + region_new_subset[i, 3])/2
				circos.lines(c(mid, mid, mid_new, mid_new), c(0, 1/3, 2/3, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			}
		})
	}
	
	circos.par(track.margin = o.track.margin)
}

# == title
# calculate and add genomic density
#
# == param
# -data A bed-file-like data frame or a ``GRanges`` object. It can also be a list
#       containing data frames and ``GRanges`` objects.
# -window.size pass to `genomicDensity`
# -overlap pass to `genomicDensity`
# -col  colors. It should be length of one. If ``data`` is a list, the length of ``col``
#       can also be the length of the list.
# -lwd  width of lines
# -lty  style of lines
# -type type of lines, see `circos.lines`
# -area see `circos.lines`
# -area.baseline see `circos.lines`
# -border see `circos.lines`
# -... pass to `circos.trackPlotRegion`
#
# == details
# This function is a high-level graphical function, and it will create a new track.
circos.genomicDensity = function(data, window.size = 10000000, overlap = TRUE, 
	col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l", area = TRUE, area.baseline = 0, border = NA, ...) {
	
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		data = list(data)
	}
	
	if(length(col) == 1) {
		col = rep(col, length(data))
	}
	if(length(lwd) == 1) {
		lwd = rep(lwd, length(data))
	}
	if(length(lty) == 1) {
		lty = rep(lty, length(data))
	}
	if(length(type) == 1) {
		type = rep(type, length(data))
	}
	if(length(area) == 1) {
		area = rep(area, length(data))
	}
	if(length(area.baseline) == 1) {
		area.baseline = rep(area.baseline, length(data))
	}
	if(length(border) == 1) {
		border = rep(border, length(data))
	}
	
	circos.genomicTrackPlotRegion(data, ylim = c(0, 1), panel.fun = function(region, value) {
		den = genomicDensity(region, window.size = window.size, overlap = overlap)
		i = getIStack(...)
		circos.genomicLines(den[1:2], den[3], col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
			border = border[i], area = area[i], area.baseline = area.baseline[i]) 
	}, ...)
}

# == title
# Calculate genomic region density
#
# == param
# -region Genomic positions at a single chromosome. It can be a data frame with two
#     columns which are start position and end position or an ``IRanges`` object.
# -window.size window size to calculate genomic density
# -overlap whether two neighbouring windows have half overlap
#
# == details
# It calculate the percent of each genomic windows that is covered by the input region.
#
# == values
# a data frame with three columns: start position, end position and percent of overlapping.
genomicDensity = function(region, window.size = 10000000, overlap = TRUE) {
	
	if(class(region) != "IRanges") {
		region = IRanges(start = region[[1]], end = region[[2]])
	}
	
	region = Reduce(sort(region))

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
	
	ol = findOverlap(windows, region)
	intersect = pintersect(windows[o[, 1]], region[o[, 2]])
	pct = (end(intersect) - start(intersect) + 1) / (end(windows[o[, 1]]) - start(windows[o[, 1]]) + 1)
	fn = paste(start(windows[o[, 1]]), end(windows[o[, 1]]), sep = ",")
	unified_pct = tapply(pct, fn, sum)
	y[names(unified_pct)] = unified_pct

	return(data.frame(start = s, end = e, pct = y))
}

# == title
# position transformation function
#
# == param
# -region Genomic positions at a single chromosome. It can be a data frame with two
#     columns which are start position and end position or an ``IRanges`` object.
#
# == details
# The default position transformation functions transforms position to be equally distributed
# along the chromosome. If users want to define their own transformation function, the requirement
# is that the returned value should be a data frame with two columns: transformed start position
# and transformed end position. The returned value should have same number of rows as the input one.
#
# For details why need to use position transformation, please refer to `circos.genomicPosTransformLines`.
posTransform.default = function(region) {
	xlim = get.cell.meta.data("xlim")
	segment = seq(xlim[1], xlim[2], length.out = nrow(region) + 1)
	return(data.frame(start = segment[-length(segment)], end = segment[-1]))
}

# == title
# Highlight a chromosome
#
# == param
# -chr chromosome name. Only allow single chromosome. It should be consistent with the sector index.
# -track.index a vector of track index that you want to highlight
# -col color for highlighting. Note the color should be semi-transparent.
# -border border of the lighlighted region
# -lwd width of borders
# -lty style of borders
# -padding padding for the highlighted region. It should contain four values
#          representing ratios of the width or height of the highlighted region
#
# == details
# You may use `circos.info` to find out index for all tracks.
#
# The function calls `draw.sector`.
highlight.chromosome = function(chr, track.index = seq_len(get.max.track.index()), 
	col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"), padding = c(0, 0, 0, 0)) {
	
	if(length(chr) != 1) {
		stop("`chr` can only be length 1.\n")
	}
	
	max.track.index = get.max.track.index()
	if(!all(track.index %in% seq_len(max.track.index))) {
		stop("`track.index` contains index that does not belong to available sectors.\n")
	}
	
	track.index = seq_len(max.track.index)[track.index %in% seq_len(max.track.index)]
	ts = continuousIndexSegment(track.index)
	
	for(i in seq_along(ts)) {
		track.index.vector = ts[[i]]
		start.degree = get.cell.meta.data("cell.start.degree", 1)
		end.degree = get.cell.meta.data("cell.end.degree", 1)
		rou1 = get.cell.meta.data("cell.top.radius", chr, track.index.vector[1])
		rou2 = get.cell.meta.data("cell.bottom.radius", chr, track.index.vector[length(track.index.vector)])
		
		d1 = end.degree - start.degree
		d2 = rou1 - rou2
		start.degree = start.degree - d1*padding[2]
		end.degree = end.degree + d1*padding[4]
		rou1 = rou1 + d2*padding[3]
		rou2 = rou2 + d2*padding[1]
		
		draw.sector(start.degree = start.degree, end.degree = end.degree, rou1 = rou1, rou2 = rou2, col = col, border = border, lwd = lwd, lty = lty)
	}
	
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
# Get current chromosome name
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

# == title
# Genomic rainfall plot
#
# == param
# -data A bed-file-like data frame or a ``GRanges`` object. It can also be a list
#       containing data frames and ``GRanges`` objects.
# -col  color of points. It should be length of one. If ``data`` is a list, the length of ``col``
#       can also be the length of the list.
# -pch  style of points
# -cex  size of points
# -... pass to `circos.trackPlotRegion`
#
# == details
# This is high-level graphical function, which mean, it will create a new track.
#
# Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis
# corresponds to the distance to neighbour regions. So if there is a drop-down on
# the plot, it means there is a cluster of regions at that area.
#
# On the plot, y-axis are log10-transformed.
circos.genomicRainfall = function(data, col = "black", pch = par("pch"), cex = par("cex"), ...) {
	
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
		df = rainfallTransform(region)
		i = getIStack(...)
		circos.genomicPoints(df[1:2], log10(df[3]+1), col = col[i], cex = cex[i], pch = pch[i], ...)
	}, ...)
	
}

# == title
# calculate inter-distance of genomic regions
#
# == param
# -region Genomic positions at a single chromosome. It can be a data frame with two
#     columns which are start positions and end positions or an ``IRanges`` object.
# -mode How to calculate inter-distance. For a region, there is a distance to the 
#       prevous region and also there is a distance to the next region. ``mode``
#       controls how to merge these two distances into one value.
#
# == values
# A data frame with three columns: start position, end position and distance
rainfallTransform = function(region, mode = c("min", "max", "mean")) {
	
	mode = match.arg(mode)[1]
	
	region = IRanges(start = region[[1]], end = region[[2]])
	region = as.data.frame(sort(region))
	n = nrow(region)
	dist = numeric(n)
		
	if(n < 2) {
		return(data.frame(start = numeric(0), end = numeric(0), dist = numeric(0)))
	}
		
	dist[1] = region[2, 1] - region[1, 2]
	dist[n] = region[n, 1] - region[n-1, 2]
			
	if(n > 2) {
		d1 = region[3:n, 1] - region[2:(n-1), 2]
		d2 = region[2:(n-1), 1] - region[1:(n-2), 2]
		if(mode == "min") {
			dist[2:(n-1)] = pmin(d1, d2)
		} else if(mode == "max") {
			dist[2:(n-1)] = pmax(d1, d2)
		} else {
			dist[2:(n-1)] = apply(cbind(d1, d2), 1, mean)
		}
	}
		
	dist = ifelse(dist < 0, 0, dist)
	
	return(data.frame(start = region[, 1], end = region[, 2], dist = dist))
}