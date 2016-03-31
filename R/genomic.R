
# == title
# Initialize the circos layout with an ideogram
#
# == param
# -cytoband  A path of the cytoband file or a data frame that already contains cytoband data. By default it is cytoband for hg19.
#            Pass to `read.cytoband`.
# -species Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically. If there is no cytoband for user's species,
#          it will keep on trying to download chromInfo file. Pass to `read.cytoband` and `read.chromInfo`.
# -chromosome.index subset of chromosomes, also used to re-set chromosome orders.
# -sort.chr Whether chromosome names should be sorted (first sort by numbers then by letters).
#           If ``chromosome.index`` is set, this argumetn is enforced to ``FALSE``
# -major.by     Increment of major ticks. Pass to `circos.genomicInitialize`.
# -plotType     Which tracks should be drawn. ``ideogram`` for ideogram rectangle, ``axis`` for genomic axis and ``labels`` for chromosome names.
#               If there is no ideogram for specified species, ``ideogram`` will be enforced to be excluded.
#               If it is set to ``NULL``, the function just initialize the plot but draw nothing.
# -track.height Height of the track which contains "axis" and "labels".
# -ideogram.height Height of the ideogram track
# -...    Pass to `circos.initialize`
#
# == details
# The function will initialize the circos plot in which each sector corresponds to a chromosome. You can control the order of 
# chromosomes by ``chromosome.index`` or by ``sort.chr``, or by setting a special format of ``cytoband`` (please refer to `read.cytoband` 
# to find out how to control a proper ``cytoband``).
#
# The function finally pass data to `circos.genomicInitialize` to initialize the circos plot.
#
# The style of ideogram is almost fixed, but you can customize it with your self-sefined code. Refer to vignette for demonstration.
circos.initializeWithIdeogram = function(cytoband = paste(system.file(package = "circlize"),
	"/extdata/cytoBand.txt", sep=""), species = NULL, sort.chr = TRUE,
	chromosome.index = NULL, major.by = NULL,
	plotType = c("ideogram", "axis", "labels"), 
	track.height = 0.05, ideogram.height = 0.05, ...) {
	

	# proper order will be returned depending on cytoband and sort.chr
	e = try(cytoband <- read.cytoband(cytoband, species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
	if(class(e) == "try-error" && !is.null(species)) {  # if species is defined
		e2 = try(cytoband <- read.chromInfo(species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
		if(class(e2) == "try-error") {
			message(e)
			message(e2)
			stop("Cannot download either cytoband or chromInfo file from UCSC.\n")
		} else {
			message("Downloading cytoBand file from UCSC failed. Use chromInfo file instead.\nNote ideogram track will be removed from the plot.")
			plotType = setdiff(plotType, "ideogram")

			# because in chromInfo file, there are also many short scaffold
			if(is.null(chromosome.index)) {
	            chromInfo = read.chromInfo(species = species)
	            chr_len = sort(chromInfo$chr.len, decreasing = TRUE)

	            # sometimes there are small scaffold
	            i = which(chr_len[seq_len(length(chr_len)-1)] / chr_len[seq_len(length(chr_len)-1)+1] > 5)[1]
	            if(length(i)) {
	                chromosome = chromInfo$chromosome[chromInfo$chromosome %in% names(chr_len[chr_len >= chr_len[i]])]
	            } else {
	                chromosome = chromInfo$chromosome
	            }
	            cytoband = read.chromInfo(species = species, chromosome.index = chromosome, sort.chr = sort.chr)
	        } 
		}
	} else if(class(e) == "try-error") {
		stop(e)
	}
	df = cytoband$df
	chromosome = cytoband$chromosome
	
	if(is.null(chromosome.index)) {
		chromosome.index = chromosome
	}

	# here df[[1]] is quite important, should be re-factered
	df[[1]] = factor(as.vector(df[[1]]), levels = chromosome.index)
	
	# sn for sector names, but not for sector index
	sn = unique(as.vector(df[[1]]))

	# we do not need 'chr' prefix if it exits, it holds too much space.
	sn = gsub("chr", "", sn)
	
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(o.cell.padding[1], 0, o.cell.padding[3], 0))
	
	circos.genomicInitialize(df, sector.names = sn, major.by = major.by, plotType = plotType, track.height = track.height, ...)

	if(any(plotType %in% "ideogram")) {
		circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = ideogram.height,
			panel.fun = function(region, value, ...) {
				col = cytoband.col(value[[2]])
				circos.genomicRect(region, value, ybottom = 0, ytop = 1, col = col, border = NA, ...)
				xlim = get.cell.meta.data("xlim")
				circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
			}, cell.padding = c(0, 0, 0, 0)
		)
	}
}

# == title
# Initialize circos plot with any genomic data
#
# == param
# -data         A data frame containing genomic data.
# -sector.names Labels for each sectors which will be drawn along each sector. It will not modify values of sector index.
# -major.by     Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).
# -plotType     If it is not ``NULL``, there will create a new track containing axis and names for sectors.
#               This argument controls which part should be drawn, ``axis`` for genomic axis and ``labels`` for chromosome names
# -tickLabelsStartFromZero Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.
# -track.height If ``PlotType`` is not ``NULL``, height of the annotation track.
# -...          Pass to `circos.initialize`
#
# == details
# The function will initialize circos plot from genomic data. If ``plotType`` is set with value in ``axis`` or ``labels``, there will
# create a new track.
#
# The order of sectors related to data structure of ``data``. If the first column in ``data`` is a factor, the order of sectors
# is ``levels(data[[1]])``; If the first column is just a simple vector, the order of sectors is ``unique(data[[1]]``.
#
# For more details on initializing genomic plot, please refer to the vignettes.
circos.genomicInitialize = function(data, sector.names = NULL, major.by = NULL,
	plotType = c("axis", "labels"), tickLabelsStartFromZero = TRUE, 
	track.height = 0.05, ...) {
	
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
	
	op = circos.par("cell.padding")
	ow = circos.par("points.overflow.warning")
	circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
	circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, x2), ...)
	
	# axis and chromosome names
	if(any(plotType %in% c("axis", "labels"))) {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height,
			panel.fun = function(region, value, ...) {
				sector.index = get.cell.meta.data("sector.index")
				xlim = get.cell.meta.data("xlim")
				
				if(tickLabelsStartFromZero) {
					offset = xlim[1]
					if(is.null(major.by)) {
						xlim = get.cell.meta.data("xlim")
						major.by = .default.major.by()
					}
					major.at = seq(xlim[1], xlim[2], by = major.by)
					major.at = c(major.at, major.at[length(major.at)] + major.by)
					
					if(major.by > 1e6) {
						major.tick.labels = paste((major.at-offset)/1000000, "MB", sep = "")
					} else if(major.by > 1e3) {
						major.tick.labels = paste((major.at-offset)/1000, "KB", sep = "")
					} else {
						major.tick.labels = paste((major.at-offset), "bp", sep = "")
					}
					
				} else {
					if(is.null(major.by)) {
						xlim = get.cell.meta.data("xlim")
						major.by = .default.major.by()
					}
					major.at = seq(floor(xlim[1]/major.by)*major.by, xlim[2], by = major.by)
					major.at = c(major.at, major.at[length(major.at)] + major.by)
					
					if(major.by > 1e6) {
						major.tick.labels = paste(major.at/1000000, "MB", sep = "")
					} else if(major.by > 1e3) {
						major.tick.labels = paste(major.at/1000, "KB", sep = "")
					} else {
						major.tick.labels = paste(major.at, "bp", sep = "")
					}
				}
				
			
				if(all(c("axis", "labels") %in% plotType)) {
					circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, labels.cex = 0.3*par("cex"), labels.facing = "clockwise", major.tick.percentage = 0.2)
					circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], cex = par("cex"), adj = c(0.5, 0), niceFacing = TRUE)
				} else if("labels" %in% plotType) {
					circos.text(mean(xlim), 0, labels = sector.names[sector.index], cex = par("cex"), adj = c(0.5, 0), niceFacing = TRUE)
				}
			}
		)
	}
	
	
	circos.par("cell.padding" = op, "points.overflow.warning" = ow)
	return(invisible(NULL))
}

# == title
# Create a track for genomic graphics
#
# == param
# -data A bed-file-like data frame or a list of data frames
# -ylim If it is ``NULL``, the value will be calculated from data. If ``stack`` is set to ``TRUE``, this value is ignored.
# -stack whether to plot in a "stack" mode.
# -numeric.column Columns of numeric values in ``data`` that will be used for plotting. 
#                 If ``data`` is a data frame list, ``numeric.column`` should be either length of one or length of ``data``.
#                 If value of ``numeric.column`` is not set, its value will depend on the structure of ``data``.
#                 If ``data`` is a data frame, the default value for ``numeric.column`` is all the numeric column starting from the fourth column.
#                 If ``data`` is a list of data frame, the default value for ``numeric.column`` is a vector which have the same length as ``data``
#                 and the value in default ``numeric.column`` is the index of the first numeric column in corresponding data frame.
# -jitter Numeric. Only works for adding points in ``circos.genomicTrackPlotRegion`` under ``stack`` mode
# -panel.fun Self-defined function which will be applied on each sector. Please not it is different
#            from that in `circos.trackPlotRegion`. In this function, there are two arguments (``region`` and ``value``) plus ``...``.
#            In them, ``region`` is a two-column data frame with start positions and end positions in current genomic category (e.g. chromosome). 
#            ``value`` is a data frame which is derived from ``data`` but excluding the first three columns. Rows in ``value`` correspond to 
#            rows in ``region``. ``...`` is mandatory and is used to pass internal parameters to other functions. The definition of
#            ``value`` will be different according to different input data (data frame or list of data frame) and different settings (stacked or not), 
#            please refer to 'details' section and vignettes to detailed explanation.
# -... Pass to `circos.trackPlotRegion`.
#
# == details
# Similar as `circos.trackPlotRegion`, users can add customized graphics by ``panel.fun``, but the behaviour of ``panel.fun``
# will change depending on users' input data and ``stack`` setting.
#
# When ``data`` is a single data frame, ``region`` in ``panel.fun`` is a data frame containing the second and third column in ``data`` in 'current` genomic category (e.g. current chromosome).
# ``value`` is also a data frame containing columns in ``data`` excluding the first three columns.
#
# When ``data`` is a list containing data frames, ``panel.fun`` will be applied iteratively on each data frame, thus, 
# ``region`` is extracted from the data frame which is in the current iteration. For example, if ``data`` contains two data frames, ``panel.fun``
# will be applied with the first data frame in current chromosome and then applied with the second data frame in the same chromosome.
#
# If ``stack`` is set to ``TRUE``, ``ylim`` will be re-defined. in ``stack`` mode, the y-axis will be splitted into several part
# with equal height and graphics will be drawn on each 'horizontal' lines (y = 1, 2, ...). In this case:
#
# When ``data`` is a single data frame containing one or more numeric columns, each numeric column defined in ``numeric.column`` will be treated as a single unit. 
# ``ylim`` is re-defined to ``c(0.5, n+0.5)`` in which ``n`` is number of numeric columns. ``panel.fun`` will be applied iteratively on each numeric column. In each
# iteration, in ``panel.fun``, ``region`` is still the genomic regions in current genomic category, but ``value`` contains current numeric column plus all non-numeric columns.
# Under ``stack`` mode, in ``panel.fun``, all low-level genomic graphical functions will draw on the 'horizontal line' ``y = i`` in which ``i`` is the index of current numeric column 
# and the value of ``i`` can be obtained by `getI`.
#
# When ``data`` is a list containing data frames, each data frame will be treated as a single unit. The situation is quite similar as described in previous paragraph.
# ``ylim`` is re-defined to ``c(0.5, n+0.5)`` in which ``n`` is number of data frames. ``panel.fun`` will be applied iteratively on each data frame. In each
# iteration, in ``panel.fun``, ``region`` is still the genomic regions in current genomic category, and ``value`` contains columns in current data frame excluding the first three columns.
# Under ``stack`` mode, in ``panel.fun``, all low-level genomic graphical functions will draw on the 'horizontal line' ``y = i`` in which ``i`` is the index of current data frame.
#
# Being different from ``panel.fun`` in `circos.trackPlotRegion`, there should be an additional argument ``...`` in ``panel.fun``. This additional
# argument is used to pass hidden values to low-level graphical functions. So if you are using functions like ``circos.genomicPoints``, you should also
# add ``...`` as an additional argument into ``circos.genomicPoints``.
circos.genomicTrackPlotRegion = function(data = NULL, ylim = NULL, stack = FALSE,
	numeric.column = NULL, jitter = 0,
	panel.fun = function(region, value, ...)  {NULL}, ... ) {
	
	if(is.null(data)) {
		all.sector.index = get.all.sector.index()
		data = data.frame(all.sector.index,
		           rep(0, length(all.sector.index)),
				   rep(0, length(all.sector.index)))
	}
	
	# re-define panel.fun
	genomicPanelFun = panel.fun

	# now `data` is either a data frame or a list of data frame
	data = normalizeToDataFrame(data)

	# excluding the first three columns
	if(!is.null(numeric.column)) {
		if(is.numeric(numeric.column)) {
			numeric.column = numeric.column - 3
		}
		if(any(numeric.column < 0)) {
			stop("Wrong value in `numeric.column`, they should be larger than 3 or character index.\n")
		}
	}
	
	# auto calcualte numeric column
	if(is.dataFrameList(data)) {
		if(!is.null(numeric.column)) {
			if(length(numeric.column) == 1) {
				numeric.column = rep(list(numeric.column), length(data))
			} else if(length(numeric.column) == length(data)) {
				
			} else {
				stop("Length of `numeric.column` should only be one or length of ``data`` if it is a list of data frames.")
			}
			for(i in seq_along(data)) {
				if(!is.numeric(data[[i]][-(1:3)][numeric.column[i]])) {
					stop("Some of your `numeric.column` are not numeric.\n")
				}
			}
		} else {
			numeric.column = sapply(data, function(gr) {
								nc = which(as.logical(sapply(gr[-(1:3)], is.numeric)))
								if(length(nc) == 0) {
									NA
								} else {
									nc[1]
								}
							})
		}
		
	} else {
		# check numeric.column
		if(is.null(numeric.column)) {
			numeric.column = which(as.logical(sapply(data[-(1:3)], is.numeric)))
		} else {
			if(!all(sapply(data[-(1:3)][numeric.column], is.numeric))) {
				stop("Some of your `numeric.column` are not numeric.\n")
			}
		}
	}
	
	args = formals(genomicPanelFun)
	if(!(length(args) == 3 && names(args)[3] == "...")) {
		stop("The `panel.fun` need a third argument `...` to pass special parameters to graphical functions.\n")
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
							.param = new.env()
							assign("i", i, envir = .param)
							assign("stack", TRUE, envir = .param)
							assign("jitter", jitter, envir = .param)
							if(!is.null(numeric.column) && !is.na(numeric.column[i])) {
								assign("numeric.column", numeric.column[i], envir = .param)
							}
							genomicPanelFun(df[2:3], df[-(1:3)], .param = .param)
						}
					}

				}, ...)
		} else {
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
							.param = new.env()
							assign("i", i, envir = .param)
							assign("stack", TRUE, envir = .param)
							assign("jitter", jitter, envir = .param)
							genomicPanelFun(df[2:3], df[-(1:3)][non.numeric.column], .param = .param)
						}

					}, ...)
			} else {
				circos.trackPlotRegion(ylim = c(0.5, n + 0.5), panel.fun = function(x, y) {
							
						chr = get.current.chromosome()
						for(i in seq_len(n)) {
							l = data[[1]] == chr
							df = data[l, , drop = FALSE]
							if(nrow(df)) {
								.param = new.env()
								assign("i", i, envir = .param)
								assign("stack", TRUE, envir = .param)
								assign("numeric.column", 1, envir = .param)
								assign("jitter", jitter, envir = .param)
								genomicPanelFun(df[2:3], df[-(1:3)][c(numeric.column[i], non.numeric.column)], .param = .param)
							}
						}

					}, ...)
			}
		}			
		
	} else {
		# auto calculate ylim
		if(is.null(ylim)) {
			if(is.dataFrameList(data)) {
				ylim = range(unlist(lapply(seq_along(data), function(i) {
					gr = data[[i]]
					if(is.na(numeric.column[i])) {
						stop("There is no numeric column in one of your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.\n")
					}
					range(unlist(lapply(gr[-(1:3)][ numeric.column[i] ], range)))
				})))
			} else {
				if(length(numeric.column) == 0) {
					stop("There is no numeric column in your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.\n")
				}
				ylim = range(unlist(lapply(data[-(1:3)][numeric.column], range)))
			}
		}
		if(is.dataFrameList(data)) {
			circos.trackPlotRegion(ylim = ylim, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					for(i in seq_along(data)) {
						l = data[[i]] == chr
						df = data[[i]][l, , drop = FALSE]
						if(nrow(df)) {
							.param = new.env()
							assign("i", i, envir = .param)
							if(!is.na(numeric.column[i])) {
								assign("numeric.column", numeric.column[i], envir = .param)
							}
							genomicPanelFun(df[2:3], df[-(1:3)], .param = .param)
						}
					}
				}, ...)
		} else {
			circos.trackPlotRegion(ylim = ylim, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					df = data[data[[1]] == chr, , drop = FALSE]
					if(nrow(df)) {
						.param = new.env()
						assign("i", 1, envir = .param)
						if(length(numeric.column)) {
							assign("numeric.column", numeric.column, envir = .param)
						}
						genomicPanelFun(df[2:3], df[-(1:3)], .param = .param)
					}
				}, ...)
		}
	}
	
}

# == title
# Create a track for genomic graphics
#
# == param
# -... pass to `circos.genomicTrackPlotRegion`
#
# == details
# shortcut function of `circos.genomicTrackPlotRegion`.
circos.genomicTrack = function(...) {
	circos.genomicTrackPlotRegion(...)
}


# == title
# Which data that ``panel.fun`` is using
#
# == param
# -... Invisible arguments that users do not need to care
#
# == details
# The function should only be put inside ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
#
# If ``stack`` is set to ``TRUE`` in `circos.genomicTrackPlotRegion`, the returned value
# indicates which stack the function will be applied to.
#
# If ``data`` is a list of data frames, the value
# indicates which data frame is being used. Please see the vignette to get a more clear explanation.
getI = function(...) {
	args = list(...)
	if(is.null(args$.param)) {
		stop("Maybe you should call like `getI(...)`\n")
	}
	.param = args$.param
	return(.param$i)
}


# == title
# Add points to a plotting region, specifically for genomic graphics
#
# ==param
# -region A data frame contains 2 columns which correspond to start positions and end positions
# -value  A data frame contains values and other information
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, the whole numeric columns in ``value`` will be taken.
# -sector.index Pass to `circos.points`
# -track.index Pass to `circos.points`
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explanation
# -col color of points. If there is only one numeric column, the length of ``col`` can be either one or number of rows of ``region``.
#      If there are more than one numeric column, the length of ``col`` can be either one or number of numeric columns.
#      Pass to `circos.points`
# -pch Type of points. Settings are similar as ``col``. Pass to `circos.points`
# -cex Size of points. Settings are similar as ``col``. Pass to `circos.points`
# -... Mysterious parameters
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
circos.genomicPoints = function(region, value, numeric.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	pch = par("pch"), col = par("col"), cex = par("cex"), ...) {
	
	nr = nrow(region)
	
	if(is.atomic(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.atomic(value) && length(value) == nr) {
		value = data.frame(value = value)
	}
	if(!is.data.frame(value)) stop("`value` should be a data frame.\n")
	
	args = list(...)
	if(!is.null(args$.param)) {
		.param = args$.param
		if(!is.null(.param$stack)) {
			if(.param$stack && is.null(numeric.column)) {
				if(is.null(.param$jitter)) {
					value = data.frame(hline = rep(.param$i, nr))
				} else {
					value = data.frame(hline = rep(.param$i, nr) + (runif(nr) - 0.5)*abs(.param$jitter))
				}
				numeric.column = 1
			}
		} else if(!is.null(.param$numeric.column) && is.null(numeric.column)) {
			numeric.column = .param$numeric.column
		}
	}
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
		numeric.column = 1
	} else if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
		numeric.column = 1
	}
	
	if(!is.null(posTransform)) {
		region = posTransform(region)
	}
	
	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(value, is.numeric)))
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
# Add lines to a plotting region, specifically for genomic graphics
#
# == param
# -region A data frame contains 2 column which correspond to start position and end position
# -value  A data frame contains values and other information
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, the whole numeric columns in ``value`` will be taken.
# -sector.index Pass to `circos.lines`
# -track.index Pass to `circos.lines`
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explaination
# -col col of lines/areas. If there are more than one numeric column, the length of ``col`` can be either one or number of numeric columns.
#      If there is only one numeric column and type is either ``segment`` or ``h``, 
#      the length of ``col`` can be either one or number of rows of ``region``.
#      pass to `circos.lines`
# -lwd Settings are similar as ``col``. Pass to `circos.lines`
# -lty Settings are similar as ``col``. Pass to `circos.lines`
# -type There is an additional option ``segment`` which plot segment lines from start position to end position. Settings are similar as ``col``. Pass to `circos.lines`. 
# -area Settings are similar as ``col``. Pass to `circos.lines`
# -area.baseline Deprecated, use ``baseline`` instead.
# -baseline Settings are similar as ``col``. Pass to `circos.lines`
# -border Settings are similar as ``col``. Pass to `circos.lines`
# -pt.col Settings are similar as ``col``. Pass to `circos.lines`
# -cex Settings are similar as ``col``. Pass to `circos.lines`
# -pch Settings are similar as ``col``. Pass to `circos.lines`
# -... mysterious parameters
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
circos.genomicLines = function(region, value, numeric.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l",
    area = FALSE, area.baseline = NULL, border = "black", baseline = "bottom",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"), ...) {
	
	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning("`area.baseline` is deprecated, please use `baseline` instead.\n")
	}
	
	nr = nrow(region)
	if(is.atomic(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.atomic(value) && length(value) == nr) {
		value = data.frame(value = value)
	}
	if(!is.data.frame(value)) stop("`value` should be a data frame.\n")
	
	args = list(...)
	if(!is.null(args$.param)) {
		.param = args$.param
		if(!is.null(.param$stack)) {
			if(.param$stack && is.null(numeric.column)) {
				value = data.frame(hline = rep(.param$i, nr))
				numeric.column = 1
				type = rep("segment", length(type))
			}
		} else if(!is.null(.param$numeric.column) && is.null(numeric.column)) {
			numeric.column = .param$numeric.column
		}
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
		numeric.column = which(as.logical(sapply(value, is.numeric)))
		if(length(numeric.column) == 0) {
			stop("Cannot find numeric column.\n")
		}
	}

	nc = length(numeric.column)
	
	if(all(type %in% c("h", "segment"))) {
		col = .normalizeGraphicalParam(col, nc, nr, "col")
		lwd = .normalizeGraphicalParam(lwd, nc, nr, "col")
		lty = .normalizeGraphicalParam(lty, nc, nr, "col")
	} else {
		col = .normalizeGraphicalParam(col, nc, 1, "col")
		lwd = .normalizeGraphicalParam(lwd, nc, 1, "col")
		lty = .normalizeGraphicalParam(lty, nc, 1, "col")
	}
	pt.col = .normalizeGraphicalParam(pt.col, nc, 1, "col")
	cex = .normalizeGraphicalParam(cex, nc, 1, "col")
	pch = .normalizeGraphicalParam(pch, nc, 1, "col")
	type = .normalizeGraphicalParam(type, nc, 1, "col")
	area = .normalizeGraphicalParam(area, nc, 1, "col")
	baseline = .normalizeGraphicalParam(baseline, nc, 1, "col")
	border = .normalizeGraphicalParam(border, nc, 1, "col")
	
	if(!is.null(args$hline)) {
		# for(i in seq_len(nr)) {
		# 	circos.lines( c(region[i, 1], region[i, 2]), c(value[i, numeric.column], value[i, numeric.column]), 
		# 		col = col, lwd = lwd, lty = lty, type = "l",
		# 		sector.index = sector.index, track.index = track.index )
		# }
		circos.segments( region[, 1], value[, numeric.column], region[, 2], value[, numeric.column], 
				col = col, lwd = lwd, lty = lty, 
				sector.index = sector.index, track.index = track.index )
	} else if(nc == 1) {
		if(type == "segment") {
			# for(i in seq_len(nr)) {
			# 	circos.lines( c(region[i, 1], region[i, 2]), c(value[i, numeric.column], value[i, numeric.column]), 
			# 		col = col[i], lwd = lwd[i], lty = lty[i], type = "l",
			# 		sector.index = sector.index, track.index = track.index )
			# }
			circos.segments( region[, 1], value[, numeric.column], region[, 2], value[, numeric.column], 
					col = col, lwd = lwd, lty = lty, 
					sector.index = sector.index, track.index = track.index )
		} else {
			circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], 
				col = col, lwd = lwd, lty = lty, type = type, 
				area = area, baseline = baseline, 
				border = border, pt.col = pt.col, cex = cex, pch = pch,
				sector.index = sector.index, track.index = track.index )
		}
	} else {
		for(i in seq_len(nc)) {
			if(type[i] == "segment") {
				# for(k in seq_len(nr)) {
				# 	circos.lines( c(region[k, 1], region[k, 2]), c(value[k, numeric.column[i] ], value[k, numeric.column[i] ]), 
				# 		col = col[i], lwd = lwd[i], lty = lty[i], type = "l",
				# 		sector.index = sector.index, track.index = track.index )
				# }
				circos.segments( region[, 1], value[, numeric.column[i] ], region[, 2], value[, numeric.column[i] ], 
						col = col[i], lwd = lwd[i], lty = lty[i], type = "l",
						sector.index = sector.index, track.index = track.index )
			} else {
				circos.lines( (region[[1]] + region[[2]])/2, value[[ numeric.column[i] ]], 
					col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
					area = area[i], baseline = baseline[i], 
					border = border[i], pt.col = pt.col[i], cex = cex[i], pch = pch[i],
					sector.index = sector.index, track.index = track.index )
			}
		}
	}
}	

# == title
# Draw rectangle-like grid, specifically for genomic graphics
#
# == param
# -region A data frame contains 2 column which correspond to start position and end position
# -value  A data frame contains values and other information
# -ytop A vector or a single value indicating top position of rectangles
# -ybottom A vector or a single value indicating bottom position of rectangles
# -ytop.column If ``ytop`` is in ``value``, the index of the column
# -ybottom.column If ``ybottom`` is in ``value``, the index of the column
# -sector.index Pass to `circos.rect`
# -track.index Pass to `circos.rect`
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explaination
# -col The length of ``col`` can be either one or number of rows of ``region``. Pass to `circos.rect`
# -border Settings are similar as ``col``. Pass to `circos.rect`
# -lty Settings are similar as ``col``. Pass to `circos.rect`
# -lwd Settings are similar as ``col``. Pass to `circos.rect`
# -... Mysterious parameters
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
circos.genomicRect = function(region, value = NULL, 
	ytop = NULL, ybottom = NULL, ytop.column = NULL, ybottom.column = NULL,
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	col = NA, border = "black", lty = par("lty"), lwd = par("lwd"), ...) {
	
	nr = nrow(region)
	
	args = list(...)
	if(!is.null(args$.param)) {
		.param = args$.param
		if(!is.null(.param$stack)) {
			if(.param$stack) {
				if(is.null(ytop)) ytop = .param$i + 0.5
				if(is.null(ybottom)) ybottom = .param$i - 0.5
			}
		}
	}
	
	if(is.atomic(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.atomic(value) && length(value) == nr) {
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
	if(is.matrix(value)) value = as.data.frame(value)
	if(!is.data.frame(value)) stop("`value` should be a data frame.\n")
	
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
	
	# for(i in seq_len(nr)) {
	# 	circos.rect(region[i, 1], value[i, ybottom.column], region[i, 2], value[i, ytop.column],
	# 	            sector.index = sector.index, track.index = track.index,
	# 				col = col[i], border = border[i], lwd = lwd[i], lty = lty[i])
	# }
	circos.rect(region[, 1], value[, ybottom.column], region[, 2], value[, ytop.column],
		            sector.index = sector.index, track.index = track.index,
					col = col, border = border, lwd = lwd, lty = lty)

}

# == title
# Draw text in a cell, specifically for genomic graphics
#
# == param
# -region A data frame contains 2 column which correspond to start position and end position
# -value  A data frame contains values and other information
# -y A vector or a single value indicating position of text.
# -labels Labels of text corresponding to each genomic positions
# -labels.column If labels are in ``value``, index of column in ``value``
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, only the first numeric columns in ``value`` will be taken.
# -sector.index Pass to `circos.rect`
# -track.index Pass to `circos.rect`
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explanation
# -facing Passing to `circos.text`. Settings are similar as ``col`` 
# -niceFacing   Should the facing of text be adjusted to fit human eyes?
# -direction Deprecated, use ``facing`` instead. 
# -adj Pass to `circos.text`. Settings are similar as ``col``
# -cex Pass to `circos.text`. Settings are similar as ``col``
# -col Pass to `circos.text`. The length of ``col`` can be either one or number of rows of ``region``.
# -font Pass to `circos.text`. Settings are similar as ``col``
# -padding pass to ``posTransform`` if it is set as `posTransform.text`
# -... Mysterious parameters
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
circos.genomicText = function(region, value = NULL, y = NULL, labels = NULL, labels.column = NULL,
	numeric.column = NULL, sector.index = get.cell.meta.data("sector.index"), 
	track.index = get.cell.meta.data("track.index"), posTransform = NULL, 
	direction = NULL, facing = "inside", niceFacing = FALSE,
	adj = par("adj"), cex = 1, col = "black", font = par("font"), padding = 0, ...) {
	
	if(!is.null(direction)) {
		facing = direction
		warning("`direction` is deprecated, please use `facing` instead.\n")
	}
	
	nr = nrow(region)
	
	if(is.vector(value) && !is.list(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
		numeric.column = 1
	}
	if(is.vector(value) && !is.list(value) && length(value) == nr) {
		value = data.frame(value = value)
		numeric.column = 1
	}
	
	args = list(...)
	if(!is.null(args$.param)) {
		.param = args$.param
		if(!is.null(.param$stack)) {
			if(.param$stack && is.null(numeric.column)) {
				value = data.frame(hline = rep(.param$i, nr))
				numeric.column = 1
			}
		} else if(!is.null(.param$numeric.column) && is.null(numeric.column)) {
			numeric.column = .param$numeric.column
		}
	}
	
	if(!is.null(y)) {
		if(length(y) == 1) {
			y = rep(y, nr)
		}
		value = cbind(value, y)
		numeric.column = ncol(value)
	}
	if(is.matrix(value)) value = as.data.frame(value)
	if(!is.data.frame(value)) stop("`value` should be a data frame.\n")

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

	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(value, is.numeric)))
		if(length(numeric.column) == 0) {
			stop("Cannot find numeric column.\n")
		}
		numeric.column = numeric.column[1]
	}
	
	if(length(numeric.column) > 1) {
		stop("You can only have one numeric column.\n")
	}
	
	if(!is.null(posTransform)) {
	
		# check settings when it is text-specific transformation
		if(identical(posTransform, posTransform.text)) {
			if(! facing %in% c("clockwise", "reverse.clockwise")) {
				stop("Only support `facing` in c('clockwise', 'reverse.clockwise') if `posTransform` is `posTransform.text`.\n")
			}
			region = posTransform(region, value[[ numeric.column ]], value[[labels.column]], cex, font, padding = padding)
		} else {
			region = posTransform(region)
		}
		
	}
	
	nc = length(numeric.column)

	col = .normalizeGraphicalParam(col, nc, nr, "col")
	cex = .normalizeGraphicalParam(cex, nc, nr, "cex")
	font = .normalizeGraphicalParam(font, nc, nr, "font")

	circos.text( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], value[[labels.column]],
		facing = facing, niceFacing = niceFacing, adj = adj, cex = cex, col = col, font = font,
		sector.index = sector.index, track.index = track.index)

}


# == title
# Add links from two sets of genomic positions
#
# == param
# -region1 A genomic data frame
# -region2 A genomic data frame
# -rou Pass to `circos.link`
# -rou1 Pass to `circos.link`
# -rou2 Pass to `circos.link`
# -col Pass to `circos.link`, length can be either one or nrow of ``region1``
# -lwd Pass to `circos.link`, length can be either one or nrow of ``region1``
# -lty Pass to `circos.link`, length can be either one or nrow of ``region1``
# -border Pass to `circos.link`, length can be either one or nrow of ``region1``
# -... Pass to `circos.link`
#
# == details
# Of course, number of rows should be same in ``region1`` and ``region2``.
#
# If you want to have more controls on links, please use `circos.link` directly.
circos.genomicLink = function(region1, region2, 
	rou = get_most_inside_radius(), rou1 = rou, rou2 = rou,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = col, ...) {
	
	region1 = normalizeToDataFrame(region1, sort = FALSE)
	region2 = normalizeToDataFrame(region2, sort = FALSE)
	
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
	
	rou1 = .normalizeGraphicalParam(rou1, 1, nr, "rou")
	rou2 = .normalizeGraphicalParam(rou2, 1, nr, "rou")
	col = .normalizeGraphicalParam(col, 1, nr, "col")
	lwd = .normalizeGraphicalParam(lwd, 1, nr, "lwd")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	border = .normalizeGraphicalParam(border, 1, nr, "border")

	for(i in seq_len(nr)) {
		if(region1[i, 2] == region1[i, 3]) {
			point1 = region1[i, 2]
		} else {
			point1 = c(region1[i, 2], region1[i, 3])
		}
		if(region2[i, 2] == region2[i, 3]) {
			point2 = region2[i, 2]
		} else {
			point2 = c(region2[i, 2], region2[i, 3])
		}
		circos.link(region1[i, 1], point1,
		            region2[i, 1], point2,
					rou1 = rou1[i], rou2 = rou2[i], col = col[i], lwd = lwd[i],
					lty = lty[i], border = border[i], ...)
	}
}


# == title
# Add genomic position transformation lines between tracks
#
# == param
# -data A data frame containing genomic data
# -track.height Height of the track
# -posTransform Genomic position transformation function, see `posTransform.default` for an example.
# -horizontalLine Whether to draw horizontal lines which indicate region width 
# -track.margin Margin of tracks
# -direction Type of the transformation. ``inside`` means position transformed track are located inside 
#       and ``outside`` means position transformed track are located outside.
# -col Color of lines, can be length of one or nrow of ``data``
# -lwd Width of lines
# -lty Style of lines
# -... pass to `circos.trackPlotRegion`
#
# == details
# There is one representative situation when such position transformation needs to be applied. 
# For example, there are two sets of regions in a chromosome in which regions in one set regions are
# quite densely to each other and regions in other set are far from others. Heatmap or text is going
# to be drawn on the next track. If there is no position transformation, heatmap or text for those
# dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a way
# to transform original positions to new positions would help for the visualization. 
circos.genomicPosTransformLines = function(data, track.height = 0.1, posTransform = NULL, 
	horizontalLine = c("none", "top", "bottom", "both"), track.margin = c(0, 0),
	direction = c("inside", "outside"), col = "black", lwd = par("lwd"),
    lty = par("lty"), ...) {
	
	horizontalLine = match.arg(horizontalLine)[1]

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
	
	if(direction[1] == "default") direction = "outside"
	if(direction[1] == "reverse") direction = "inside"
	direction = match.arg(direction)[1]
	
	if(direction == "inside") {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height, panel.fun = function(region, value, ...) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			if(!is.null(posTransform)) {
				if(is.function(posTransform)) {
					args = as.list(posTransform)
					if(length(args) == 2) {
						region_new = posTransform(region)
					} else if(length(args) == 3) {
						region_new = posTransform(region, value)
					}
				}
			} else {
				region_new  = region
			}
			
			# for(i in seq_len(nrow(region))) {
			# 	if(horizontalLine == "both" || horizontalLine == "top") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	if(horizontalLine == "both" || horizontalLine == "bottom") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	mid = (region[i, 1] + region[i, 2])/2
			# 	mid_new = (region_new[i, 1] + region_new[i, 2])/2
			# 	circos.lines(c(mid, mid, mid_new, mid_new), c(1, 2/3, 1/3, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# }
			nr = nrow(region)
			if(horizontalLine == "both" || horizontalLine == "top") {
				circos.segments(region[, 1], rep(1, nr), region[, 2], rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			if(horizontalLine == "both" || horizontalLine == "bottom") {
				circos.segments(region[, 1], rep(0, nr), region[, 2], rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			mid = (region[, 1] + region[, 2])/2
			mid_new = (region_new[, 1] + region_new[, 2])/2
			circos.segments(mid, rep(1, nr), mid, rep(2/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid, rep(2/3, nr), mid_new, rep(1/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid_new, rep(1/3, nr), mid_new, rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
		}, ...)
	} else {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height, panel.fun = function(region, value, ...) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			region_subset = data[l, , drop = FALSE]
			if(!is.null(posTransform)) {
				if(is.function(posTransform)) {
					args = as.list(posTransform)
					if(length(args) == 2) {
						region_new = posTransform(region)
					} else if(length(args) == 3) {
						region_new = posTransform(region, value)
					}
				}
			} else {
				region_new  = region
			}
			
			# for(i in seq_len(nrow(region))) {
			# 	if(horizontalLine == "both" || horizontalLine == "bottom") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	if(horizontalLine == "both" || horizontalLine == "top") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	mid = (region[i, 1] + region[i, 2])/2
			# 	mid_new = (region_new[i, 1] + region_new[i, 2])/2
			# 	circos.lines(c(mid, mid, mid_new, mid_new), c(0, 1/3, 2/3, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# }
			nr = nrow(region)
			if(horizontalLine == "both" || horizontalLine == "bottom") {
				circos.segments(region[, 1], rep(0, nr), region[, 2], rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			if(horizontalLine == "both" || horizontalLine == "top") {
				circos.segments(region[, 1], rep(1, nr), region[, 2], rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			mid = (region[, 1] + region[, 2])/2
			mid_new = (region_new[, 1] + region_new[, 2])/2
			circos.segments(mid, rep(0, nr), mid, rep(1/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid, rep(1/3, nr), mid_new, rep(2/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid_new, rep(2/3, nr), mid_new, rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
		}, ...)
	}
	
	circos.par(track.margin = o.track.margin)
}

# == title
# Calculate and add genomic density track
#
# == param
# -data A bed-file-like data frame or a list of data frames
# -ylim.force Whether to force upper bound of ``ylim`` to be 1.
# -window.size Pass to `genomicDensity`
# -overlap Pass to `genomicDensity`
# -col  Colors. It should be length of one. If ``data`` is a list of data frames, the length of ``col``
#       can also be the length of the list.
# -lwd  Width of lines
# -lty  Style of lines
# -type Type of lines, see `circos.lines`
# -area See `circos.lines`
# -area.baseline Deprecated, use ``baseline`` instead.
# -baseline See `circos.lines`
# -border See `circos.lines`
# -... Pass to `circos.trackPlotRegion`
#
# == details
# This function is a high-level graphical function, and it will create a new track.
circos.genomicDensity = function(data, ylim.force = FALSE, window.size = NULL, overlap = TRUE, 
	col = ifelse(area, "grey", "black"), lwd = par("lwd"), lty = par("lty"), type = "l",
	area = TRUE, area.baseline = NULL, baseline = 0, border = NA, ...) {
	
	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning("`area.baseline` is deprecated, please use `baseline` instead.\n")
	}
	
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
	if(length(baseline) == 1) {
		baseline = rep(baseline, length(data))
	}
	if(length(border) == 1) {
		border = rep(border, length(data))
	}

	s = sapply(get.all.sector.index(), function(si) get.cell.meta.data("xrange", sector.index = si))
	if(is.null(window.size)) {
		window.size = 10^nchar(sum(s))/1000  # around 100 major ticks
		#cat(window.size, "is choosen as the window size.\n")
	}
	
	df = vector("list", length = length(data))
	for(i in seq_along(data)) {
		all.chr = unique(data[[i]][[1]])
		for(chr in all.chr) {
			region = data[[i]][data[[i]][[1]] == chr, 2:3, drop = FALSE]
			dn = genomicDensity(region, window.size = window.size, overlap = overlap)
			dn = cbind(rep(chr, nrow(dn)), dn)
			df[[i]] = rbind(df[[i]], dn)
		}
	}
	if(ylim.force) {
		ymax = 1
	} else {
		ymax = max(sapply(df, function(gr) max(gr[[4]])))
	}
	circos.genomicTrackPlotRegion(df, ylim = c(0, ymax), panel.fun = function(region, value, ...) {
		i = getI(...)
		circos.genomicLines(region, value, col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
			border = border[i], area = area[i], baseline = baseline[i]) 
	}, ...)
}

# == title
# Calculate genomic region density
#
# == param
# -region Genomic positions. It can be a data frame with two
#     columns which are start positions and end positions on a single chromosome.
#     It can also be a bed-format data frame which contains the chromosome column.
# -window.size Window size to calculate genomic density
# -overlap Whether two neighbouring windows have half overlap
#
# == details
# It calculate the percent of each genomic windows that is covered by the input regions.
#
# == values
# If the input is a two-column data frame, the function returns a data frame with three columns: 
# start position, end position and percent of overlapping. And if the input is a bed-format
# data frame, there will be an additionally chromosome name column.
genomicDensity = function(region, window.size = 10000000, overlap = TRUE) {
	
	if(is.character(region[, 1]) || is.factor(region[, 1])) {
		region[, 1] = as.vector(region[, 1])
		return(do.call("rbind", lapply(unique(region[, 1]), function(chr) {
			l = region[,1] == chr
			df = genomicDensity(region[l, 2:3, drop = FALSE], window.size = window.size, overlap = overlap)
			cbind(chr = rep(chr, nrow(df)), df)
		})))
	}
	
	if(ncol(region) >= 3) {
		if(is.numeric(region[, 1])) {
			if(max(region[, 1]) < 100) {
				return(do.call("rbind", lapply(unique(region[, 1]), function(chr) {
					l = region[, 1] == chr
					df = genomicDensity(region[l, 2:3, drop = FALSE], window.size = window.size, overlap = overlap)
					cbind(chr = rep(chr, nrow(df)), df)
				})))
			}
		}
	}

	region = region[, 1:2]
	
	region = sort_region(region)
	region = reduce_region(region)

	# make a segmentation
	if(overlap) {
		b = seq(1, max(region[[2]]), by = window.size/2)
		s = b[-length(b)]
		s = s[-length(s)]
		e = s + window.size - 1
		
	} else {
		b = seq(1, max(region[[2]]), by = window.size)
		s = b[-length(b)]
		e = s + window.size - 1
	}

	s = as.integer(s)
	e = as.integer(e)

	y = rep(0, length(s))
	names(y) = paste(s, e, sep = ",")
	
	windows = data.frame(start = s, end = e)
	
	op = overlap_region(windows, region)
	
	res = data.frame(start = s, end = e, pct = op)
	return(res)
}

# == title
# Highlight chromosomes
#
# == param
# -... pass to `highlight.sector`
#
# == details
# This is only a shortcut function of `highlight.sector`.
#
highlight.chromosome = function(...) {
	
	highlight.sector(...)
}

continuousIndexSegment = function(x, n = NULL, loop = FALSE) {
	if(length(x) == 1) {
		return(list(x))
	} else {
		k = c(0, which(diff(x) > 1), length(x))
		lt = vector("list", length = length(k) - 1)
		for(i in seq_along(k)[-length(k)]) {
			lt[[i]] = x[(k[i] + 1):(k[i+1])]
		}
		
		if(loop && length(lt) > 1) {
			first = lt[[1]]
			last = lt[[length(lt)]]
			if(first[1] == 1 && last[length(last)] == n) {
				lt[[1]] = c(last, first)
				lt = lt[-length(lt)]
			}
		}
		
		return(lt)
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


normalizeToDataFrame = function(data, sort = FALSE) {

	all.chr = get.all.sector.index()

	if(is.data.frame(data)) {
		if(ncol(data) < 3) {
			stop("Your data frame is less than 3 column!.\n")
		}
		data = data[data[[1]] %in% all.chr, , drop = FALSE]
		if(sort) {
			data = data[order(data[[1]], data[[2]]), , drop = FALSE]
		}
		return(data)
	} else if(is.list(data) && all(sapply(data, is.data.frame))) {
		df = lapply(data, function(gr) {
			if(ncol(gr) < 3) {
				stop("Your data frame is less than 3 column!.\n")
			}
			gr = gr[gr[[1]] %in% all.chr, , drop = FALSE]
			if(sort) {
				gr = gr[order(gr[[1]], gr[[2]]), ]
			}
			return(gr)
		})
		return(df)
	} else {
		stop("The format of `data` should only be a data frame or a list of data frames.\n")
	}
}


.normalizeGraphicalParam = function(x, nc, nr, name) {
	if(nc == 1) {
		if(!(length(x) == 1 || length(x) == nr)) {
			stop(paste0("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your regions (", nr, ").\n"))
		} else if(length(x) == 1) {
			x = rep(x, nr)
		}
	} else {
		if(!(length(x) == 1 || length(x) == nc)) {
			stop(paste0("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your data column (", nc, ").\n"))
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
# -data A bed-file-like data frame or a list of data frames
# -ylim ylim for rainfall plot track. It's value should be log10(inter-distance+1)
# -col  Color of points. It should be length of one. If ``data`` is a list, the length of ``col``
#       can also be the length of the list.
# -pch  Style of points
# -cex  Size of points
# -... Pass to `circos.trackPlotRegion`
#
# == details
# This is high-level graphical function, which mean, it will create a new track.
#
# Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis
# corresponds to the distance to neighbour regions (log-based). So if there is a drop-down on
# the plot, it means there is a cluster of regions at that area.
#
# On the plot, y-axis are log10-transformed.
circos.genomicRainfall = function(data, ylim = c(0, 9), col = "black", pch = par("pch"),
	cex = par("cex"), ...) {
	
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
	
	circos.genomicTrackPlotRegion(data, ylim = ylim, panel.fun = function(region, value, ...) {
		df = rainfallTransform(region)
		i = getI(...)
		circos.genomicPoints(df[1:2], log10(df[3]+1), col = col[i], cex = cex[i], pch = pch[i])
	}, ...)
	
}

# == title
# Calculate inter-distance of genomic regions
#
# == param
# -region Genomic positions. It can be a data frame with two
#     columns which are start positions and end positions on a single chromosome.
#     It can also be a bed-format data frame which contains the chromosome column.
# -mode How to calculate inter-distance. For a region, there is a distance to the 
#       prevous region and also there is a distance to the next region. ``mode``
#       controls how to merge these two distances into one value.
#
# == values
# If the input is a two-column data frame, the function returnes a data frame with three columns: start position, end position and distance.
# And if the input is a bed-format data frame, there will be the chromosome column added.
rainfallTransform = function(region, mode = c("min", "max", "mean")) {
	
	mode = match.arg(mode)[1]

	if(is.character(region[, 1]) || is.factor(region[, 1])) {
		region[, 1] = as.vector(region[, 1])
		return(do.call("rbind", lapply(unique(region[, 1]), function(chr) {
			l = region[, 1] == chr
			df = rainfallTransform(region[l, 2:3, drop = FALSE], mode = mode)
			cbind(chr = rep(chr, nrow(df)), df)
		})))
	}
	if(ncol(region) >= 3) {
		if(is.numeric(region[, 1])) {
			if(max(region[, 1]) < 100) {
				return(do.call("rbind", lapply(unique(region[, 1]), function(chr) {
					l = region[, 1] == chr
					df = rainfallTransform(region[l, 2:3, drop = FALSE], mode = mode)
					cbind(chr = rep(chr, nrow(df)), df)
				})))
			}
		}
	}
	
	region = as.data.frame(sort_region(region[1:2]))
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

# == title
# Genomic position transformation function
#
# == param
# -region Genomic positions at a single chromosome. It is a data frame with two
#     columns which are start position and end position.
# -... other arguments
#
# == details
# The default position transformation functions transforms position to be equally distributed
# along the chromosome. If users want to define their own transformation function, the requirement
# is that the returned value should be a data frame with two columns: transformed start position
# and transformed end position. The returned value should have same number of rows as the input one.
#
# For details why need to use position transformation, please refer to `circos.genomicPosTransformLines`.
posTransform.default = function(region, ...) {
	xlim = get.cell.meta.data("xlim")
	segment = seq(xlim[1], xlim[2], length.out = nrow(region) + 1)
	return(data.frame(start = segment[-length(segment)], end = segment[-1]))
}

# == title
# Genomic position transformation function specifically for text
#
# == param
# -region Genomic positions at a single chromosome. It is a data frame with two
#     columns which are start position and end position.
# -y positions of texts
# -labels text labels
# -cex  text size
# -font  text font style
# -sector.index sector index
# -track.index track index
# -padding padding of text
# -... other arguments
#
# == details
# This position transformation function is designed specifically for text.
# Under the transformation, texts will be as close as possible to the original positions.
posTransform.text = function(region, y, labels, cex = 1, font = par("font"),
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), padding = 0, ...) {
	
	if(length(y) == 1) y = rep(y, nrow(region))
	if(length(labels) == 1) labels = rep(labels, nrow(region))
	
	od = order(region[[1]])
	region = region[od, ]
	y = y[od]
	labels = labels[od]

	text_height = strheight(labels, cex = cex, font = font)*(1+padding)
	
	d = circlize( (region[[1]] + region[[2]])/2, y, sector.index = sector.index, track.index = track.index)
	alpha1 = d[, "theta"] + as.degree(atan(text_height/2/d[, "rou"]))
	alpha2 = d[, "theta"] - as.degree(atan(text_height/2/d[, "rou"]))
	x1 = reverse.circlize(alpha1, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	x2 = reverse.circlize(alpha2, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	
	xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index)
	
	x1_new = x1
	x2_new = x2
	l = x2 - x1 >= xlim[2] - xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = xlim[2]
	l = x1 < xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = x2[l] + xlim[1] - x1[l]
	l = x2 > xlim[2]; x1_new[l] = x1[l] - (x2[l] - xlim[2]); x2_new[l] = xlim[2]
	return(smartAlign(x1_new, x2_new, xlim = xlim))
}

# == title
# Adjust positions of text
#
# == param
# -x1 position which corresponds to the top of the text
# -x2 position which corresponds to the bottom of the text 
# -xlim ranges on x-axis
#
# == details
# used internally
smartAlign = function(x1, x2, xlim) {
	
	ncluster.before = -1
	ncluster = length(x1)
	while(ncluster.before != ncluster) {
		ncluster.before = ncluster
		cluster = rep(0, length(x1))
		i_cluster = 1
		cluster[1] = i_cluster
		for(i in seq_along(x1)[-1]) {
			# overlap with previous one
			if(x1[i] <= x2[i-1]) {
				cluster[i] = i_cluster
			} else {
				i_cluster = i_cluster + 1
				cluster[i] = i_cluster
			}
		}
		ncluster = length(unique(cluster))
		
		if(ncluster.before == ncluster) break
		
		# tile intervals in each cluster and re-assign x1 and x2
		new_x1 = numeric(length(x1))
		new_x2 = numeric(length(x2))
		for(i_cluster in unique(cluster)) {
			index = which(cluster == i_cluster)
			total_len = sum(x2[index] - x1[index])
			mid = (min(x1[index]) + max(x2[index]))/2
			if(total_len > xlim[2] - xlim[1]) {
				tp = seq(xlim[1], xlim[2], length = length(index) + 1)
			} else if(mid - total_len/2 < xlim[1]) {
				tp = seq(xlim[1], xlim[1] + total_len, length = length(index) + 1)
			} else if(mid + total_len/2 > xlim[2]) {
				tp = seq(xlim[2] - total_len, xlim[2], length = length(index) + 1)
			} else {
				tp = seq(mid - total_len/2, mid + total_len/2, length = length(index)+1)
			}
			new_x1[index] = tp[-length(tp)]
			new_x2[index] = tp[-1]
		}
		
		x1 = new_x1
		x2 = new_x2
	}
	
	return(data.frame(start = x1, end = x2))
}

