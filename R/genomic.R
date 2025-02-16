
# == title
# Initialize the circular layout with an ideogram
#
# == param
# -cytoband  A path of the cytoband file or a data frame that already contains cytoband data. By default it is cytoband for hg19.
#            Pass to `read.cytoband`.
# -species Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically. If there is no cytoband for user's species,
#          it will keep on trying to download chromInfo file. Pass to `read.cytoband` or `read.chromInfo`.
# -chromosome.index subset of chromosomes, also used to reorder chromosomes.
# -sort.chr Whether chromosome names should be sorted (first sort by numbers then by letters).
#           If ``chromosome.index`` is set, this argumetn is enforced to ``FALSE``
# -draw.chr.prefix Whether draw the "chr" prefix for chromosomes.
# -major.by     Increment of major ticks. Pass to `circos.genomicInitialize`.
# -plotType     Which tracks should be drawn. ``ideogram`` for ideogram rectangle, ``axis`` for genomic axis and ``labels`` for chromosome names.
#               If there is no ideogram for specified species, ``ideogram`` will be enforced to be excluded.
#               If it is set to ``NULL``, the function just initialize the plot but draw nothing.
# -track.height Height of the track which contains "axis" and "labels".
# -ideogram.height Height of the ideogram track
# -...    Pass to `circos.genomicInitialize`.
#
# == details
# The function will initialize the circular plot in which each sector corresponds to a chromosome. You can control the order of 
# chromosomes by ``chromosome.index`` or by ``sort.chr``, or by setting a special format of ``cytoband`` (please refer to `read.cytoband` 
# to find out how to control a proper ``cytoband``).
#
# The function finally pass data to `circos.genomicInitialize` to initialize the circular plot.
#
# The style of ideogram is almost fixed, but you can customize it with your self-sefined code. Refer to vignette for demonstration.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html#initialize-cytoband
#
# == example
# \donttest{
# circos.initializeWithIdeogram()
#
# cytoband.file = system.file(package = "circlize",
#     "extdata", "cytoBand.txt")
# circos.initializeWithIdeogram(cytoband.file)
#
# cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband.df)
#
# circos.initializeWithIdeogram(species = "hg18")
#
# circos.initializeWithIdeogram(species = "mm10")
#
# circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))
#
# cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
#
# cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
# circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
#
# cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)
#
# circos.initializeWithIdeogram(plotType = c("axis", "labels"))
#
# circos.initializeWithIdeogram(plotType = NULL)
#
# circos.par("start.degree" = 90)
# circos.initializeWithIdeogram()
# circos.clear()
#
# circos.par("gap.degree" = rep(c(2, 4), 12))
# circos.initializeWithIdeogram()
# circos.clear()
# }
circos.initializeWithIdeogram = function(
	cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"), 
	species = NULL, 
	sort.chr = TRUE,
	draw.chr.prefix = FALSE,
	chromosome.index = usable_chromosomes(species), 
	major.by = NULL,
	plotType = c("ideogram", "axis", "labels"), 
	track.height = NULL, 
	ideogram.height = convert_height(2, "mm"), 
	...) {
	
	if("sector.names" %in% names(list(...))) {
		stop_wrap("Found argumnet `sector.names` is used. Please use the argument `chromosome.index` instead.")
	}

	# proper order will be returned depending on cytoband and sort.chr
	e = try(cytoband <- read.cytoband(cytoband, species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
	if(inherits(e, "try-error") && !is.null(species)) {  # if species is defined
		e2 = try(cytoband <- read.chromInfo(species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
		if(inherits(e2, "try-error")) {
			message(e)
			message(e2)
			stop_wrap("Cannot download either cytoband or chromInfo file from UCSC.")
		} else {
			message_wrap("Downloading cytoBand file from UCSC failed. Use chromInfo file instead. Note ideogram track will be removed from the plot.")
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
	} else if(inherits(e, "try-error")) {
		stop(e)
	}
	df = cytoband$df
	chromosome = cytoband$chromosome
	if(is.null(chromosome)) {
		if(is.factor(cytoband[, 1])) {
			chromosome = levels(cytoband$df[, 1])
		} else {
			chromosome = unique(cytoband$df[, 1])
		}
	}

	if(is.null(chromosome.index)) {
		chromosome.index = chromosome
	}

	# here df[[1]] is quite important, should be re-factered
	df[[1]] = factor(as.vector(df[[1]]), levels = chromosome.index)
	
	# sn for sector names, but not for sector index
	sn = unique(as.vector(df[[1]]))

	if(!draw.chr.prefix) {
		# we do not need 'chr' prefix if it exits, it holds too much space.
		sn = gsub("chr", "", sn)
	}
	
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(o.cell.padding[1], 0, o.cell.padding[3], 0))
	
	circos.genomicInitialize(df, sector.names = sn, major.by = major.by, plotType = plotType, track.height = track.height, ...)

	if(any(plotType %in% "ideogram")) {
		circos.genomicIdeogram(df, track.height = ideogram.height)
	}
}

# == title
# Add an ideogram track
#
# == param
# -cytoband A data frame or a file path, pass to `read.cytoband`.
# -species Abbreviations of the genome, pass to `read.cytoband`.
# -track.height Height of the ideogram track.
# -track.margin Margins for the track.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#ideograms
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# \donttest{
# circos.initializeWithIdeogram(plotType = c("labels", "axis"))
# circos.track(ylim = c(0, 1))
# circos.genomicIdeogram() # put ideogram as the third track
# }
circos.genomicIdeogram = function(
	cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"), 
	species = NULL, 
	track.height = mm_h(2),
	track.margin = circos.par("track.margin")) {

	chromosome.index = get.all.sector.index()
	e = try(cytoband <- read.cytoband(cytoband, species = species, chromosome.index = chromosome.index), silent = TRUE)
	if(inherits(e, "try-error")) {
		stop(e)
	}
	df = cytoband$df
	if(all(cytoband.col(df[, 5]) == "#FFFFFF")) {
		warning_wrap("Cannot map colors to cytobands. The ideogram won't be drawn.")
		return(invisible(NULL))
	}
	circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, track.height = track.height,
		panel.fun = function(region, value, ...) {
			col = cytoband.col(value[[2]])
			circos.genomicRect(region, value, ybottom = 0, ytop = 1, col = col, border = NA, ...)
			xlim = get.cell.meta.data("xlim")
			circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
		}, cell.padding = c(0, 0, 0, 0), track.margin = track.margin
	)
}

# == title
# Initialize circular plot with any genomic data
#
# == param
# -data         A data frame in bed format.
# -sector.names Labels for each sectors which will be drawn along each sector. It will not modify values of sector index.
# -major.by     Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).
# -plotType     If it is not ``NULL``, there will create a new track containing axis and names for sectors.
#               This argument controls which part should be drawn, ``axis`` for genomic axis and ``labels`` for chromosome names
# -tickLabelsStartFromZero Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.
# -axis.labels.cex The font size for the axis tick labels.
# -labels.cex   The font size for the labels.
# -track.height If ``PlotType`` is not ``NULL``, height of the annotation track.
# -...          Pass to `circos.initialize`
#
# == details
# The function will initialize circular plot from genomic data. If ``plotType`` is set with value in ``axis`` or ``labels``, there will
# create a new track.
#
# The order of sectors related to data structure of ``data``. If the first column in ``data`` is a factor, the order of sectors
# is ``levels(data[[1]])``; If the first column is just a simple vector, the order of sectors is ``unique(data[[1]]``.
#
# For more details on initializing genomic plot, please refer to the vignettes.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html#initialize-with-general-genomic-category
#
# == example
# df = read.cytoband()$df
# circos.genomicInitialize(df)
#
# df = data.frame(name = c("TP53", "TP63", "TP73"),
#                 start = c(7565097, 189349205, 3569084),
#                 end = c(7590856, 189615068, 3652765),
#                 stringsAsFactors = FALSE)
# circos.genomicInitialize(df)
# circos.clear()
#
# circos.genomicInitialize(df, tickLabelsStartFromZero = FALSE)
# circos.clear()
#
# circos.genomicInitialize(df, major.by = 5000)
# circos.clear()
#
# circos.genomicInitialize(df, plotType = "labels")
# circos.clear()
#
# circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))
# circos.clear()
#
# circos.genomicInitialize(df, sector.names = c("tp53x", "tp63x", "tp73"))
# circos.clear()
#
# df[[1]] = factor(df[[1]], levels = c("TP73", "TP63", "TP53"))
# circos.genomicInitialize(df)
# circos.clear()
#
circos.genomicInitialize = function(
	data, 
	sector.names = NULL, 
	major.by = NULL,
	plotType = c("axis", "labels"), 
	tickLabelsStartFromZero = TRUE,
	axis.labels.cex = 0.4*par("cex"), 
	labels.cex = 0.8*par("cex"), 
	track.height = NULL, 
	...) {

	data = validate_data_frame(data)
	validate_region(data, check_chr = FALSE)
	
	if(is.factor(data[[1]])) {
		fa = levels(data[[1]])
	} else {
		fa = unique(data[[1]])
	}
	
	if(!is.null(sector.names)) {
		if(length(sector.names) != length(fa)) {
			stop_wrap("length of `sector.names` and length of sectors differ.")
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

	if(circos.par$ring) {
		op = c(op[1], 0, op[3], 0)
    	ow = FALSE
	}

	if(is.null(track.height)) {
		if(all(c("axis", "labels") %in% plotType)) {
			track.height = convert_unit_in_canvas_coordinate(1.5, "mm") + strheight("0", cex = axis.labels.cex) + 
				convert_unit_in_canvas_coordinate(0.5, "mm") + strheight("chr", cex = labels.cex)
		} else if("labels" %in% plotType) {
			track.height = strheight("chr", cex = labels.cex)
		} else if("axis" %in% plotType) {
			track.height = convert_unit_in_canvas_coordinate(1.5, "mm") + strheight("0", cex = axis.labels.cex)
		} else {
			track.height = convert_height(3, "mm")
		}
	}

	# axis and chromosome names
	if(any(plotType %in% c("axis", "labels"))) {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height,
			panel.fun = function(region, value, ...) {
				sector.index = get.cell.meta.data("sector.index")
				xlim = get.cell.meta.data("xlim")
				
				if(all(c("axis", "labels") %in% plotType)) {
					circos.genomicAxis(h = "bottom", major.by = major.by, tickLabelsStartFromZero = tickLabelsStartFromZero, labels.cex = axis.labels.cex)
					circos.text(mean(xlim), convert_y(1.5, "mm") + convert_y(strheight("chr", cex = axis.labels.cex), "canvas") + convert_y(0.5, "mm"), 
						labels = sector.names[sector.index], cex = labels.cex, adj = c(0.5, 0), niceFacing = TRUE)
				} else if("labels" %in% plotType) {
					circos.text(mean(xlim), 0, labels = sector.names[sector.index], cex = labels.cex, adj = c(0.5, 0), niceFacing = TRUE)
				} else if("axis" %in% plotType) {
					circos.genomicAxis(h = "bottom", major.by = major.by, tickLabelsStartFromZero = tickLabelsStartFromZero,labels.cex = axis.labels.cex)
				}
 			}
		)
	}
	
	circos.par("cell.padding" = op, "points.overflow.warning" = ow)
	return(invisible(NULL))
}


# == title
# Initialize a layout for circular genome
#
# == param
# -name Name of the genome (or the "chromosome name").
# -genome_size Size of the genome
# -plotType Pass to `circos.genomicInitialize`.
# -... All goes to `circos.genomicInitialize`.
#
circos.initializeCircularGenome = function(name, genome_size, plotType = "axis", ...) {
	circos.genomicInitialize(data.frame(name, 0, genome_size), ..., ring = TRUE, plotType = plotType)
} 

# == title
# Add genomic axes
#
# == param
# -h Position of the axes. "top" or "bottom".
# -major.at Major breaks. If ``major.at`` is set, ``major.by`` is ignored.
# -labels labels corresponding to ``major.at``. If ``labels`` is set, ``major.at`` must be set.
# -major.by Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).
# -tickLabelsStartFromZero Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.
# -labels.cex The font size for the axis tick labels.
# -sector.index Index for the sector
# -track.index  Index for the track
# -... Other arguments pass to `circos.axis`.
#
# == details
# It assigns proper tick labels under genomic coordinate.
# 
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-axes
#
# == example
# circos.initializeWithIdeogram(chromosome.index = paste0("chr", 1:4), plotType = NULL)
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) circos.genomicAxis())
# circos.track(ylim = c(0, 1), track.height = 0.1)
# circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
#     circos.genomicAxis(h = "bottom", direction = "inside")
# })
# circos.clear()
#
circos.genomicAxis = function(
	h = "top", 
	major.at = NULL, 
	labels = NULL,
	major.by = NULL, 
	tickLabelsStartFromZero = TRUE,
	labels.cex = 0.4*par("cex"), 
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(), 
	...) {

	if(!h %in% c("top", "bottom")) {
		stop_wrap("`h` can only be 'top' or 'bottom'.")
	}

	xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index)

	if(!is.null(major.at) && !is.null(labels)) {
		if(length(major.at) != length(labels)) {
			stop_wrap("Length of major.at and labels should be the same.")
		}
	}
	if(is.null(major.at) && !is.null(labels)) {
		stop_wrap("If labels is set, major.at should also be set.")
	}

	if(tickLabelsStartFromZero) {
		offset = xlim[1]
		if(is.null(major.by)) {
			major.by = .default.major.by()
		}

		if(is.null(major.at)) {
			major.at = seq(xlim[1], xlim[2], by = major.by)
			major.at = c(major.at, major.at[length(major.at)] + major.by)
		}
		
		if(is.null(labels)) {
			if(major.by >= 1e6) {
				major.tick.labels = paste((major.at-offset)/1000000, "Mb", sep = "")
			} else if(major.by >= 1e3) {
				major.tick.labels = paste((major.at-offset)/1000, "kb", sep = "")
			} else {
				major.tick.labels = paste((major.at-offset), "bp", sep = "")
			}
		} else {
			major.tick.labels = labels
		}
		
	} else {
		if(is.null(major.by)) {
			major.by = .default.major.by()
		}
		if(is.null(major.at)) {
			major.at = seq(floor(xlim[1]/major.by)*major.by, xlim[2], by = major.by)
			major.at = c(major.at, major.at[length(major.at)] + major.by)
		}

		if(is.null(labels)) {
			if(major.by >= 1e6) {
				major.tick.labels = paste(major.at/1000000, "MB", sep = "")
			} else if(major.by >= 1e3) {
				major.tick.labels = paste(major.at/1000, "KB", sep = "")
			} else {
				major.tick.labels = paste(major.at, "bp", sep = "")
			}
		} else {
			major.tick.labels = labels
		}
	}
	circos.axis(h = h, major.at = major.at, labels = major.tick.labels, labels.cex = labels.cex,
		sector.index = sector.index, track.index = track.index, ...)				
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
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/genomic-plotting-region.html and https://jokergoo.github.io/circlize_book/book/modes-of-input.html
#
circos.genomicTrackPlotRegion = function(
	data = NULL, 
	ylim = NULL, 
	stack = FALSE,
	numeric.column = NULL, 
	jitter = 0,
	panel.fun = function(region, value, ...) {NULL}, 
	... ) {
	
	if(is.null(data)) {
		all.sector.index = get.all.sector.index()
		data = data.frame(all.sector.index,
		           rep(0, length(all.sector.index)),
				   rep(0, length(all.sector.index)))
	}

    if(is.function(data) || is.function(ylim) || is.function(stack) || is.function(numeric.column)) {
        stop_wrap("The panel function should be set explicitly with the argument name `panel.fun = ...`.")
    }
	
	# re-define panel.fun
	genomicPanelFun = panel.fun

	# now `data` is either a data frame or a list of data frame
	data = normalizeToDataFrame(data)

	if(is.dataFrameList(data)) {
		for(i in seq_along(data)) {
			data[[i]][[1]] = as.character(data[[i]][[1]])
			if(circos.par$ring) {
				l = data[[i]][, 2] > data[[i]][, 3]
				if(any(l)) {
					data[[i]][l, 2] = data[[i]][l, 2] - diff(get.sector.data()[c("min.value", "max.value")])
				}
			}
			validate_region(data[[i]], check_chr = FALSE)
		}
	} else {
		data[[1]] = as.character(data[[1]])
		if(circos.par$ring) {
			l = data[, 2] > data[, 3]
			if(any(l)) {
				data[l, 2] = data[l, 2] - diff(get.sector.data()[c("min.value", "max.value")])
			}
		}
		validate_region(data, check_chr = FALSE)
	}

	# excluding the first three columns
	if(!is.null(numeric.column)) {
		if(is.numeric(numeric.column)) {
			numeric.column = numeric.column - 3
		}
		if(any(numeric.column <= 0)) {
			stop_wrap("Wrong value in `numeric.column`, they should be larger than 3 or character index.")
		}
	}

	# auto calcualte numeric column
	if(is.dataFrameList(data)) {
		if(!is.null(numeric.column)) {
			if(length(numeric.column) == 1) {
				numeric.column = rep(list(numeric.column), length(data))
			} else if(length(numeric.column) == length(data)) {
				
			} else {
				stop_wrap("Length of `numeric.column` should only be one or length of ``data`` if it is a list of data frames.")
			}
			for(i in seq_along(data)) {
				if(!is.numeric(data[[i]][-(1:3)][, numeric.column[i]])) {
					stop_wrap("Some of your `numeric.column` are not numeric.")
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
			if(!all(sapply(data[-(1:3)][, numeric.column, drop = FALSE], is.numeric))) {
				stop_wrap("Some of your `numeric.column` are not numeric.")
			}
		}
	}
	
	args = formals(genomicPanelFun)
	if(!(length(args) == 3 && names(args)[3] == "...")) {
		stop_wrap("The `panel.fun` need a third argument `...` to pass special parameters to graphical functions.")
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
						stop_wrap("There is no numeric column in one of your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.")
					}
					range(unlist(lapply(gr[-(1:3)][ numeric.column[i] ], range, na.rm = TRUE)))
				})))
			} else {
				if(length(numeric.column) == 0) {
					stop_wrap("There is no numeric column in your data frame which calculation of `ylim` depends on. Or you can set `ylim` explicitely.")
				}
				ylim = range(unlist(lapply(data[-(1:3)][numeric.column], range, na.rm = TRUE)))
			}

			if(ylim[1] == ylim[2]) {
				stop_wrap("It seems the data points are all the same. Please explicitly set values to `ylim`.")
			}
		}

		if(is.dataFrameList(data)) {
			circos.trackPlotRegion(ylim = ylim, panel.fun = function(x, y) {
					
					chr = get.current.chromosome()
					for(i in seq_along(data)) {
						l = data[[i]][, 1] == chr
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
# -... Pass to `circos.genomicTrackPlotRegion`.
#
# == details
# shortcut function of `circos.genomicTrackPlotRegion`.
#
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
		stop_wrap("Maybe you should call like `getI(...)`")
	}
	.param = args$.param
	return(.param$i)
}


# == title
# Add points to a plotting region, specifically for genomic graphics
#
# ==param
# -region A data frame contains 2 columns which correspond to start positions and end positions.
# -value  A data frame contains values and other information.
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, the whole numeric columns in ``value`` will be taken.
# -sector.index Index of sector.
# -track.index Index of track.
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explanation
# -col Color of points. If there is only one numeric column, the length of ``col`` can be either one or number of rows of ``region``.
#      If there are more than one numeric column, the length of ``col`` can be either one or number of numeric columns.
#      Pass to `circos.points`.
# -pch Type of points. Settings are similar as ``col``. Pass to `circos.points`.
# -cex Size of points. Settings are similar as ``col``. Pass to `circos.points`.
# -bg Background colors for points.
# -... Mysterious parameters.
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrack`.
#
# The function behaviours differently from different formats of input, see the examples in 
# the "Examples" Section or go to https://jokergoo.github.io/circlize_book/book/modes-of-input.html for more details.
#
# == example
# circos.par("track.height" = 0.1)
# circos.initializeWithIdeogram(plotType = NULL)
#
# bed = generateRandomBed(nr = 100)
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
# })
#
# circos.genomicTrack(bed, stack = TRUE, panel.fun = function(region, value, ...) {
#     circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
#     i = getI(...)
#     cell.xlim = get.cell.meta.data("cell.xlim")
#     circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
# })
#
# bed1 = generateRandomBed(nr = 100)
# bed2 = generateRandomBed(nr = 100)
# bed_list = list(bed1, bed2)
#
# # data frame list
# circos.genomicTrack(bed_list, panel.fun = function(region, value, ...) {
#     cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
#     i = getI(...)
#     circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
# })
#
# circos.genomicTrack(bed_list, stack = TRUE,
#     panel.fun = function(region, value, ...) {
#     cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
#     i = getI(...)
#     circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
#     cell.xlim = get.cell.meta.data("cell.xlim")
#     circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
# })
#
# bed = generateRandomBed(nr = 100, nc = 4)
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
#     circos.genomicPoints(region, value, cex = 0.5, pch = 16, col = 1:4, ...)
# })
#
# circos.genomicTrack(bed, stack = TRUE, panel.fun = function(region, value, ...) {
#     cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
#     i = getI(...)
#     circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
#     cell.xlim = get.cell.meta.data("cell.xlim")
#     circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
# })
#
# circos.clear()
#
circos.genomicPoints = function(
	region, 
	value, 
	numeric.column = NULL, 
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), 
    posTransform = NULL, 
	pch = par("pch"), 
	col = par("col"), 
	cex = par("cex"), 
	bg = par("bg"), 
	...) {
	
	nr = nrow(region)
	if(ncol(region) > 2 && inherits(region[, 1], c("character", "factor"))) {
		region = region[, -1, drop = FALSE]
	}
	
	if(is.atomic(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.atomic(value) && length(value) == nr) {
		value = data.frame(value = value)
	}
	if(!is.data.frame(value)) stop_wrap("`value` should be a data frame.")
	
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

	if(ncol(value) == 1) numeric.column = 1
	
	if(!is.null(posTransform)) {
		region = posTransform(region)
	}
	
	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(value, is.numeric)))
		if(length(numeric.column) == 0) {
			stop_wrap("Cannot find numeric column.")
		}
	}
	
	nc = length(numeric.column)
	pch = .normalizeGraphicalParam(pch, nc, nr, "pch")
	col = .normalizeGraphicalParam(col, nc, nr, "col")
	cex = .normalizeGraphicalParam(cex, nc, nr, "cex")
	bg = .normalizeGraphicalParam(bg, nc, nr, "cex")
	
	if(nc == 1) {
		circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.column ]], 
			pch = pch, col = col, cex = cex, bg = bg,
			sector.index = sector.index, track.index = track.index )
	} else {
		for(i in seq_len(nc)) {
			circos.points( (region[[1]] + region[[2]])/2, value[[ numeric.column[i] ]], 
				pch = pch[i], col = col[i], cex = cex[i], bg = bg[i],
				sector.index = sector.index, track.index = track.index ) 
		}
	}
}

# == title
# Add lines to a plotting region, specifically for genomic graphics
#
# == param
# -region A data frame contains 2 column which correspond to start positions and end positions.
# -value  A data frame contains values and other information.
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, the whole numeric columns in ``value`` will be taken.
# -sector.index Index of sector.
# -track.index Index of track.
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explaination.
# -col col of lines/areas. If there are more than one numeric column, the length of ``col`` can be either one or number of numeric columns.
#      If there is only one numeric column and type is either ``segment`` or ``h``, 
#      the length of ``col`` can be either one or number of rows of ``region``.
#      pass to `circos.lines`
# -lwd Settings are similar as ``col``. Pass to `circos.lines`.
# -lty Settings are similar as ``col``. Pass to `circos.lines`.
# -type There is an additional option ``segment`` which plot segment lines from start position to end position. Settings are similar as ``col``. Pass to `circos.lines`. 
# -area Settings are similar as ``col``. Pass to `circos.lines`.
# -area.baseline Deprecated, use ``baseline`` instead.
# -baseline Settings are similar as ``col``. Pass to `circos.lines`.
# -border Settings are similar as ``col``. Pass to `circos.lines`.
# -pt.col Settings are similar as ``col``. Pass to `circos.lines`.
# -cex Settings are similar as ``col``. Pass to `circos.lines`.
# -pch Settings are similar as ``col``. Pass to `circos.lines`.
# -... Mysterious parameters.
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrack`.
#
# The function behaviours differently from different formats of input, see the examples in 
# the "Examples" Section or go to https://jokergoo.github.io/circlize_book/book/modes-of-input.html for more details.
#
# == examples
# \donttest{
# ### test bed
# circos.par("track.height" = 0.1)
# circos.initializeWithIdeogram(plotType = NULL)
#
# bed = generateRandomBed(nr = 100)
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     circos.genomicLines(region, value, type = "l", ...)
# })
#
# bed1 = generateRandomBed(nr = 100)
# bed2 = generateRandomBed(nr = 100)
# bed_list = list(bed1, bed2)
#
# circos.genomicTrack(bed_list, panel.fun = function(region, value, ...) {
#     i = getI(...)
#     circos.genomicLines(region, value, col = i, ...)
# })
#
# circos.genomicTrack(bed_list, stack = TRUE, 
#     panel.fun = function(region, value, ...) {
#     i = getI(...)
#     circos.genomicLines(region, value, col = i, ...)
# })
#
# bed = generateRandomBed(nr = 100, nc = 4)
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     circos.genomicLines(region, value, col = 1:4, ...)
# })
#
# circos.genomicTrack(bed, stack = TRUE, panel.fun = function(region, value, ...) {
#     i = getI(...)
#     circos.genomicLines(region, value, col = i, ...)
# })
#
# bed = generateRandomBed(nr = 100)
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
# })
#
# circos.clear()
# }
circos.genomicLines = function(
	region, 
	value, 
	numeric.column = NULL, 
	sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), 
    posTransform = NULL, 
	col = ifelse(area, "grey", "black"), 
	lwd = par("lwd"),
    lty = par("lty"), 
    type = "l",
    area = FALSE, 
    area.baseline = NULL, 
    border = "black", 
    baseline = "bottom",
    pt.col = par("col"), 
    cex = par("cex"), 
    pch = par("pch"), 
    ...) {
	
	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning_wrap("`area.baseline` is deprecated, please use `baseline` instead.")
	}
	nr = nrow(region)
	if(ncol(region) > 2 && inherits(region[, 1], c("character", "factor"))) {
		region = region[, -1, drop = FALSE]
	}

	if(is.atomic(value) && length(value) == 1) {
		value = data.frame(value = rep(value, nr))
	}
	if(is.atomic(value) && length(value) == nr) {
		value = data.frame(value = value)
	}
	if(!is.data.frame(value)) stop_wrap("`value` should be a data frame.")
	
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
	if(ncol(value) == 1) numeric.column = 1

	if(!is.null(posTransform)) {
		region = posTransform(region)
	}

	if(is.null(numeric.column)) {
		numeric.column = which(as.logical(sapply(value, is.numeric)))
		if(length(numeric.column) == 0) {
			stop_wrap("Cannot find numeric column.")
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
# -region A data frame contains 2 column which correspond to start positions and end positions.
# -value  A data frame contains values and other information.
# -ytop A vector or a single value indicating top position of rectangles.
# -ybottom A vector or a single value indicating bottom position of rectangles.
# -ytop.column If ``ytop`` is in ``value``, the index of the column.
# -ybottom.column If ``ybottom`` is in ``value``, the index of the column.
# -sector.index Index of sector.
# -track.index Index of track.
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explaination.
# -col The length of ``col`` can be either one or number of rows of ``region``. Pass to `circos.rect`.
# -border Settings are similar as ``col``. Pass to `circos.rect`.
# -lty Settings are similar as ``col``. Pass to `circos.rect`.
# -... Mysterious parameters.
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrack`.
#
# The function behaviours differently from different formats of input, see the examples in 
# the "Examples" Section or go to https://jokergoo.github.io/circlize_book/book/modes-of-input.html for more details.
#
# == example
# \donttest{
# circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
# circos.initializeWithIdeogram(plotType = NULL)
#
# bed1 = generateRandomBed(nr = 100)
# bed2 = generateRandomBed(nr = 100)
# bed_list = list(bed1, bed2)
# f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
# circos.genomicTrack(bed_list, stack = TRUE,
#     panel.fun = function(region, value, ...) {
#  
#     circos.genomicRect(region, value, col = f(value[[1]]), 
#         border = NA, ...)
#     i = getI(...)
#     cell.xlim = get.cell.meta.data("cell.xlim")
#     circos.lines(cell.xlim, c(i, i), lty = 2, col = "#000000")
# })
#
# circos.genomicTrack(bed_list, ylim = c(0, 3),
#     panel.fun = function(region, value, ...) {
#     i = getI(...)
#     circos.genomicRect(region, value, ytop = i+0.4, ybottom = i-0.4, col = f(value[[1]]), 
#         border = NA, ...)
#   
#     cell.xlim = get.cell.meta.data("cell.xlim")
#     circos.lines(cell.xlim, c(i, i), lty = 2, col = "#000000")
# })
#
# circos.genomicTrack(bed1, panel.fun = function(region, value, ...) {
#     circos.genomicRect(region, value, col = "red", border = NA, ...)
#
# })
#
# circos.genomicTrack(bed_list, panel.fun = function(region, value, ...) {
#     i = getI(...)
#     circos.genomicRect(region, value, col = i, border = NA, ...)
#
# })
#
# circos.clear()
# }
circos.genomicRect = function(
	region, 
	value = NULL, 
	ytop = NULL, 
	ybottom = NULL, 
	ytop.column = NULL, 
	ybottom.column = NULL,
	sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), 
    posTransform = NULL, 
	col = NA, 
	border = "black", 
	lty = par("lty"), 
	...) {
	
	if(ncol(region) > 2 && inherits(region[, 1], c("character", "factor"))) {
		region = region[, -1, drop = FALSE]
	}
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
	if(!is.data.frame(value)) stop_wrap("`value` should be a data frame.")
	
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
		stop_wrap("Only one ytop columns is allowed.")
	}
	if(length(ybottom.column) > 1) {
		stop_wrap("Only one ybottom columns is allowed.")
	}

	col = .normalizeGraphicalParam(col, 1, nr, "col")
	border = .normalizeGraphicalParam(border, 1, nr, "border")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	
	# for(i in seq_len(nr)) {
	# 	circos.rect(region[i, 1], value[i, ybottom.column], region[i, 2], value[i, ytop.column],
	# 	            sector.index = sector.index, track.index = track.index,
	# 				col = col[i], border = border[i], lwd = lwd[i], lty = lty[i])
	# }
	circos.rect(region[, 1], value[, ybottom.column], region[, 2], value[, ytop.column],
		            sector.index = sector.index, track.index = track.index,
					col = col, border = border, lty = lty)

}

# == title
# Draw text in a cell, specifically for genomic graphics
#
# == param
# -region A data frame contains 2 column which correspond to start positions and end positions.
# -value  A data frame contains values and other information.
# -y A vector or a single value indicating position of text.
# -labels Labels of text corresponding to each genomic positions.
# -labels.column If labels are in ``value``, index of column in ``value``.
# -numeric.column Which column in ``value`` data frame should be taken as y-value.
#                 If it is not defined, only the first numeric columns in ``value`` will be taken.
# -sector.index Index of sector.
# -track.index Index of track.
# -posTransform Self-defined function to transform genomic positions, see `posTransform.default` for explanation.
# -facing Passing to `circos.text`. Settings are similar as ``col``.
# -niceFacing   Should the facing of text be adjusted to fit human eyes?
# -direction Deprecated, use ``facing`` instead. 
# -adj Pass to `circos.text`. Settings are similar as ``col``.
# -cex Pass to `circos.text`. Settings are similar as ``col``.
# -col Pass to `circos.text`. The length of ``col`` can be either one or number of rows of ``region``.
# -font Pass to `circos.text`. Settings are similar as ``col``.
# -padding pass to ``posTransform`` if it is set as `posTransform.text`.
# -extend pass to ``posTransform`` if it is set as `posTransform.text`.
# -align_to pass to ``posTransform`` if it is set as `posTransform.text`.
# -... Mysterious parameters.
#
# == details
# The function is a low-level graphical function and usually is put in ``panel.fun`` when using `circos.genomicTrack`.
#
# == example
# circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
# circos.initializeWithIdeogram(plotType = NULL)
#
# bed = generateRandomBed(nr = 20)
#
# circos.genomicTrack(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
#     circos.genomicText(region, value, y = 0.5, labels = "text", ...)
# })
#
# bed = cbind(bed, sample(letters, nrow(bed), replace = TRUE))
# circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
#     circos.genomicText(region, value, labels.column = 2, ...)
# })
#
# circos.clear()
circos.genomicText = function(
	region, 
	value = NULL, 
	y = NULL, 
	labels = NULL, 
	labels.column = NULL,
	numeric.column = NULL, 
	sector.index = get.current.sector.index(), 
	track.index = get.current.track.index(), 
	posTransform = NULL, 
	direction = NULL, 
	facing = "inside", 
	niceFacing = FALSE,
	adj = par("adj"), 
	cex = 1, 
	col = "black", 
	font = par("font"), 
	padding = 0,
	extend = 0, 
	align_to = "region", 
	...) {
	
	if(!is.null(direction)) {
		facing = direction
		warning_wrap("`direction` is deprecated, please use `facing` instead.")
	}
	
	if(ncol(region) > 2 && inherits(region[, 1], c("character", "factor"))) {
		region = region[, -1, drop = FALSE]
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
	if(!is.data.frame(value)) stop_wrap("`value` should be a data frame.")

	if(is.null(labels) && is.null(labels.column)) {
		stop_wrap("You should either specify `labels` or `labels.column`.")
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
			stop_wrap("Cannot find numeric column.")
		}
		numeric.column = numeric.column[1]
	}
	
	if(length(numeric.column) > 1) {
		stop_wrap("You can only have one numeric column.")
	}
	
	if(!is.null(posTransform)) {
	
		# check settings when it is text-specific transformation
		if(identical(posTransform, posTransform.text)) {
			if(! facing %in% c("clockwise", "reverse.clockwise")) {
				stop_wrap("Only support `facing` in c('clockwise', 'reverse.clockwise') if `posTransform` is `posTransform.text`.")
			}
			region = posTransform(region, value[[ numeric.column ]], value[[labels.column]], cex, font, padding = padding, extend = extend, align_to = align_to)
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
# -region1 A data frame in bed format.
# -region2 A data frame in bed format.
# -rou Pass to `circos.link`.
# -rou1 Pass to `circos.link`.
# -rou2 Pass to `circos.link`.
# -col Pass to `circos.link`, length can be either one or nrow of ``region1``.
# -lwd Pass to `circos.link`, length can be either one or nrow of ``region1``.
# -lty Pass to `circos.link`, length can be either one or nrow of ``region1``.
# -border Pass to `circos.link`, length can be either one or nrow of ``region1``.
# -inverse Pass to `circos.link`.
# -... Pass to `circos.link`.
#
# == details
# Of course, number of rows should be same in ``region1`` and ``region2``.
#
# If you want to have more controls on links, please use `circos.link` directly.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/genomic-plotting-region.html#genomic-links
#
# == example
# \donttest{
# set.seed(123)
#
# bed1 = generateRandomBed(nr = 100)
# bed1 = bed1[sample(nrow(bed1), 20), ]
# bed2 = generateRandomBed(nr = 100)
# bed2 = bed2[sample(nrow(bed2), 20), ]
# circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
# circos.initializeWithIdeogram()
#
# circos.genomicLink(bed1, bed2, col = sample(1:5, 20, replace = TRUE), border = NA)
# circos.clear()
# }
circos.genomicLink = function(
	region1, 
	region2, 
	rou = get_most_inside_radius(), 
	rou1 = rou, 
	rou2 = rou,
    col = "black", 
    lwd = par("lwd"), 
    lty = par("lty"), 
    border = col, 
    inverse = NULL,
    ...) {

	if(circos.par$ring) {
		l = region1[, 2] > region1[, 3]
		if(any(l)) {
			region1[l, 2] = region1[l, 2] - diff(get.sector.data()[c("min.value", "max.value")])
		}
		l = region2[, 2] > region2[, 3]
		if(any(l)) {
			region2[l, 2] = region2[l, 2] - diff(get.sector.data()[c("min.value", "max.value")])
		}
	}
	
	region1 = validate_data_frame(region1)
	region2 = validate_data_frame(region2)

	if(ncol(region1) == 2) {
		region1[, 3] = region1[, 2]
	}
	if(ncol(region2) == 2) {
		region2[, 3] = region2[, 2]
	}

	region1 = normalizeToDataFrame(region1, sort = FALSE)
	region2 = normalizeToDataFrame(region2, sort = FALSE)
	
	if(is.dataFrameList(region1)) {
		stop_wrap("`region1` can not be a region list.")
	}
	
	if(is.dataFrameList(region1)) {
		stop_wrap("`region1` can not be a region list.")
	}
	
	if(nrow(region1) != nrow(region2)) {
		stop_wrap("nrow of `region1` and `region2` differ. Please check the chromosome column and make sure all the chromosomes are in the circular layout.")
	}
	
	nr = nrow(region1)
	
	rou1 = .normalizeGraphicalParam(rou1, 1, nr, "rou")
	rou2 = .normalizeGraphicalParam(rou2, 1, nr, "rou")
	col = .normalizeGraphicalParam(col, 1, nr, "col")
	lwd = .normalizeGraphicalParam(lwd, 1, nr, "lwd")
	lty = .normalizeGraphicalParam(lty, 1, nr, "lty")
	border = .normalizeGraphicalParam(border, 1, nr, "border")
	if(!is.null(inverse)) {
		if(!is.logical(inverse) || any(is.na(inverse))) {
			stop_wrap("`inverse` vector needs ot be boolean. Please check `inverse` vector type.")
		}
		if(length(inverse) != nr) {
			stop_wrap("length of `inverse` vector and `region1` differ. Please check the length of the `inverse` vector.")
		}
	}

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
		if(!is.null(inverse)) {
			circos.link(region1[i, 1], point1,
		            region2[i, 1], point2,
					rou1 = rou1[i], rou2 = rou2[i], col = col[i], lwd = lwd[i],
					lty = lty[i], border = border[i], inverse = inverse[i], ...)
		} else {
			circos.link(region1[i, 1], point1,
		            region2[i, 1], point2,
					rou1 = rou1[i], rou2 = rou2[i], col = col[i], lwd = lwd[i],
					lty = lty[i], border = border[i], ...)
		}
	}
}


# == title
# Add genomic position transformation lines between tracks
#
# == param
# -data A data frame containing genomic data.
# -track.height Height of the track.
# -posTransform Genomic position transformation function, see `posTransform.default` for an example.
# -horizontalLine Whether to draw horizontal lines which indicate region width .
# -track.margin Margin of tracks.
# -direction Type of the transformation. ``inside`` means position transformed track are located inside 
#       and ``outside`` means position transformed track are located outside.
# -col Color of lines, can be length of one or nrow of ``data``.
# -lwd Width of lines.
# -lty Style of lines.
# -... Pass to `circos.trackPlotRegion`.
#
# == details
# There is one representative situation when such position transformation needs to be applied. 
# For example, there are two sets of regions in a chromosome in which regions in one set regions are
# quite densely to each other and regions in other set are far from others. Heatmap or text is going
# to be drawn on the next track. If there is no position transformation, heatmap or text for those
# dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a way
# to transform original positions to new positions would help for the visualization. 
circos.genomicPosTransformLines = function(
	data, 
	track.height = 0.1, 
	posTransform = NULL, 
	horizontalLine = c("none", "top", "bottom", "both"), 
	track.margin = c(0, 0),
	direction = c("inside", "outside"), 
	col = "black", 
	lwd = par("lwd"),
    lty = par("lty"), 
    ...) {
	
	horizontalLine = match.arg(horizontalLine)[1]

	data = validate_data_frame(data)
	data = normalizeToDataFrame(data)
	
	if(is.dataFrameList(data)) {
		stop_wrap("`data` can not be list of regions.")
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
# -data A bed-file-like data frame or a list of data frames. If the input is a list of data frames.
#     there will be multiple density plot in one same track.
# -ylim.force Whether to force upper bound of ``ylim`` to be 1. Ignored if ``count_by`` is set to ``number``.
# -window.size Pass to `genomicDensity`.
# -overlap Pass to `genomicDensity`.
# -count_by Pass to `genomicDensity`.
# -col  Colors. It should be length of one. If ``data`` is a list of data frames, the length of ``col``
#       can also be the length of the list. If multiple sets of genomic regions are visualized in one
#       single track, you should set the colors with transparency to distinguish them.
# -lwd  Width of lines, the same setting as ``col`` argument.
# -lty  Style of lines, the same setting as ``col`` argument.
# -type Type of lines, see `circos.lines`.
# -area See `circos.lines`.
# -area.baseline Deprecated, use ``baseline`` instead.
# -baseline See `circos.lines`.
# -border See `circos.lines`.
# -... Pass to `circos.trackPlotRegion`.
#
# == details
# This function is a high-level graphical function, and it will create a new track.
#
# If you have multiple sets of genomic regions, you should make sure the density ranges 
# for all sets are similar, or I suggest you should put them into different tracks. One example
# can be found in the "Examples" Section where the density range for ``bed_list[[2]]`` is too high
# compared to the range for ``bed_list[[1]]``, thus, it is better to put the two sets of
# regions into two separate tracks.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-density-and-rainfall-plot
#
# == example
# load(system.file(package = "circlize", "extdata", "DMR.RData"))
#
# # rainfall
# \donttest{
# circos.initializeWithIdeogram(plotType = c("axis", "labels"))
#
# bed_list = list(DMR_hyper, DMR_hypo)
# circos.genomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))
#
# circos.genomicDensity(bed_list[[1]], col = c("#FF000080"), track.height = 0.1)
# circos.genomicDensity(bed_list[[2]], col = c("#0000FF80"), track.height = 0.1)
# circos.clear()
#
# ############ draw the two densities in one track  #############
# circos.initializeWithIdeogram(plotType = c("axis", "labels"))
# circos.genomicDensity(bed_list, col = c("#FF000080", "#0000FF80"), track.height = 0.2)
# circos.clear()
# }
circos.genomicDensity = function(
	data, 
	ylim.force = FALSE, 
	window.size = NULL, 
	overlap = TRUE, 
	count_by = c("percent", "number"),
	col = ifelse(area, "grey", "black"), 
	lwd = par("lwd"), 
	lty = par("lty"), 
	type = "l",
	area = TRUE, 
	area.baseline = NULL, 
	baseline = 0, 
	border = NA, 
	...) {
	
	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning_wrap("`area.baseline` is deprecated, please use `baseline` instead.")
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
	
	count_by = match.arg(count_by)[1]
	df = vector("list", length = length(data))
	for(i in seq_along(data)) {
		df[[i]] = genomicDensity(data[[i]], window.size = window.size, overlap = overlap, count_by = count_by)
	}
	if(ylim.force && count_by == "percent") {
		ymax = 1
	} else {
		ymax = max(sapply(df, function(gr) max(gr[[4]])))
	}
	if(length(df) == 1) {
		circos.genomicTrackPlotRegion(df[[1]], ylim = c(0, ymax), panel.fun = function(region, value, ...) {
			circos.genomicLines(region, value, col = col, lwd = lwd, lty = lty, type = type, 
				border = border, area = area, baseline = baseline) 
		}, ...)
	} else {
		circos.genomicTrackPlotRegion(df, ylim = c(0, ymax), panel.fun = function(region, value, ...) {
			i = getI(...)
			circos.genomicLines(region, value, col = col[i], lwd = lwd[i], lty = lty[i], type = type[i], 
				border = border[i], area = area[i], baseline = baseline[i]) 
		}, ...)
	}
}

# == title
# Calculate genomic region density
#
# == param
# -region Genomic positions. It can be a data frame with two
#     columns which are start positions and end positions on a single chromosome.
#     It can also be a bed-format data frame which contains the chromosome column.
# -window.size Window size to calculate genomic density
# -n.window number of windows, if it is specified, ``window.size`` is ignored
# -overlap Whether two neighbouring windows have half overlap
# -count_by How to count the value for each window, ``percent``: percent of the window covered by the input regions; ``number``: number of regions that overlap to the window.
# -chr.len the chromosome length. The value should be named vector
#
# == details
# It calculate the percent of each genomic windows that is covered by the input regions.
#
# == values
# If the input is a two-column data frame, the function returns a data frame with three columns: 
# start position, end position and the overlapping (value depends on the ``count_by`` argument). And if the input is a bed-format
# data frame, there will be an additionally chromosome name column.
#
# == example
# bed = generateRandomBed()
# bed = subset(bed, chr == "chr1")
# head(genomicDensity(bed))
# head(genomicDensity(bed, count_by = "number"))
genomicDensity = function(
	region, 
	window.size = 1e7, 
	n.window = NULL, 
	overlap = TRUE, 
	count_by = c("percent", "number"),
	chr.len = NULL) {
	
	region = validate_data_frame(region)

	if(is.character(region[, 1]) || is.factor(region[, 1])) {
		validate_region(region, check_chr = TRUE)
		region[, 1] = as.vector(region[, 1])
		all_chr = unique(region[, 1])
		if(!is.null(chr.len)) {
			if(length(all_chr) == 1 & length(chr.len) == 1) {
				names(chr.len) = all_chr[1]
			}
		}
		return(do.call("rbind", lapply(all_chr, function(chr) {
			l = region[,1] == chr
			if(is.null(chr.len)) {
				max_rg = NULL
				if(is.circos.initialized()) {
					if(chr %in% get.all.sector.index()) {
						max_rg = get.sector.data(sector.index = chr)["max.data"]
					} 
				}
			} else {
				if(chr %in% names(chr.len)) {
					max_rg = chr.len[chr]
				} else {
					max_rg = NULL
				}
			}
			df = genomicDensity(region[l, 2:3, drop = FALSE], window.size = window.size, overlap = overlap, chr.len = max_rg, count_by = count_by)
			cbind(chr = rep(chr, nrow(df)), df)
		})))
	}
	
	validate_region(region, 1, 2, check_chr = FALSE)
	if(ncol(region) >= 3) {
		if(is.numeric(region[, 1])) {
			if(max(region[, 1]) < 100) {
				return(do.call("rbind", lapply(unique(region[, 1]), function(chr) {
					l = region[, 1] == chr
					df = genomicDensity(region[l, 2:3, drop = FALSE], window.size = window.size, overlap = overlap, count_by = count_by)
					cbind(chr = rep(chr, nrow(df)), df)
				})))
			}
		}
	}

	region = region[, 1:2]
	
	region = sort_region(region)
	region = reduce_region(region)

	# make a segmentation
	if(!is.null(chr.len)) {
		max_pos = max(c(chr.len, max(region[[2]])))
	} else {
		max_pos = max(region[[2]])
	}
	if(overlap) {
		if(missing(n.window)) {
			b = seq(1, max_pos, by = window.size/2)
			s = b[-length(b)]
			s = s[-length(s)]
			e = s + window.size - 1
		} else {
			b = seq(1, max_pos, length.out = 2*n.window - 1)
			s = b[-length(b)]
			s = s[-length(s)]
			e = s + b[3] - b[1] - 1
		}
	} else {
		if(missing(n.window)) {
			b = seq(1, max_pos, by = window.size)
			s = b[-length(b)]
			e = s + window.size - 1
		} else {
			b = seq(1, max_pos, length.out = n.window)
			s = b[-length(b)]
			e = s + b[2] - b[1]	
		}
		
	}

	s = as.integer(s)
	e = as.integer(e)

	y = rep(0, length(s))
	names(y) = paste(s, e, sep = ",")
	
	windows = data.frame(start = s, end = e)
	
	op = overlap_region(windows, region, count_by = count_by)

	res = data.frame(start = s, end = e, value = op)
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
# The function is same as `get.current.sector.index` and
# should only be put inside ``panel.fun`` when using `circos.genomicTrackPlotRegion`.
get.current.chromosome = function() {
	get.current.sector.index()
}




is.dataFrameList = function(data) {
	is.list(data) && all(sapply(data, is.data.frame))
}


normalizeToDataFrame = function(data, sort = FALSE) {

	all.chr = get.all.sector.index()

	if(is.data.frame(data)) {
		data = as.data.frame(data)
		if(ncol(data) < 3) {
			stop_wrap("Your data frame is less than 3 column!.")
		}
		data = data[data[[1]] %in% all.chr, , drop = FALSE]
		if(sort) {
			data = data[order(data[[1]], data[[2]]), , drop = FALSE]
		}
		return(data)
	} else if(is.list(data) && all(sapply(data, is.data.frame))) {
		df = lapply(data, function(gr) {
			if(ncol(gr) < 3) {
				stop_wrap("Your data frame is less than 3 column!.")
			}
			gr = gr[gr[[1]] %in% all.chr, , drop = FALSE]
			if(sort) {
				gr = gr[order(gr[[1]], gr[[2]]), ]
			}
			return(gr)
		})
		return(df)
	} else if(inherits(df, "GRanges")) {
		df = as.data.frame(df)
		return(df)
	} else {
		stop_wrap("The format of `data` should only be a data frame or a list of data frames.")
	}
}


.normalizeGraphicalParam = function(x, nc, nr, name) {
	if(nc == 1) {
		if(!(length(x) == 1 || length(x) == nr)) {
			stop_wrap("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your regions (", nr, ").")
		} else if(length(x) == 1) {
			x = rep(x, nr)
		}
	} else {
		if(!(length(x) == 1 || length(x) == nc)) {
			stop_wrap("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your data column (", nc, ").")
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
# -data A bed-file-like data frame or a list of data frames.
# -mode How to calculate the distance of two neighbouring regions, pass to `rainfallTransform`.
# -ylim ylim for rainfall plot track. If ``normalize_to_width`` is ``FALSE``, the value should correspond to ``log10(dist+1)``,
#       and if ``normalize_to_width`` is ``TRUE``, the value should correspond to ``log2(rel_dist)``.
# -col  Color of points. It should be length of one. If ``data`` is a list, the length of ``col``
#       can also be the length of the list.
# -pch  Style of points.
# -cex  Size of points.
# -normalize_to_width If it is ``TRUE``, the value is the relative distance divided by the width of the region.
# -... Pass to `circos.trackPlotRegion`.
#
# == details
# This is high-level graphical function, which mean, it will create a new track.
#
# Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis
# corresponds to the distance to neighbour regions (log-based). So if there is a drop-down on
# the plot, it means there is a cluster of regions at that area.
#
# On the plot, y-axis are log10-transformed.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-density-and-rainfall-plot
#
# == example
# \donttest{
# load(system.file(package = "circlize", "extdata", "DMR.RData"))
#
# # rainfall
# circos.initializeWithIdeogram(plotType = c("axis", "labels"))
#
# bed_list = list(DMR_hyper, DMR_hypo)
# circos.genomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))
#
# circos.genomicDensity(bed_list[[1]], col = c("#FF000080"), track.height = 0.1)
# circos.genomicDensity(bed_list[[2]], col = c("#0000FF80"), track.height = 0.1)
#
# circos.clear()
# }
circos.genomicRainfall = function(
	data, 
	mode = "min", 
	ylim = NULL, 
	col = "black", 
	pch = par("pch"), 
	cex = par("cex"), 
	normalize_to_width = FALSE, 
	...) {
	
	data = normalizeToDataFrame(data)
	
	if(!is.dataFrameList(data)) {
		data = list(data)
	}
	
	for(i in seq_along(data)) {
		validate_region(data[[i]], check_chr = TRUE)
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

	if(is.null(ylim)) {
		if(normalize_to_width) {
			ylim = c(-10, 10)
		} else {
			ylim = c(0, 9)
		}
 	}
	
	circos.genomicTrackPlotRegion(data, ylim = ylim, panel.fun = function(region, value, ...) {
		df = rainfallTransform(region, mode = mode, normalize_to_width = normalize_to_width)
		i = getI(...)
		if(normalize_to_width) {
			circos.genomicPoints(df[1:2], log10(df[3]), col = col[i], cex = cex[i], pch = pch[i])
		} else {
			circos.genomicPoints(df[1:2], log10(df[3]+1), col = col[i], cex = cex[i], pch = pch[i])
		}
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
# -normalize_to_width If it is ``TRUE``, the value is the relative distance divided by the width of the region.
#
# == values
# If the input is a two-column data frame, the function returnes a data frame with three columns: start position, end position and distance.
# And if the input is a bed-format data frame, there will be the chromosome column added.
#
# The row order of the returned data frame is as same as the input one.
#
# == example
# bed = generateRandomBed()
# bed = subset(bed, chr == "chr1")
# head(rainfallTransform(bed))
#
rainfallTransform = function(
	region, 
	mode = c("min", "max", "mean", "left", "right"),
	normalize_to_width = FALSE) {
	
	mode = match.arg(mode)[1]

	region = validate_data_frame(region)
	if(is.character(region[, 1]) || is.factor(region[, 1])) {
		region[, 1] = as.vector(region[, 1])
		region = region[1:3]
		region$dist = NA
		for(chr in unique(region[, 1])) {
			l = region[, 1] == chr
			df = rainfallTransform(region[l, 2:3, drop = FALSE], mode = mode)
			region$dist[l] = df$dist
		}
		return(region)
	}
	if(ncol(region) >= 3) {
		if(is.numeric(region[, 1])) {
			if(max(region[, 1]) < 100) {
				region = as.data.frame(region)
				region[[1]] = as.character(region[[1]])
				rainfallTransform(region, mode = mode)
			}
		}
	}
	
	region_order = order_region(region[1:2])
	region_bk = as.data.frame(region[1:2])
	region = as.data.frame(region[region_order, ])
	n = nrow(region)
	dist = numeric(n)
		
	if(n < 2) {
		region = region_bk
		region$dist = NA
		return(region)
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
		} else if(mode == "mean") {
			dist[2:(n-1)] = apply(cbind(d1, d2), 1, mean)
		} else if (mode == "left") {
			dist[2:(n - 1)] = d2
		} else if (mode == "right") {
			dist[2:(n - 1)] = d1
		}
	}
		
	dist = ifelse(dist < 0, 0, dist)

	if(mode == "left") {
		dist[1] = NA
	} else if(mode == "right") {
		dist[n] = NA
	}

	region = region_bk
	region$dist[region_order] = dist

	if(normalize_to_width) {
		region$dist = region$dist/(region[, 3] - region[, 2])
	}
	
	return(region)
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
# -extend extend to allow labels to be put in an region which is wider than the current chromosome.
#    The value should be a proportion value and the length is either one or two.
# -... other arguments
#
# == details
# This position transformation function is designed specifically for text.
# Under the transformation, texts will be as close as possible to the original positions.
posTransform.text = function(
	region, 
	y, 
	labels, 
	cex = 1, 
	font = par("font"),
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), 
	padding = 0, 
	extend = 0, 
	...) {

	if(length(y) == 1) y = rep(y, nrow(region))
	if(length(labels) == 1) labels = rep(labels, nrow(region))

	if(length(extend) == 1) extend = rep(extend, 2)
	if(length(extend) > 2) extend = extend[1:2]

	od = order(region[, 1] + region[, 2])
	od_back = NULL
	od_back[od] = seq_along(od)
	region = region[od, ,drop = FALSE]
	y = y[od]
	labels = labels[od]

	text_height = strheight(labels, cex = cex, font = font)*(1+padding)
	
	d = circlize( (region[[1]] + region[[2]])/2, y, sector.index = sector.index, track.index = track.index)
	alpha1 = d[, "theta"] + as.degree(atan(text_height/2/d[, "rou"]))
	alpha2 = d[, "theta"] - as.degree(atan(text_height/2/d[, "rou"]))
	x1 = reverse.circlize(alpha1, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	x2 = reverse.circlize(alpha2, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	
	xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
	xrange = xlim[2] - xlim[1]
	xlim = c(xlim[1] - extend[1]*xrange, xlim[2] + extend[2]*xrange)

	x1_new = x1
	x2_new = x2
	l = x2 - x1 >= xlim[2] - xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = xlim[2]
	l = x1 < xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = x2[l] + xlim[1] - x1[l]
	l = x2 > xlim[2]; x1_new[l] = x1[l] - (x2[l] - xlim[2]); x2_new[l] = xlim[2]
	df = smartAlign(x1_new, x2_new, xlim = xlim)

	return(df[od_back, ,drop = FALSE])
}


# == title
# Add heatmaps for selected regions
#
# == param
# -bed A data frame in bed format, the matrix should be stored from the fourth column.
# -col Colors for the heatmaps. The value can be a matrix or a color mapping function generated by `colorRamp2`.
# -na_col Color for NA values.
# -numeric.column Column index for the numeric columns. The values can be integer index or character index.
#     By default it takes all numeric columns from the fourth column.
# -border Border of the heatmap grids.
# -border_lwd Line width for borders of heatmap grids.
# -border_lty Line style for borders of heatmap grids.
# -connection_height Height of the connection lines. If it is set to ``NULL``, no connection will be drawn.
#        Use `mm_h`/`cm_h`/`inches_h` to set a height in absolute unit.
# -line_col Color of the connection lines. The value can be a vector.
# -line_lwd Line width of the connection lines.
# -line_lty Line style of the connection lines.
# -heatmap_height Height of the heatmap track
# -side Side of the heatmaps. Is the heatmap facing inside or outside?
# -track.margin Bottom and top margins.
#
# == details
# The function visualizes heatmaps which correspond to a subset of regions in the genome.
# The correspondance between heatmaps and regions are identified by connection lines.
#
# The function actually creates two tracks, one track for the connection lines and one track
# for the heamtaps. The heatmaps always fill the whole track.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-heatmap
#
# == example
# \donttest{
# circos.initializeWithIdeogram(plotType = c("labels", "axis"))
# bed = generateRandomBed(nr = 100, nc = 4)
# col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
# circos.genomicHeatmap(bed, col_fun, side = "inside", border = "white")
# circos.genomicHeatmap(bed, col_fun, side = "outside", 
#     line_col = as.numeric(factor(bed[[1]])))
# }
circos.genomicHeatmap = function(
	bed, 
	col, 
	na_col = "grey",
	numeric.column = NULL, 
	border = NA, 
	border_lwd = par("lwd"), 
	border_lty = par("lty"), 
	connection_height = mm_h(5),
	line_col = par("col"), 
	line_lwd = par("lwd"), 
	line_lty = par("lty"),
	heatmap_height = 0.15, 
	side = c("inside", "outside"), 
	track.margin = circos.par("track.margin")) {

	bed = validate_data_frame(bed)
	validate_region(bed, check_chr = TRUE)

	mat = bed[, -(1:3), drop = FALSE]
	if(is.null(numeric.column)) {
		numeric.column = which(apply(mat, 2, "is.numeric"))
		if(length(numeric.column) == 0) {
			stop_wrap("You don't have numeric columns in `bed`.")
		}
	} else {
		if(is.numeric(numeric.column)) {
			numeric.column = numeric.column - 3
		}
	}
	mat = mat[, numeric.column, drop = FALSE]

	mat = as.matrix(mat)
	bed = cbind(bed[, 1:3], mat)

	side = match.arg(side)
	if(missing(col)) {
		col = colorRamp2(seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), length.out = 3), c("blue", "#EEEEEE", "red"))
	}
	if(is.function(col)) {
		col = col(mat)
	}
	col[is.na(col)] = na_col
	if(length(border) == 1) {
		if(is.na(border)) {
			border = col
		}
	}
	if(length(border) == 1) {
		border = rep(border, length(col))
		dim(border) = dim(col)
	}
	if(side == "inside") {
		if(!is.null(connection_height)) {
			circos.genomicPosTransformLines(bed, posTransform = posTransform.default, 
			    horizontalLine = "top", track.height = connection_height,
			    track.margin = c(convert_height(1, "mm"), track.margin[2]), 
			    cell.padding = c(0, 0, 0, 0),
			    col = line_col, lwd = line_lwd, lty = line_lty)
		}
		circos.genomicTrackPlotRegion(bed, stack = TRUE, 
		    panel.fun = function(region, value, ...) {
		    	l = bed[, 1] == CELL_META$sector.index
		    	i = getI(...)
		        circos.genomicRect(region, value, col = col[l, i], 
		            border = border[l, i], lwd = border_lwd, lty = border_lty, 
		            posTransform = posTransform.default, ...)
			}, bg.border = NA, track.height = heatmap_height, track.margin = c(track.margin[1], 0),
			cell.padding = c(0, 0, 0, 0))
	} else {
		circos.genomicTrackPlotRegion(bed, stack = TRUE, 
		    panel.fun = function(region, value, ...) {
		    	l = bed[, 1] == CELL_META$sector.index
		    	i = getI(...)
		        circos.genomicRect(region, value, col = col[l, i], 
		            border = border[l, i], lwd = border_lwd, lty = border_lty, 
		            posTransform = posTransform.default, ...)
			}, bg.border = NA, track.height = heatmap_height, track.margin = c(0, track.margin[2]),
			cell.padding = c(0, 0, 0, 0))
		if(!is.null(connection_height)) {
			circos.genomicPosTransformLines(bed, posTransform = posTransform.default, 
			    direction = "outside", horizontalLine = "bottom", track.height = connection_height,
			    track.margin = c(track.margin[2], convert_height(1, "mm")), 
			    cell.padding = c(0, 0, 0, 0), 
			    col = line_col, lwd = line_lwd, lty = line_lty)
		}
	}
}

# == title
# Add labels to specified genomic regions
#
# == param
# -bed A data frame in bed format.
# -labels A vector of labels corresponding to rows in ``bed``.
# -labels.column If the label column is already in ``bed``, the index for this column in ``bed``.
# -facing fFacing of the labels. The value can only be ``"clockwise"`` or ``"reverse.clockwise"``.
# -niceFacing Whether automatically adjust the facing of the labels.
# -col Color for the labels.
# -cex Size of the labels.
# -font Font of the labels.
# -padding Padding of the labels, the value is the ratio to the height of the label.
# -connection_height Height of the connection track.
# -line_col Color for the connection lines.
# -line_lwd Line width for the connection lines.
# -line_lty Line type for the connectioin lines.
# -labels_height Height of the labels track.
# -side Side of the labels track, is it in the inside of the track where the regions are marked?
# -labels.side Same as ``side``. It will replace ``side`` in the future versions.
# -track.margin Bottom and top margins.
#
# == details
# The function adds labels for the specified regions. The positions of labels are arranged
# so that they are not overlapping to each other.
#
# This function creates two tracks, one for the connection lines and one for the labels.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#labels
# 
# == example
# \donttest{
# circos.initializeWithIdeogram()
# bed = generateRandomBed(nr = 50, fun = function(k) sample(letters, k, replace = TRUE))
# bed[1, 4] = "aaaaa"
# circos.genomicLabels(bed, labels.column = 4, side = "inside")
# circos.clear()
#
# circos.initializeWithIdeogram(plotType = NULL)
# circos.genomicLabels(bed, labels.column = 4, side = "outside",
#     col = as.numeric(factor(bed[[1]])), line_col = as.numeric(factor(bed[[1]])))
# circos.genomicIdeogram()
# circos.clear()
# }
circos.genomicLabels = function(
	bed, 
	labels = NULL, 
	labels.column = NULL,
	facing = "clockwise", 
	niceFacing = TRUE,
	col = par("col"), 
	cex = 0.8, 
	font = par("font"), 
	padding = 0.4,
	connection_height = mm_h(5), 
	line_col = par("col"), 
	line_lwd = par("lwd"), 
	line_lty = par("lty"),
	labels_height = min(c(cm_h(1.5), max(strwidth(labels, cex = cex, font = font)))),
	side = c("inside", "outside"), 
	labels.side = side,
	track.margin = circos.par("track.margin")) {

	bed = validate_data_frame(bed)
	validate_region(bed, check_chr = TRUE)
	
	if(is.null(labels)){
		labels = bed[[labels.column]]
	}
	bed[[4]] = as.vector(labels)
	bed[[1]] = factor(as.vector(bed[[1]]), levels = get.all.sector.index())

	od = order(bed[[1]], bed[[2]])
	bed = bed[od, , drop = FALSE]
	bed[[1]] = as.vector(bed[[1]])

	# order `bed`

	if(length(col) == 1) col = rep(col, nrow(bed))
	if(length(cex) == 1) cex = rep(cex, nrow(bed))
	if(length(font) == 1) font = rep(font, nrow(bed))
	if(length(line_col) == 1) line_col = rep(line_col, nrow(bed))
	if(length(line_lwd) == 1) line_lwd = rep(line_lwd, nrow(bed))
	if(length(line_lty) == 1) line_lty = rep(line_lty, nrow(bed))

	col = col[od]
	cex = cex[od]
	font = font[od]
	line_col = line_col[od]
	line_lwd = line_lwd[od]
	line_lty = line_lty[od]

	if(!circos.par$xaxis.clock.wise) {
		all_chr_vec = as.character(bed[, 1])
		bed[, 2] = .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "max.value"] - bed[, 2] + .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "min.value"]
		bed[, 3] = .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "max.value"] - bed[, 3] + .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "min.value"]
		bed[, 2:3] = bed[, 3:2]
		circos.par(xaxis.clock.wise = TRUE)
		on.exit(circos.par(xaxis.clock.wise = FALSE))
	}

	chr = get.all.sector.index()[1]
	sector_data = get.sector.data(chr)
	
	## find the largest gap
	all_chr = unique(bed[, 1])
	rho = NULL
	for(cr in all_chr) {
		sub_bed = bed[bed[, 1] == cr, ]
		rho = c(rho, circlize(sub_bed[, 2], y = rep(1, nrow(sub_bed)), sector.index = cr)[, 1])
	}
	if(length(rho) == 0) {
		anchor = ((sector_data["start.degree"] + sector_data["end.degree"])/2 + 180) %% 360
	} else if(length(rho) == 1) {
		anchor = (rho + 180) %% 360
	} else {
		rho = sort(rho)
		rho = c(rho, rho[1] + 360)
		i = which.max(diff(rho))
		anchor = ((rho[i] + rho[i+1])/2) %% 360
	}

	# map all other chromosomes to the first chromosome
	chr_width = sector_data["start.degree"] - sector_data["end.degree"]
	# extend = (360 - chr_width)/chr_width
	# extend = c(0, extend)
	extend = numeric(2)
	s1 = sector_data["start.degree"] %% 360
	s2 = sector_data["end.degree"] %% 360
	# if the anchor in inside the first sector
	if(s1 < s2) { # the first sector go across theta = 0
		s1 = s1 + 360
	} 

	if(anchor >= s2 && anchor <= s1) { # anchor inside sector
		if(s1 - s2 > 180) {
			extend[1] = (abs(s1 - anchor) %% 360)/chr_width
			extend[2] = -(abs(s2 - anchor) %% 360)/chr_width
		} else {
			extend = (360 - chr_width)/chr_width
			extend = c(0, extend)
		}
	} else {
		extend[1] = ((anchor - s1) %% 360)/chr_width  # goes reverse clockwise
		extend[2] = ((s2 - anchor) %% 360)/chr_width  # goes clockwise
	}
	extend = abs(extend)

	if(0) segments(0, 0, cos(anchor/180*pi), sin(anchor/180*pi))

	# start.degree is always larger than end.degree
	new_chr_range = c(sector_data["start.degree"] + chr_width*extend[1], sector_data["end.degree"] - chr_width*extend[2])

	all_chr = unique(bed[, 1])
	bed2 = NULL
	for(cr in all_chr) {
		sub_bed = bed[bed[, 1] == cr, ]
		if(cr != chr) {
			dfx1 = circlize(sub_bed[, 2], y = rep(1, nrow(sub_bed)), sector.index = cr)
			dfx2 = circlize(sub_bed[, 3], y = rep(1, nrow(sub_bed)), sector.index = cr)
			degree_diff = dfx2[, 1] - dfx1[, 1]
			
			l = dfx1[, 1] > new_chr_range[1] | dfx1[, 1] < new_chr_range[2]
			if(any(l)) dfx1[l, 1] = (dfx1[l, 1] - new_chr_range[2]) %% 360 + new_chr_range[2]
			x1 = reverse.circlize(dfx1, sector.index = chr)[, 1]
			
			dfx2[, 1] = dfx1[, 1] + degree_diff
			x2 = reverse.circlize(dfx2, sector.index = chr)[, 1]
			sub_bed[, 2:3] = data.frame(start = x1, end = x2)
		}
		bed2 = rbind(bed2, sub_bed)
	}
	bed2[, 1] = chr

	if(!facing %in% c("clockwise", "reverse.clockwise")) {
		stop_wrap("facing can only be 'clockwise' or `reverse.clockwise`.")
	}

	op = circos.par("points.overflow.warning")
	circos.par("points.overflow.warning" = FALSE)
	labels.side = side = match.arg(side)[1]
	if(labels.side == "inside") {
		# an empty track
		circos.genomicTrackPlotRegion(bed2, ylim = c(0, 1), 
			track.margin = c(convert_height(0.5, "mm"), track.margin[2]), 
		    cell.padding = c(0, 0, 0, 0),
		    track.height = connection_height, bg.border = NA)
		i_track = get.cell.meta.data("track.index")

		# add labels
		circos.genomicTrackPlotRegion(bed2, ylim = c(0, 1), panel.fun = function(region, value, ...) {
			l = bed2[[1]] == CELL_META$sector.index
			circos.genomicText(region, value, y = 1, labels.column = 1, facing = facing, adj = c((facing == "clockwise") + 0, 0.5),
				posTransform = posTransform.text, col = col[l], cex = cex[l], font = font[l], niceFacing = niceFacing,
				padding = padding, extend = extend)
		}, track.height = labels_height, bg.border = NA, track.margin = c(track.margin[1], 0),
		cell.padding = c(0, 0, 0, 0))

		circos.genomicPosTransformLines(bed2, 
			posTransform = function(region, value) {
				l = bed2[[1]] == get.cell.meta.data("sector.index", track.index = i_track + 1)
				posTransform.text(region, y = 1, labels = value[[1]], cex = cex[l], font = font[l], 
					track.index = i_track + 1, padding = padding, extend = extend)
			},
			direction = "inside", horizontalLine = "top", track.index = i_track,
			cell.padding = c(0, 0, 0, 0),
			col = line_col, lwd = line_lwd, lty = line_lty
		)
	} else {
		circos.genomicTrackPlotRegion(bed2, ylim = c(0, 1), panel.fun = function(region, value, ...) {
			l = bed2[[1]] == CELL_META$sector.index
			circos.genomicText(region, value, y = 0, labels.column = 1, facing = facing, adj = c((facing == "reverse.clockwise") + 0, 0.5),
				posTransform = posTransform.text, col = col[l], cex = cex[l], font = font[l], niceFacing = niceFacing,
				padding = padding, extend = extend)
		}, track.height = labels_height, bg.border = NA, track.margin = c(0, track.margin[2]),
		cell.padding = c(0, 0, 0, 0))
		i_track = get.cell.meta.data("track.index")

		circos.genomicPosTransformLines(bed2, 
			posTransform = function(region, value) {
				l = bed2[[1]] == get.cell.meta.data("sector.index", track.index = i_track)
				posTransform.text(region, y = 0, labels = value[[1]], cex = cex[l], font = font[l], 
					track.index = i_track, padding = padding, extend = extend)
			},
			direction = "outside", horizontalLine = "bottom",
			col = line_col, lwd = line_lwd, lty = line_lty, track.height = connection_height,
			track.margin = c(track.margin[1], convert_height(0.5, "mm")), 
		    cell.padding = c(0, 0, 0, 0)
		)
	}
	circos.par("points.overflow.warning" = op)
}

# == title
# Adjust positions of text
#
# == param
# -x1 Position which corresponds to the top of the text.
# -x2 Position which corresponds to the bottom of the text.
# -xlim Ranges on x-axis.
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
			len = x2[index] - x1[index]
			total_len = sum(len)
			mid = (min(x1[index]) + max(x2[index]))/2
			if(total_len > xlim[2] - xlim[1]) {
				tp = c(0, cumsum(len)) + (xlim[1] + xlim[2])/2 - total_len/2
			} else if(mid - total_len/2 < xlim[1]) {
				tp = c(0, cumsum(len)) + xlim[1]
			} else if(mid + total_len/2 > xlim[2]) {
				tp = xlim[2] - total_len + c(0, cumsum(len))
			} else {
				tp = c(0, cumsum(len)) + mid - total_len/2
			}
			new_x1[index] = tp[-length(tp)]
			new_x2[index] = tp[-1]
		}
		
		x1 = new_x1
		x2 = new_x2
	}
	
	return(data.frame(start = x1, end = x2))
}

