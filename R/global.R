# this file contains variables and functions related to
# global variables.

.CIRCOS.ENV = new.env()

resetGlobalVariable = function() {
	assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
}

resetGlobalVariable()

# == title
# Parameters for circos layout
#
# == param
# -... Arguments for the parameters, see "details" section
# -RESET reset to default values
# -READ.ONLY whether only return read-only options
# 
# == details
# Global parameters for the circos layout. Currently supported parameters are:
#
# -start.degree            The starting degree from which the circle begins to draw. Note this degree is measured
#     in the standard polar coordinate which means it is always reverse-clockwise.
# -gap.degree              Gap between two neighbour sectors. It can be a single value or a vector. If it is a vector,
#                          the first value corresponds to the gap after the first sector.
# -track.margin            Like ``margin`` in Cascading Style Sheets (CSS), it is the blank area
#     out of the plotting region, also outside of the borders. Since left and right margin are controlled
#     by ``gap.degree``, only bottom and top margin need to be set. And all cells in a same track share the same margins, and
#     that's why this parameter is called ``track.margin``. The value for the ``track.margin``
#     is the percentage according to the radius of the unit circle.
# -unit.circle.segments    Since curves are simulated by a series of straight lines,
#     this parameter controls the amount of segments to represent a curve. The minimal length
#     of the line segmentation is the length of the unit circle (``2pi``) divided by ``unit.circoe.segments``.
#     More segments means better approximation for the curves while larger size if you generate figures as PDF format.
# -cell.padding            Padding of the cell. Like ``padding`` in Cascading Style Sheets
#    (CSS), it is the blank area around the plotting regions, but within the borders.
#     The parameter has four values, which controls the bottom, left, top and right paddings
#     respectively. The first and the third padding
#     values are the percentages according to the radius of the unit circle and the second and
#     fourth values are degrees.
# -track.height    The default height of tracks. It is the percentage according to the radius
#     of the unit circle. The height includes the top and bottom cell paddings but not the margins.
# -points.overflow.warning Since each cell is in fact not a real plotting region but only
#     an ordinary rectangle, it does not eliminate points that are plotted out of
#     the region. So if some points are out of the plotting region, ``circlize`` would continue drawing the points but print warnings. In some 
#     cases, draw something out of the plotting region is useful, such as draw
#     some legend or text. Set this value to ``FALSE`` to turn off the warnings.
# -canvas.xlim              The coordinate for the canvas. Because ``circlize`` draws everything (or almost everything) inside the unit circle,
#     the default ``canvas.xlim`` and ``canvas.ylim`` for the canvas would be all ``c(-1, 1)``. However, you can set it to a more broad
#     interval if you want to draw other things out of the circle. By choosing proper
#     ``canvas.xlim`` and ``canvas.ylim``, you can draw part of the circle. E.g. setting
#     ``canvas.xlim`` to ``c(0, 1)`` and ``canvas.ylim`` to ``c(0, 1)`` would only draw
#     circle in the region of (0, pi/2).
# -canvas.ylim              The coordinate for the canvas. By default it is ``c(-1, 1)``
# -clock.wise               The direction for adding sectors. Default is ``TRUE``.
#
# Similar as `graphics::par`, you can get the parameter values by specifying the 
# names of parameters and you can set the parameter values by specifying a
# named list which contains the new values.
#
# ``gap.degree``, ``start.degree``, ``canvas.xlim``, ``canvas.ylim`` and ``clock.wise`` 
# only be set before the initialization of circos layout
# (i.e. before calling `circos.initialize`) because these values will not be changed after
# adding sectors on the circle. The left and right padding for ``cell.padding`` will also be
# ignored after the initialization because all cells in a sector would share the same
# left and right paddings. 
circos.par = function(..., RESET = FALSE, READ.ONLY = NULL) {}
circos.par = setGlobalOptions(
	start.degree = list(
		.value = 0,
		.length = 1,
		.class = "numeric",
		.filter = function(x) {
			if(is.circos.initialized()){
				warning(paste("'start.degree' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
			}
			return(x)
		}),
	gap.degree = list(
		.value = 1,
		.class = "numeric",
		.validate = function(x) {
			all(x >= 0 & x < 360)
		},
		.filter = function(x) {
			if(is.circos.initialized()){
				warning(paste("'gap.degree' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
			}
			return(x)
		}
		),
	track.margin = list(
		.value = c(0.01, 0.01),  # top margin and bottom margin, percentage
		.length = 2,
		.class = "numeric"
		),
	unit.circle.segments = 500,   #to simulate smooth curve
	cell.padding = list(
		.value = c(0.02, 1, 0.02, 1),  # percentage
		.length = c(2, 4),
		.class = "numeric",
		.filter = function(x) {
			if(length(x) == 2) x = c(x, x)
			o.cell.padding = circos.par("cell.padding")
			if(is.circos.initialized()){
				return(c(x[1], o.cell.padding[2], x[3], o.cell.padding[4]))
			} else {
				return(x)
			}
		}),
	default.track.height = list(
		.value = 0.2,
		.visible = FALSE,
		.filter = function(x) {
			warning("`default.track.height` is replaced by `track.height`, ignore this setting.\n")
			return(x)
		}),
	track.height = 0.2,
	points.overflow.warning = TRUE,
	canvas.xlim = list(
		.value = c(-1, 1),
		.filter = function(x) {
			if(is.circos.initialized()){
				warning(paste("'canvas.xlim' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
			}
			return(x)
		}),
	canvas.ylim = list(
		.value = c(-1, 1),
		.filter = function(x) {
			if(is.circos.initialized()){
				warning(paste("'canvas.ylim' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
			}
			return(x)
		}),
	major.by.degree = 10,
	clock.wise = list(
		.value = TRUE,
		.filter = function(x) {
			if(is.circos.initialized()){
				warning(paste("'clock.wise' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
			}
			return(x)
		}),
	lend = list(
		.value = NULL,
		.visible = FALSE,
		.private = TRUE),
	ljoin = list(
		.value = NULL,
		.visible = FALSE,
		.private = TRUE),
	'__tempdir__' = list(
		.value = ".",
		.private = TRUE,
		.filter = function(x) {dir.create(x, showWarnings = FALSE); return(x)},
		.visible = FALSE
	)
)

# before initialization, .SECTOR.DATA is NULL
is.circos.initialized = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	return(! is.null(.SECTOR.DATA))
}

# == title
# Initialize the circos layout
#
# == param
# -factors Factors which represent data categories
# -x       Data on x-axis, a vector
# -xlim    Limitations for values on x-axis
# -sector.width Width for each sector. The length of the vector should be either 1 which means
#          all sectors have same width or as same as the number of sectors. Values for
#          the vector are relative, and they will be scaled by dividing their summation.
#          By default, it is ``NULL`` which means the width of sectors correspond to the data
#          range in sectors which is calculated internally.
#
# == details
# The function allocates the sectors according to the values on x-axis.
# The number of sectors are determined by the ``factors`` and the order
# of sectors are determined by the levels of factors. In this function,
# the start and end position for each sector on the circle (measured by degree)
# are calculated according to the values on x-axis.
#
# If ``x`` is set, the length of ``x`` must be equal to the length of ``factors``.
# Then the data range for each sector are calculated from ``x`` and ``factors``.
#
# If ``xlim`` is set, it should be a vector containing two numbers or a matrix with 2 columns.
# If ``xlim`` is a 2-element vector, it means all sector share the same ``xlim``.
# If ``xlim`` is a 2-column matrix, the number of rows should be equal to the number of categories (number of levels)
# identified by ``factors``, then each row of ``xlim`` corresponds to the data range for each sector
# and the order of rows is corresponding to the order of levels of ``factors``.
#
# Normally, width of sectors will be calculated internally according to the data range in sectors. But you can
# still set the width manually. However, it is not always a good idea to change the default sector width since
# the width can reflect the range of data in sectors. Anyway, in some cases, it is useful to manually set
# the width such as you want to zoom in some part of the sectors.
#
# The function finally calls `graphics::plot` and be ready for adding graphics.
circos.initialize = function(factors, x = NULL, xlim = NULL, sector.width = NULL) {

    resetGlobalVariable()
	
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
	
	if(any(factors == "")) {
		stop("`factors` cannot contain empty strings.\n")
	}
	
    if(! is.factor(factors)) {
        factors = factor(factors)
    }
    le = levels(factors)
    
    # initialize .SECTOR.DATA
    # you can think it as the global x axis configuration
    # calculate min and max value for each sectors
    # there are several ways
	# xlim is prior than x
    if(is.vector(xlim)) {
        if(length(xlim) != 2) {
            stop("Since `xlim` is vector, it should have length of 2.\n")
        }    
        
        min.value = rep(xlim[1], length(le))
        max.value = rep(xlim[2], length(le))
    } else if(is.matrix(xlim)) {
        if(dim(xlim)[1] != length(le) || dim(xlim)[2] != 2) {
            stop("Since `xlim` is a matrix, it should have same number of rows as the length of the level of `factors` and number of columns of 2.\n")
        }
        
        min.value = apply(xlim, 1, function(x) x[1])
        max.value = apply(xlim, 1, function(x) x[2])
    } else if(is.vector(x)) {
    
        if(length(x) != length(factors)) {
            stop("Length of `x` and length of `factors` differ.\n")
        }
        min.value = tapply(x, factors, min)
        max.value = tapply(x, factors, max)
    } else {
		stop("You should specify either `x` or `xlim`.\n")
	}
    
    cell.padding = circos.par("cell.padding")
    
	# range for sectors
    sector.range = max.value - min.value
    n.sector = length(le)
    
    sector = vector("list", 7)
	# for each sector, `start.degree always referto `min.value` and `end.degree` always
	# refer to `max.value` in a reverse clockwise fasion. So here `start.degree` and 
	# `end.degree` also correspond to the direction.
	# So in the polar coordinate, `start.degree` would be larger than `end.degree`
    names(sector) = c("factor", "min.value", "max.value", "start.degree", "end.degree", "min.data", "max.data")
    sector[["factor"]] = le
	sector[["min.data"]] = min.value
	sector[["max.data"]] = max.value
    
    gap.degree = circos.par("gap.degree")
	if(length(gap.degree) == 1) {
		gap.degree = rep(gap.degree, n.sector)
	} else if(length(gap.degree) != n.sector) {
		stop("Since `gap.degree` parameter has length larger than 1, it should have same length as number of levels of factors.\n")
	}
	
	start.degree = circos.par("start.degree")
	clock.wise = circos.par("clock.wise")
    
    if(360 - sum(gap.degree) <= 0) {
		stop("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.\n")
	}
		
    if(is.null(sector.width)) {
		# degree per data
		unit = (360 - sum(gap.degree)) / sum(sector.range)
		
		for(i in seq_len(n.sector)) {
			
			if(sector.range[i] == 0) {
				stop(paste("Range of the sector (", le[i] ,") cannot be 0.\n", sep = ""))
			}
			
			# only to ensure value are always increasing or decreasing with the absolute degree value
			if(clock.wise) {
				sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
				sector[["end.degree"]][i] =  sector[["start.degree"]][i] - sector.range[i]*unit
			} else {
				sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
				sector[["start.degree"]][i] = sector[["end.degree"]][i] + sector.range[i]*unit   
			}
		}
	} else {
		if(length(sector.width) == 1) {
			sector.width = rep(sector.width, n.sector)
		} else if(length(sector.width) != n.sector) {
			stop("Since you manually set the width for each sector, the length of `sector.width` should be either 1 or as same as the number of sectors.\n")
		}
		
		sector.width.percentage = sector.width / sum(sector.width)
		degree.per.sector = (360 - sum(gap.degree)) * sector.width.percentage
		
		if(any(degree.per.sector <= 0)) {
			stop("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.\n")
		}
		
		for(i in seq_len(n.sector)) {
			
			if(sector.range[i] == 0) {
				stop(paste("Range of the sector (", le[i] ,") cannot be 0.\n", sep = ""))
			}
			
			
			# only to ensure value are always increasing or decreasing with the absolute degree value
			if(clock.wise) {
				sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
				sector[["end.degree"]][i] =  sector[["start.degree"]][i] - degree.per.sector[i]
			} else {
				sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
				sector[["start.degree"]][i] = sector[["end.degree"]][i] + degree.per.sector[i] 
			}
		}
	}
	# from start.degree, degree is increasing in a reverse-clock wise fasion
	# so, if circos is created clock wise, the forward sector would have large degrees
	# if circos is created reverse clock wise, the forward sector would have small degrees
	# just for goodlooking for the degree
	if(clock.wise) {
		sector[["start.degree"]] = sector[["start.degree"]] + 360
		sector[["end.degree"]] = sector[["end.degree"]] + 360
	}
	
	if(any(cell.padding[2] + cell.padding[4] >= sector[["start.degree"]] - sector[["end.degree"]])) {
		stop("Summation of cell padding on x-direction are larger than the width for some sectors.\n")
	}
	
	min.value = min.value - cell.padding[2]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real min value
    max.value = max.value + cell.padding[4]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real max value
    sector[["min.value"]] = min.value
    sector[["max.value"]] = max.value
    
    sector = as.data.frame(sector, stringsAsFactors = FALSE)
    .SECTOR.DATA = sector
    
    # initialize .CELL.DATA which contains information of each cell
    # if content of that cell has been created, it means that the 
    # plotteing region for that cell has been created.
    .CELL.DATA = vector("list", length = length(le))
    names(.CELL.DATA) = le
    for(i in seq_along(.CELL.DATA)) {
        .CELL.DATA[[ le[i] ]] = vector("list", length = 0)
    }
	
	assign(".SECTOR.DATA", .SECTOR.DATA, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", .CELL.DATA, envir = .CIRCOS.ENV)
    
	
    # draw everything in a unit circle
	plot(circos.par("canvas.xlim"), circos.par("canvas.ylim"), type = "n", ann = FALSE, axes = FALSE)
    
	# all the information of cells would be visited through `get.cell.meta.data`
	return(invisible(NULL))
}

# == title
# Reset the circos layout parameters
#
# == details
# Because there are several
# parameters for circos plot which can only be set before `circos.initialize`. So before you draw the next
# circos plot, you need to reset these parameters.
#
# If you meet some errors when re-drawing the circos plot, try running this function and it will solve most of the problems.
circos.clear = function() {
    
	resetGlobalVariable()
	circos.par(RESET = TRUE)
	
    return(invisible(NULL))
}

# == title
# Get index for all sectors
#
# == details
# Simple function returning a vector of all sector index.
get.all.sector.index = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	if(is.null(.SECTOR.DATA)) {
		return(character(0))
	} else {
		return(as.vector(.SECTOR.DATA$factor))
	}
}

# == title
# Get index for all tracks
#
# == details
# Simple function returning a vector of all track index.
get.all.track.index = function() {
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
	if(is.null(.CELL.DATA)) {
		return(integer(0))
	} else {
		return(seq_along(.CELL.DATA[[1]]))
	}
}

get.sector.data = function(sector.index = get.current.sector.index()) {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
    sector.data = as.vector(as.matrix(.SECTOR.DATA[.SECTOR.DATA[[1]] == sector.index, -1]))
    names(sector.data) = colnames(.SECTOR.DATA)[-1]
    return(sector.data)
}

# numeric index, i.e. 1, 2, 3, ...
get.current.track.index = function() {
	.CURRENT.TRACK.INDEX = get(".CURRENT.TRACK.INDEX", envir = .CIRCOS.ENV)
    return(.CURRENT.TRACK.INDEX)   
}

set.current.track.index = function(x) {
	.CURRENT.TRACK.INDEX = x
	assign(".CURRENT.TRACK.INDEX", .CURRENT.TRACK.INDEX, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}

# factors name, note it is not numeric index
get.current.sector.index = function() {
	.CURRENT.SECTOR.INDEX = get(".CURRENT.SECTOR.INDEX", envir = .CIRCOS.ENV)
    return(.CURRENT.SECTOR.INDEX)   
}

set.current.sector.index = function(x) {
	.CURRENT.SECTOR.INDEX = get(".CURRENT.SECTOR.INDEX", envir = .CIRCOS.ENV)
	if(!x %in% get.all.sector.index()) {
		stop(paste0("Cannot find ", x, " in all available sector names.\n"))
	}
    .CURRENT.SECTOR.INDEX = x
	assign(".CURRENT.SECTOR.INDEX", .CURRENT.SECTOR.INDEX, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}


get.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    .CELL.DATA[[sector.index]][[track.index]]
}

set.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index(), ...) {
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    .CELL.DATA[[sector.index]][[track.index]] = list(...)
	assign(".CELL.DATA", .CELL.DATA, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}

# whether cell in sector.index, track.index exists?
has.cell = function(sector.index, track.index) {

	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    if(sector.index %in% names(.CELL.DATA) &&
       track.index <= length(.CELL.DATA[[sector.index]]) &&
       !is.null(.CELL.DATA[[sector.index]][[track.index]])) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# == title
# Get information of the circos plot
#
# == param
# -sector.index Which sectors you want to look at? It can be a vector.
# -track.index  Which tracks you want to look at? It can be a vector.
# -plot         Whether to add information on the plot
#
# == details
# It tells you the basic parameters for sectors/tracks/cells. If both ``sector.index``
# and ``track.index`` are set to ``NULL``, the function would print index for 
# all sectors and all tracks. If ``sector.index`` and/or ``track.index`` are set,
# the function would print ``xlim``, ``ylim``, ``cell.xlim``, ``cell.ylim``,
# ``xplot``, ``yplot``, ``track.margin`` and ``cell.padding`` for every cell in specified sectors and tracks.
# Also, the function will print index for your current sector and current track.
#
# If ``plot`` is set to ``TRUE``, the function will plot the index of the sector and the track 
# for each cell on the figure.
circos.info = function(sector.index = NULL, track.index = NULL, plot = FALSE) {
	sectors = get.all.sector.index()
	tracks = get.all.track.index()
		
	if(plot) {
		for(i in seq_along(sectors)) {
			for(j in seq_along(tracks)) {
				cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sectors[i], track.index = j)
				cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sectors[i], track.index = j)
				circos.text(mean(cell.xlim), mean(cell.ylim), labels = paste(sectors[i], j, sep = ":"),
					sector.index = sectors[i], track.index = j, facing = "downward")
			}
		}
	} else {
		# just print the name and xlim for each sector
		if(is.null(sector.index) && is.null(track.index)) {
			if(length(sectors)) {
				cat("All your sectors:\n")
				print(sectors)
			} else {
				cat("No sector has been created\n")
			}
			cat("\n")
			if(length(tracks)) {
				cat("All your tracks:\n")
				print(tracks)
			} else {
				cat("No track has been created\n")
			}
			cat("\n")

		} else {
			if(is.null(track.index)) {
				track.index = tracks
			} else if(is.null(sector.index)) {
				sector.index = sectors
			}
			for(i in seq_along(sector.index)) {
				for(j in seq_along(track.index)) {
					cat("sector index: '", sector.index[i], "'\n", sep = "")
					cat("track index: ", track.index[j], "\n", sep = "")
					xlim = get.cell.meta.data('xlim', sector.index[i], track.index[j])
					ylim = get.cell.meta.data('ylim', sector.index[i], track.index[j])
					cell.xlim = get.cell.meta.data("cell.xlim", sector.index[i], track.index[j])
					cell.ylim = get.cell.meta.data("cell.ylim", sector.index[i], track.index[j])
					xplot = get.cell.meta.data("xplot", sector.index[i], track.index[j])
					yplot = get.cell.meta.data("yplot", sector.index[i], track.index[j])
				    track.margin = get.cell.meta.data("track.margin", sector.index[i], track.index[j])
				    cell.padding = get.cell.meta.data("cell.padding", sector.index[i], track.index[j])
					cat("xlim: [", xlim[1], ", ", xlim[2], "]\n", sep = "")
					cat("ylim: [", ylim[1], ", ", ylim[2], "]\n", sep = "")
					cat("cell.ylim: [", cell.ylim[1], ", ", cell.ylim[2], "]\n", sep = "")
					cat("cell.ylim: [", cell.ylim[1], ", ", cell.ylim[2], "]\n", sep = "")
					cat("xplot (degree): [", xplot[1], ", ", xplot[2], "]\n", sep = "")
					cat("yplot (radius): [", yplot[1], ", ", yplot[2], "]\n", sep = "")
					cat("track.margin: c(", track.margin[1], ", ", track.margin[2], ")\n", sep = "")
					cat("cell.padding: c(", cell.padding[1], ", ", cell.padding[2], ", ", cell.padding[3], ", ", cell.padding[4], ")\n", sep = "")
					cat("\n")
				}
			}
				
		}

		if(length(get.current.sector.index())) cat("Your current sector.index is ", get.current.sector.index(), "\n", sep = "")
		if(get.current.track.index() > 0) cat("Your current track.index is ", get.current.track.index(), "\n", sep = "")
	}
	
}


# == title
# Label the sector index and the track index on each cell
#
# == details
# This function is deprecated, please use `circos.info` instead.
show.index = function() {
	circos.info(plot = TRUE)
	warning("`show.index` is deprecated, please use `circos.info` instead.\n")
}

# == title
# Get the meta data of a cell
#
# == param
# -name         Only support one name at a time, see "details" section
# -sector.index Index of the sector
# -track.index  Index of the track
#
# == details
# The following meta information for a cell can be obtained:
#
# -sector.index         The name (index) for the sector
# -sector.numeric.index Numeric index for the sector
# -track.index          Numeric index for the track
# -xlim                 Minimal and maximal values on the x-axis
# -ylim                 Minimal and maximal values on the y-axis
# -xrange               Range of ``xlim``. It equals to ``xlim[2] - xlim[1]`` 
# -yrange               Range of ``ylim``
# -xcenter              Center of x-axis. It equals to ``(xlim[2] + xlim[1])/2`` 
# -ycenter              Center of y-axis
# -cell.xlim            Minimal and maximal values on the x-axis extended by cell paddings
# -cell.ylim            Minimal and maximal values on the y-axis extended by cell paddings
# -xplot                Degrees for right and left borders of the cell.
# -yplot                Radius for top and bottom borders of the cell.
# -cell.start.degree    Same as ``xplot[1]``
# -cell.end.degree      Same as ``xplot[2]``
# -cell.bottom.radius   Same as ``yplot[1]``
# -cell.top.radius      Same as ``yplot[2]``
# -track.margin         Margin for the cell
# -cell.padding         Padding for the cell
#
# The function is useful when using ``panel.fun`` in `circos.trackPlotRegion` to
# get detailed information of the current cell.
get.cell.meta.data = function(name, sector.index = get.current.sector.index(), 
                              track.index = get.current.track.index()) {
	if(length(sector.index) == 0) {
		stop("It seems the circos plot has not been initialized.\n")
	}
	if(length(track.index) == 0) {
		stop("It seems the track has not been created.")
	}
	if(length(sector.index) != 1) {
		stop("Length of `sector.index` should only be 1.\n")
	}
	if(length(track.index) != 1) {
		stop("Length of `track.index` should only be 1.\n")
	}
	if(!any(sector.index %in% get.all.sector.index())) {
		stop(paste0("Cannot find sector: ", sector.index, ".\n"))
	}
	if(!any(track.index %in% get.all.track.index())) {
		stop(paste0("Cannot find track: ", track.index, ".\n"))
	}

	current.sector.data = get.sector.data(sector.index)
	current.cell.data = get.cell.data(sector.index, track.index)
	cell.padding = current.cell.data$cell.padding
	
	if(length(name) != 1) {
		stop("``name`` should only have length of 1.\n")
	}
	
	if(name == "xlim") {
		return(current.cell.data$xlim)
	} else if(name == "ylim") {
		return(current.cell.data$ylim)
	} else if(name == "xrange") {
		xlim = current.cell.data$xlim
		return(xlim[2] - xlim[1])
	} else if(name == "yrange") {
		ylim = current.cell.data$ylim
		return(ylim[2] - ylim[1])
	} else if(name == "xcenter") {
		xlim = current.cell.data$xlim
		return((xlim[2] + xlim[1])/2)
	} else if(name == "ycenter") {
		ylim = current.cell.data$ylim
		return((ylim[2] + ylim[1])/2)
	} else if(name == "cell.xlim") {
		return(current.cell.data$cell.xlim)
	} else if(name == "cell.ylim") {
		return(current.cell.data$cell.ylim)
	} else if(name == "sector.numeric.index") {
		return(which(get.all.sector.index() == sector.index))
	} else if(name == "sector.index") {
		return(sector.index)
	} else if(name == "track.index") {
		return(track.index)
	} else if(name == "xplot") {
		x = current.sector.data[c("start.degree", "end.degree")]
		names(x) = NULL
		return(x)
	} else if(name == "yplot") {
		return(c(current.cell.data$track.start - current.cell.data$track.height, current.cell.data$track.start))
	} else if(name == "track.margin") {
		return(current.cell.data$track.margin)
	} else if(name == "cell.padding") {
		return(current.cell.data$cell.padding)
	} else if(name == "cell.start.degree") {
		x = current.sector.data["start.degree"]
		names(x) = NULL
		return(x)
	} else if(name == "cell.end.degree") {
		x = current.sector.data["end.degree"]
		names(x) = NULL
		return(x)
	} else if(name == "cell.bottom.radius") {
		return(current.cell.data$track.start - current.cell.data$track.height)
	} else if(name == "cell.top.radius") {
		return(current.cell.data$track.start)
	} else if(name == "bg.col") {
		return(current.cell.data$bg.col)
	} else if(name == "bg.border") {
		return(current.cell.data$bg.border)
	} else if(name == "bg.lty") {
		return(current.cell.data$bg.lty)
	} else if(name == "bg.lwd") {
		return(current.cell.data$bg.lwd)
	} else if(name == "track.height") {
		return(current.cell.data$track.height)
	} else {
		stop("Wrong cell meta name.\n")
	}
	return(NULL)
}
