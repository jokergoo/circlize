# this file contains variables and functions related to
# global variables.

.CIRCOS.ENV = new.env()

assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
assign(".TRACK.END.POSITION", 1, envir = .CIRCOS.ENV)
assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
.CIRCOS.PAR.DEFAULT = list(
    start.degree = 0,
	gap.degree = 1,
	track.margin = c(0.01, 0.01),  # top margin and bottom margin, percentage
	unit.circle.segments = 500,   #to simulate smooth curve
	cell.padding = c(0.02, 1, 0.02, 1),  # percentage
	default.track.height = 0.2,
	points.overflow.warning = TRUE,
	canvas.xlim = c(-1, 1),
	canvas.ylim = c(-1, 1),
	clock.wise = TRUE)
assign(".CIRCOS.PAR", .CIRCOS.PAR.DEFAULT, envir = .CIRCOS.ENV)

# == title
# Parameters for circos layout
#
# == param
# -... Arguments for the parameters, see "details" section
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
#     of the line segmentation is the length of the unit circle (``2pi``) / ``unit.circoe.segments``.
#     More segments means better approximation for the curves while larger size if you generate figures as PDF format.
# -cell.padding            Padding of the cell. Like ``padding`` in Cascading Style Sheets
#    (CSS), it is the blank area around the plotting regions, but within the borders.
#     The parameter has four values, which controls the bottom, left, top and right padding
#     respectively. The first and the third padding
#     values are the percentages according to the radius of the unit circle and the second and
#     fourth values are degrees.
# -default.track.height    The default height of tracks. It is the percentage according to the radius
#     of the unit circle. The height includes the top and bottom cell paddings but not the margins.
# -points.overflow.warning Since each cell is in fact not a real plotting region but only
#     an ordinary rectangle, it does not eliminate points that are plotted out of
#     the region. So if some points are out of the plotting region, by default, the 
#     package would continue drawing the points and print warnings. But in some 
#     circumstances, draw something out of the plotting region is useful, such as draw
#     some legend or text. Set this value to ``FALSE`` to turn off the warnings.
# -canvas.xlim              The coordinate for the canvas. Because the package draw everything (or almost everything) inside the unit circle, so
#     the default ``canvas.xlim`` and ``canvas.ylim`` for the canvas would be all ``c(-1, 1)``. However, you can set it to a more broad
#     interval if you want to draw other things out of the circle. By choosing proper
#     ``canvas.xlim`` and ``canvas.ylim``, you can draw part of the circle. E.g. setting
#     ``canvas.xlim`` to ``c(0, 1)`` and ``canvas.ylim`` to ``c(0, 1)`` would only draw
#     circle in the region of (0, pi/2).
# -canvas.ylim              The coordinate for the canvas. By default it is ``c(-1, 1)``
# -clock.wise               The direction of drawing sectors. Default is ``TRUE``.
#
# Similar to `graphics::par`, you can get the values of the parameters by specifying the 
# names of the parameters and you can set the values of the parameters by specifying a
# named list which contains the new values.
#
# ``gap.degree``, ``start.degree``, ``canvas.xlim``, ``canvas.ylim`` and ``clock.wise`` 
# only be set before the initialization of circos layout
# (i.e. before calling `circos.initialize`) because these values will not be changed after
# putting sectors on the circle. The left and right padding for ``cell.padding`` will also be
# effectiveless after the initialization because all cells in a sector would share the same
# left and right paddings. 
circos.par = function (...) {
    args = list(...)
    
	.CIRCOS.PAR = get(".CIRCOS.PAR", envir = .CIRCOS.ENV)
	par.names = names(.CIRCOS.PAR)
	
    if(length(args) == 0) {
        return(.CIRCOS.PAR)
    }
    if(is.null(names(args))) {
        if(length(args) == 1) {
            return(.CIRCOS.PAR[[unlist(args)]])
        } else {
            return(.CIRCOS.PAR[unlist(args)])
        }
    }
    
    name = names(args)
    if(sum(is.null(name)) == 0) {
		o.cell.padding = .CIRCOS.PAR[["cell.padding"]]
        for(i in seq_along(args)) {
			
			if(sum(name[i] %in% par.names) == 0) {
				stop(paste("Wrong element: '", name[i], "' in `circos.par()`", sep = ""))
			}
			
			if(name[i] %in% c("start.degree", "gap.degree", "canvas.xlim", "canvas.ylim", "clock.wise") &&
			   is.circos.initialized()) {
				warning(paste("'", name[i], "' can only be modified before `circos.initialize`,\nor maybe you forgot to call `circos.clear` in your last plot.\n", sep = ""))
                next
			}

			if(name[i] == "gap.degree") {
				gap.degree = args[[ name[i] ]]
				for(k in seq_along(gap.degree)) {
					if(gap.degree[k] >= 360 || gap.degree[k] < 0) {
						stop("`gap.degree` should be only in [0, 360).\n")
					}
				}
			}

            .CIRCOS.PAR[[ name[i] ]] = args[[ name[i] ]]
			
        }
		if(is.circos.initialized()) {
			.CIRCOS.PAR[["cell.padding"]][2] = o.cell.padding[2]
			.CIRCOS.PAR[["cell.padding"]][4] = o.cell.padding[4]
		}
		assign(".CIRCOS.PAR", .CIRCOS.PAR, envir = .CIRCOS.ENV)
        return(invisible(.CIRCOS.PAR))
    }
}

# before initialization, .SECTOR.DATA is NULL
is.circos.initialized = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	return(! is.null(.SECTOR.DATA))
}

# == title
# Initialize the circos sectors
#
# == param
# -factors Factors which represent data categories
# -x       Data on x-axis, a vector
# -xlim    Limitations for values on x-axis
# -sector.width Width for each sector. The length of the vector should be either 1 which means
#          all sectors are having same width or as same as the number of sectors. The value for
#          the vector is the relative value, and they will be scaled by dividing their summation.
#          By defautl, it is ``NULL`` which means the width of sectors correspond to the data
#          range in sectors. If you set the value, you need to notice the width for the sector here
#          includes the gap after it.
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
# the width can reflect the range of data in sectors. Anyway, in some circumstances, it is useful to manually set
# the width such as you want to zoom in some part of the sectors.
#
# The function finally call `graphics::plot` and be ready for adding graphics.
circos.initialize = function(factors, x = NULL, xlim = NULL, sector.width = NULL) {

    assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".TRACK.END.POSITION", 1, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
	
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)

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
    
    sector = vector("list", 5)
	# for each sector, `start.degree always referto `min.value` and `end.degree` always
	# refer to `max.value` in a reverse clockwise fasion. So here `start.degree` and 
	# `end.degree` also correspond to the direction.
	# So in the polar coordinate, `start.degree` would be larger than `end.degree`
    names(sector) = c("factor", "min.value", "max.value", "start.degree", "end.degree")
    sector[["factor"]] = le
    
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
		degree.per.sector = 360 * sector.width.percentage - gap.degree
		
		if(any(degree.per.sector <= 0)) {
			stop("Detect some gaps are too large.\n")
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
		stop("Sumation of cell padding on x-direction are larger than the width of the sectors.\n")
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
	par(lend = "square", ljoin = "mitre")
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
# If you meet some errors when re-drawing the circos plot, try running this function and it will solve part of the problems.
circos.clear = function() {
    
	assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".TRACK.END.POSITION", 1, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
	assign(".CIRCOS.PAR", .CIRCOS.PAR.DEFAULT, envir = .CIRCOS.ENV)
    
    return(invisible(NULL))
}

# == title
# Get index for all sectors
#
# == details
# Simple function returning a vector of all sector index.
get.all.sector.index = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
    return(as.vector(.SECTOR.DATA$factor))
}

get.sector.data = function(sector.index = get.current.sector.index()) {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
    sector.data = as.vector(as.matrix(.SECTOR.DATA[.SECTOR.DATA[[1]] == sector.index, 2:5]))
    names(sector.data) = colnames(.SECTOR.DATA)[2:5]
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

get.max.track.index = function() {
    if(get.current.track.index() == 0) {
        return(0)
    } else {
		.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
        return(length(.CELL.DATA[[1]]))
    }
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

# Position where the current track ends (position of the bottom edge - bottom margin)
# If no track has been created, the position is 1
# Note there would be a little inconsistence for the definition of track.
# In the package, the track is the height of cells
# but in this funciton, track includes the margins. However, it is an internal function
# and the definition of track would be unified
get.track.end.position = function(track.index = get.current.track.index()) {
    
    if(track.index == 0) {
        return(1)
    } else {
		.TRACK.END.POSITION = get(".TRACK.END.POSITION", envir = .CIRCOS.ENV)
        return(.TRACK.END.POSITION[track.index])
    }
}

set.track.end.position = function(track.index = get.current.track.index(), y) {
    .TRACK.END.POSITION = get(".TRACK.END.POSITION", envir = .CIRCOS.ENV)
    .TRACK.END.POSITION[track.index] = y
	assign(".TRACK.END.POSITION", .TRACK.END.POSITION, envir = .CIRCOS.ENV)
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
# -sector.index Which sectors you want to look at
# -track.index  Which tracks you want to look at
# -plot         Whether to add information on the plot
#
# == details
# It tells you the basic parameters for sectors/tracks/cells. If both ``sector.index``
# and ``track.index`` are set to ``NULL``, the function would print index for 
# all sectors and all tracks. If ``sector.index`` and/or ``track.index`` are set,
# the function would print xlim and ylim in the data coordinate for every cell in specified sectors and tracks.
# Also, the function will print index for your current sector and current track.
#
# If ``plot`` is set to ``TRUE``, the function will draw the index of the sector and the track 
# for each cell on the plot.
circos.info = function(sector.index = NULL, track.index = NULL, plot = FALSE) {
	sectors = get.all.sector.index()
	max.track.index = get.max.track.index()
		
	if(plot) {
		for(i in seq_along(sectors)) {
			for(j in seq_len(max.track.index)) {
				cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sectors[i], track.index = j)
				cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sectors[i], track.index = j)
				circos.text(mean(cell.xlim), mean(cell.ylim), labels = paste(sectors[i], j, sep = ":"),
					sector.index = sectors[i], track.index = j, facing = "downward")
			}
		}
	} else {
		# just print the name and xlim for each sector
		if(is.null(sector.index) && is.null(track.index)) {
			all.sector.index = get.all.sector.index()
			max.track.index = get.max.track.index()
			cat("All your sectors:\n")
			print(all.sector.index)
			cat("\n")
			cat("All your tracks:\n")
			print(seq_len(get.max.track.index()))
			cat("\n")

		} else if(is.null(track.index)) {
			track.index = seq_len(get.max.track.index())
			for(i in seq_along(sector.index)) {
				for(j in seq_along(track.index)) {
					cat("sector index: ", sector.index[i], "\n", sep = "")
					cat("track index: ", track.index[j], "\n", sep = "")
					xlim = get.cell.meta.data('xlim', sector.index[i], track.index[j])
					ylim = get.cell.meta.data('ylim', sector.index[i], track.index[j])
					cat("xlim: [", xlim[1], ", ", xlim[2], "]\n", sep = "")
					cat("ylim: [", ylim[1], ", ", ylim[2], "]\n", sep = "")
					cat("\n")
				}
			}
		} else if(is.null(sector.index)) {
			sector.index = get.all.sector.index()
			for(i in seq_along(sector.index)) {
				for(j in seq_along(track.index)) {
					cat("sector index: ", sector.index[i], "\n", sep = "")
					cat("track index: ", track.index[j], "\n", sep = "")
					xlim = get.cell.meta.data('xlim', sector.index[i], track.index[j])
					ylim = get.cell.meta.data('ylim', sector.index[i], track.index[j])
					cat("xlim: [", xlim[1], ", ", xlim[2], "]\n", sep = "")
					cat("ylim: [", ylim[1], ", ", ylim[2], "]\n", sep = "")
					cat("\n")
				}
			}
		} else {
			for(i in seq_along(sector.index)) {
				for(j in seq_along(track.index)) {
					cat("sector index: ", sector.index[i], "\n", sep = "")
					cat("track index: ", track.index[j], "\n", sep = "")
					xlim = get.cell.meta.data('xlim', sector.index[i], track.index[j])
					ylim = get.cell.meta.data('ylim', sector.index[i], track.index[j])
					cat("xlim: [", xlim[1], ", ", xlim[2], "]\n", sep = "")
					cat("ylim: [", ylim[1], ", ", ylim[2], "]\n", sep = "")
					cat("\n")
				}
			}
				
		}

		cat("Your current sector.index is ", get.current.sector.index(), "\n", sep = "")
		cat("Your current track.index is ", get.current.track.index(), "\n", sep = "")
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
# Get the meta data for a cell
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
# -sector.numeric.index Numeric index for the sector. It is the numeric order of levels of ``factors`` in initialization step
# -track.index          Numeric index for the track
# -xlim                 Minimal and maximal values on the x-axis
# -ylim                 Minimal and maximal values on the y-axis
# -xrange               Range of ``xlim``. It equals to ``xlim[2] - xlim[1]`` 
# -yrange               Range of ``ylim``
# -xcenter              Center of x-axis. It equals to ``(xlim[2] + xlim[1])/2`` 
# -ycenter              Center of y-axis
# -cell.xlim            Minimal and maximal values on the x-axis extended by cell paddings
# -cell.ylim            Minimal and maximal values on the y-axis extended by cell paddings
# -xplot                Right and left edge degree for the plotting region which are measured in polar coordinate.
#                       The first element corresponds to the start point of values on x-axis (``cell.xlm[1]``)
#                       and the second element corresponds to the end point of values on x-axis (``cell.xlim[2]``)
#                       Since x-axis in data coordinate in cells are always clockwise, ``xplot[1]`` is larger
#                       than ``xplot[2]``.
# -yplot                Bottom and top value for the plotting region in polar coordinate. It is the value
#                       of radius of arc corresponding to top border or bottom border.
# -cell.start.degree    Same as ``xplot[1]``
# -cell.end.degree      Same as ``xplot[2]``
# -cell.bottom.radius   Same as ``yplot[1]``
# -cell.top.radius      Same as ``yplot[2]``
# -track.margin         Margin for the cell
# -cell.padding         Padding for the cell
#
# The function would be useful when you use ``panel.fun`` in `circos.trackPlotRegion` to
# get detailed information of the current cell.
get.cell.meta.data = function(name, sector.index = get.current.sector.index(), 
                              track.index = get.current.track.index()) {

	if(length(sector.index) != 1) {
		stop("Length of `sector.index` should only be 1.\n")
	}
	if(length(track.index) != 1) {
		stop("Length of `track.index` should only be 1.\n")
	}
	if(!any(sector.index %in% get.all.sector.index())) {
		stop(paste0("Cannot find sector: ", sector.index, ".\n"))
	}
	if(!any(track.index %in% seq_len(get.max.track.index()))) {
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
	} else {
		stop("Wrong cell meta name.\n")
	}
	return(NULL)
}
