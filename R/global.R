
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
	cell.padding = c(0.1, 0.1, 0.1, 0.1),  # percentage
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
# -start.degree            The starting degree from which the circle begin to draw. Note this degree is measured
#     in the standard polar coordinate which means it is reverse-clockwise.
# -gap.degree              Gap between two neighbour sectors.
# -track.margin            Like ``margin`` in Cascading Style Sheets (CSS), it is the blank area
#     out of the plotting region, also outside of the borders. Since left and right margin are controlled
#     by ``gap.degree``, only bottom and top margin need to be set. And all cells in a same track share the same margins, and
#     that's why this parameter is called ``track.margin``. The value for the ``track.margin``
#     is the percentage according to the radius of the unit circle.
# -unit.circle.segments    Since curves are simulated by a series of straight lines,
#     this parameter controls the amout of segments to represent a curve. The minimal length
#     of the line segmentation is the length of the unit circle (``2pi``) / ``unit.circoe.segments``.
# -cell.padding            Padding of the cell. Like ``padding`` in Cascading Style Sheets
#    (CSS), it is the blank area around the plotting regions, but within the borders.
#     The paramter has four values, which controls the bottom, left, top and right padding
#     respectively. The four values are all percentages in which the first and the third padding
#     values are the percentages according to the range of values on y-axis and the second and
#     fourth values are the percentages according to the range of values on x-axis.
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
# the layout of the sectors. The left and right padding for ``cell.padding`` will also be
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
				warning(paste("'", name[i], "' can only be modified before `circos.initialize`.\n", sep = ""))
                next
			}
			if(name[i] == "gap.degree" && (args[[ name[i] ]] < 0 || args[[ name[i] ]] >= 360)) {
				stop("`gap.degree` should be only in [0, 360).\n")
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
# -factors Factors which represent the categories of data
# -x       Data
# -xlim    Limitations for values on x-axis
#
# == details
# The function allocates the sectors according to the values on x-axis.
# The number of sectors are determined by the ``factors`` and the order
# of sectors are determined by the levels of factors. In this function,
# the start and end position  for each sector on the circle (measured by degree)
# are calculated according to the values on x-axis.
#
# If ``x`` is set, the length of ``x`` must be equal to the length of ``factor``.
# Then the data range for each sector are calculated from ``x`` and ``factor``.
#
# If ``xlim`` is set, it should be a vector containing two numbers or a matrix with 2 columns.
# If ``xlim`` is a vector, it means all sector share the same ``xlim``.
# If ``xlim`` is a matrix, the number of rows should be equal to the number of categories (number of levels)
# identified by ``factors``, then each row of ``xlim`` corresponds to the data range for each sector
# and the order of rows is corresponding to the order of levels of ``factors``.
#
# The function finally call `graphics::plot` and be ready to draw.
circos.initialize = function(factors, x = NULL, xlim = NULL) {

    assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".TRACK.END.POSITION", 1, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 1, envir = .CIRCOS.ENV)
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
    min.value = min.value - cell.padding[2]*sector.range  # real min value
    max.value = max.value + cell.padding[4]*sector.range  # real max value
    n.sector = length(le)
    
    sector = vector("list", 5)
	# for each sector, `start.degree always referto `min.value` and `end.degree` always
	# refer to `max.value` in a reverse clockwise fasion. So here `start.degree` and 
	# `end.degree` also correspond to the direction.
	# So in the polar coordinate, `start.degree` would be larger than `end.degree`
    names(sector) = c("factor", "min.value", "max.value", "start.degree", "end.degree")
    sector[["factor"]] = le
    sector[["min.value"]] = min.value
    sector[["max.value"]] = max.value
    
    gap.degree = circos.par("gap.degree")
	start.degree = circos.par("start.degree")
	clock.wise = circos.par("clock.wise")
    
    # degree per data
    unit = (360 - gap.degree*n.sector) / sum(sector.range)
	if(unit <= 0) {
		stop("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.\n")
	}
    for(i in seq_len(n.sector)) {
		
		if(sector.range[i] == 0) {
			stop(paste("Range of the sector (", le[i] ,") cannot be 0.\n", sep = ""))
		}
		
		# only to ensure value are always increasing or decreasing with the absolute degree value
		if(clock.wise) {
			sector[["start.degree"]][i] = -gap.degree + ifelse(i == 1, start.degree, sector[["end.degree"]][i-1])
			sector[["end.degree"]][i] =  sector[["start.degree"]][i] - sector.range[i]*unit
		} else {
			sector[["end.degree"]][i] =  gap.degree + ifelse(i == 1, start.degree, sector[["start.degree"]][i-1])
			sector[["start.degree"]][i] = sector[["end.degree"]][i] + sector.range[i]*unit   
		}
    }
	
	# gap not draw first
	if(clock.wise) {
		sector[["start.degree"]] = sector[["start.degree"]] + gap.degree
		sector[["end.degree"]] = sector[["end.degree"]] + gap.degree
	} else {
		sector[["start.degree"]] = sector[["start.degree"]] - gap.degree
		sector[["end.degree"]] = sector[["end.degree"]] - gap.degree
	}
	
	# from start.degree, degree is increasing in a reverse-clock wise fasion
	# so, if circos is created clock wise, the forward sector would have large degrees
	# if circos is created reverse clock wise, the forward sector would have small degrees
	# just for goodlooking for the degree
	if(clock.wise) {
		sector[["start.degree"]] = sector[["start.degree"]] + 360
		sector[["end.degree"]] = sector[["end.degree"]] + 360
	}
    
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
# Reset the circos layout parameters and internal variables
#
# == details
# The package uses several global variables (internal) to trace the plotting procedure.
# If you want to draw a new figure, you must call this function first to reset
# everything.
circos.clear = function() {
    
	assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".TRACK.END.POSITION", 1, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
	assign(".CIRCOS.PAR", .CIRCOS.PAR.DEFAULT, envir = .CIRCOS.ENV)
    
    return(invisible(NULL))
}

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
# Label the sector index and the track index of each cell
#
# == details
# Draw the index of the sector and the track for each cell on the figure.
# This function can help you to find the coordinates of cells. 
show.index = function() {
	sectors = get.all.sector.index()
	max.track.index = get.max.track.index()
	if(length(sectors) && max.track.index > 0) {
		for(i in seq_along(sectors)) {
			for(j in seq_len(max.track.index)) {
				cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sectors[i], track.index = j)
				cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sectors[i], track.index = j)
				circos.text(mean(cell.xlim), mean(cell.ylim), labels = paste(sectors[i], j, sep = ":"),
				    sector.index = sectors[i], track.index = j, direction = "horizontal")
			}
		}
	}
}

# == title
# Get the meta data for a cell
#
# == param
# -name         Only support one name at a time, see "details" section
# -sector.index index for the sector
# -track.index  index for hte track
#
# == details
# The following meta information for a cell can be obtained:
#
# -sector.index         The name (label) for the sector
# -sector.numeric.index Numeric index for the sector
# -track.index          Numeric index for the track
# -xlim                 Minimal and maximal values on the x-axis
# -ylim                 Minimal and maximal values on the y-axis
# -xrange               Range of ``xlim``
# -yrange               Range of ``ylim``
# -cell.xlim            Minimal and maximal values on the x-axis extended by cell paddings
# -cell.ylim            Minimal and maximal values on the y-axis extended by cell paddings
# -xplot                Right and left edge degree for the plotting region in the circle.
#                       The first element corresponds to the start point of values on x-axis (``cell.xlm[1]``)
#                       and the second element corresponds to the end point of values on x-axis (``cell.xlim[2]``)
#                       Since x-axis in data coordinate in cells are always reverse clockwise, ``xplot[1]`` is larger
#                       than ``xplot[2]``.
# -yplot                Bottom and top value for the plotting region in the canvas. It is the value
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
	current.sector.data = get.sector.data(sector.index)
	current.cell.data = get.cell.data(sector.index, track.index)
	cell.padding = current.cell.data$cell.padding
	
	if(length(name) != 1) {
		stop("``name`` only should have length of 1.\n")
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
	} else {
		stop("Wrong cell meta name.\n")
	}
	return(NULL)
}
