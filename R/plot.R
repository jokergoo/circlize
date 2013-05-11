# == title
# Create plotting regions for a whole track
#
# == param
# -factors      Factors which represent the categories of data, if is ``NULL``, 
#               then it is the whole sector index.
# -x            Data on the x-axis
# -y            Data on the y-axis
# -ylim         Range of data on the y-axis
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``
# -track.index  Index for the track which is goning to be updated. Setting it to ``NULL`` means
#               creating the plotting regions in the next newest track.
# -track.height Height of the track. It is the percentage to the radius of the unit circls.
#               If to update a track, this argument is disabled.
# -bg.col       Background color for the plotting regions
# -bg.border    Color for the boder of the plotting regions
# -bg.lty       Line style for the border of the plotting regions
# -bg.lwd       Line width for the border of the plotting regions
# -panel.fun    Panel function to draw figures in each cell, see "details" section
#               and vignette for explaination.
#
# == details
# This function pretends to be high-level plotting function, which means, 
# you must first call this function to create a plotting region, then those
# low-level-style plotting function such as `circos.points`, `circos.lines` can be
# applied.
#
# It has two different usages. First, it can create a complete track which among several
# sectors. Because currently it does not support creating single cell since it would
# make the layout disordered, this is the only way to create the plotting region.
#
# Currently, all the cells that are created in a same track sharing same height, which means,
# there is no cell has longer height than others.
#
# Since limitation for values on x-axis has already been defined by `circos.initialize`, only
# limitation for values on y-axis should be specified in this function. The ``x`` argument is only
# used if you set ``panel.fun``. There are two ways to identify the limitation for values on y-axies either by ``y``
# or ``ylim``. If ``y`` is set, it must has the same length as ``factors`` and the ylim or each cell is calculated
# from y values. Also, the ylim can be specified from ``ylim`` which can be a two-element vector or a matrix which
# has two columns and the number of rows is the same as the length of the levels of the factors.
#
# If there is no enough space for the new track or the new track has overlap with other tracks,
# there will be an error.
#
# ``panel.fun`` provides a convinient way to draw figures in each cell when initializing the 
# track. The self-defined function need two arguments: ``x`` and ``y`` which is the data points
# in the current cell. `circos.trackPlotRegion` creates plotting regions one by one on the track and
# ``panel.fun`` draw graphs in the 'current' cell after the plotting region of a certain cell has been
# created. See vignette for examples of how to use this feature.
#
# If ``factors`` does not cover all sectors which is going to be initialized, the cells in remaining unselected
# sectors would also be created but without drawing anything. The ``ylim`` for these cells
# are the same as that in the latest created cell.
#
# Second, it can update a already-created plotRegion if the index for the track
# is specified. If the index is one bigger than the largest current track index. It in fact
# creates the new track. If updating an existed track, those parameters related to the position
# of the plotting region can not be changed.
circos.trackPlotRegion = function(factors = NULL, x = NULL, y = NULL, ylim = NULL, force.ylim = TRUE,
    track.index = NULL, track.height = circos.par("default.track.height"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    panel.fun = function(x, y) {NULL}) {
    
	if(is.null(factors)) {
		factors = get.all.sector.index()
	}
	
    # basic check here
    # if ``ylim`` set then do not need ``y``
    if(!is.null(y) && length(y) != length(factors) ||
	   !is.null(x) && length(x) != length(factors)) {
        stop("Length of data and length of factors differ.\n")
    }
        
    if(!is.factor(factors)) {
        factors = factor(factors)
    }
        
    if(length(setdiff(levels(factors), get.all.sector.index()))) {
        stop("Factors name should be all in existed sector index.\n")
    }
    
    if(is.null(track.index)) {
        # new track should inside the most recently created track
        last.track.index = get.max.track.index()
        set.current.track.index(last.track.index + 1)
        track.index = get.current.track.index()
    } else {
		if(track.index > get.max.track.index() + 1) {
			stop("Wrong track index.\n")
		}
		if(track.index <= get.max.track.index()) {
			if(! is.null(track.height)) {
				warning("You are updating an existed track, the `track.height` is not used.\n")
			}
			track.height = get.cell.data(factors[1], track.index)$track.height
		}
        set.current.track.index(track.index)
    }
        
   
    le = levels(factor(factors))
	nlevel = length(le)
    bg.col = recycle.with.levels(bg.col, le)
    bg.border = recycle.with.levels(bg.border, le)
    bg.lty = recycle.with.levels(bg.lty, le)
    bg.lwd = recycle.with.levels(bg.lwd, le)
	
     # whether force ylim for all cells in a track same
    if(is.null(ylim)) {
		if(force.ylim) {
			y.range = range(y)
			y.range = matrix(rep(y.range, nlevel), ncol = 2, byrow = TRUE)
		} else {
			y.range = tapply(y, factors, range)
			y.range = matrix(unlist(y.range), ncol = 2, byrow = TRUE)
		}
	}
	
    track.start = get.track.end.position(track.index - 1) - circos.par("track.margin")[1]
        
    # check whether there is enough space for the new track and whether the new space
    # overlap with other tracks
    check.track.position(track.index, track.start, track.height)
        
    if(!is.null(ylim)) {
		if(is.vector(ylim) && length(ylim) == 2) {
			ylim = matrix(rep(ylim, length(le)), ncol = 2, byrow = TRUE)
		} else if(is.matrix(ylim) && ncol(ylim) == 2 && nrow(ylim) == length(le)) {
		
		} else {
			stop("Wrong `ylim`.\n")
		}
    } 
        
    # now for each factor
    for(i in seq_along(le)) {
            
        sector.data = get.sector.data(le[i])
        xlim = c(sector.data["start.value"], sector.data["end.value"])
		names(xlim) = NULL
		
		cell.padding = circos.par("cell.padding")
		yl = numeric(2)
        if(is.null(ylim)) {
            yl[1] = y.range[i, 1] - (y.range[i, 2] - y.range[i, 1])*cell.padding[1]
            yl[2] = y.range[i, 2] + (y.range[i, 2] - y.range[i, 1])*cell.padding[3]
        } else {
			yl[1] = ylim[i, 1] - (ylim[i, 2] - ylim[i, 1])*cell.padding[1]
            yl[2] = ylim[i, 2] + (ylim[i, 2] - ylim[i, 1])*cell.padding[3]
		}
        
        # create plotting region for single cell
        circos.createPlotRegion(track.start = track.start,
                              track.height = track.height, sector.index = le[i],
                              track.index = track.index,
                              xlim = xlim, ylim = yl, bg.col = bg.col[i],
                              bg.border = bg.border[i], bg.lty = bg.lty[i], bg.lwd = bg.lwd[i])
		
		l = factors == le[i]
        if(! is.null(panel.fun)) {
            if(is.null(x)) {
                nx = NULL
            } else {
                nx = x[l]
            }
                
            if(is.null(y)) {
                ny = NULL
            } else {
                ny = y[l]
            }
                
            panel.fun(nx, ny)
        }
            
    }
	
	# and those sectors not include in factors
    le2 = setdiff(get.all.sector.index(), levels(factors))
	if(length(le2)) {
		for(i in seq_along(le2)) {
            
			sector.data = get.sector.data(le2[i])
			xlim = c(sector.data["start.value"], sector.data["end.value"])
			names(xlim) = NULL
			
			# ylim is the most recent ``yl``
			circos.createPlotRegion(track.start = track.start,
								  track.height = track.height, sector.index = le2[i],
								  track.index = track.index,
								  xlim = xlim, ylim = yl, bg.col = "white",
								  bg.border = "white")
		}
	}
	
    # After the track has been created, the default tract starting position is set
    # just next to the most recently created track
    set.track.end.position(track.index, track.start - track.height - circos.par("track.margin")[2])
    return(invisible(NULL))

}

# == title
# Update the plotting region in an existed cell
#
# == param
# -sector.index Index for the sector
# -track.index  Index for the track
# -bg.col       Background color for the plotting region
# -bg.border    Color for the boder of the plotting region
# -bg.lty       Line style for the border of the plotting region
# -bg.lwd       Line width for the border of the plotting region
#
# == details
# You can update an existed cell by this function by erasing the contents in the plotting regions
circos.updatePlotRegion = function(sector.index = get.current.sector.index(), track.index = get.current.track.index(),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd")) {
    
    if(!has.cell(sector.index, track.index)) {
        stop("You can only update an existed cell.\n")
    }
    
    cell.data = get.cell.data(sector.index, track.index)
    xlim = cell.data$xlim
    ylim = cell.data$ylim
    
    set.current.sector.index(sector.index)
    set.current.track.index(track.index)
    
    # cover the exsited region by fill with white
    circos.rect(xlim[1], ylim[1],
        xlim[2], ylim[2], 
        sector.index = sector.index, track.index = track.index,
        col = "white", border = "white",
        lty = 1, lwd = 1)
    circos.rect(xlim[1], ylim[1],
        xlim[2], ylim[2], 
        sector.index = sector.index, track.index = track.index,
        col = bg.col, border = bg.border,
        lty = bg.lty, lwd = bg.lwd)
    return(invisible(NULL))
}

# internal
circos.createPlotRegion = function(track.start, track.height = circos.par("default.track.height"),
    sector.index = get.current.sector.index(), track.index = get.current.track.index(), xlim, ylim,
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd")) {
    
    # when creating the plotting region for a single cell, ``track.start``, ``track.height``,
    # ``xlim`` and ``ylim`` are already set and because it's done internally, so there is no
    # need to check these arguments.
    set.cell.data(sector.index = sector.index,
        track.index = track.index,
        xlim = xlim,
        ylim = ylim,
        track.start = track.start,
        track.height = track.height)
    
    set.current.sector.index(sector.index)
    
    # The plotting region is a rectangle
    circos.rect(xlim[1], ylim[1], sector.index = sector.index, track.index = track.index,
        xlim[2], ylim[2], col = bg.col, border = bg.border,
        lty = bg.lty, lwd = bg.lwd)
    return(invisible(NULL))
}

# == title 
# Add points to a plotting region
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -pch          Points type
# -col          Points color
# -cex          Points size
#
# == details
# This function can only add points in a specified cell. Pretending a low-level plotting 
# function, it can only be applied in plottting region which has been created.
#
# You can think the function as the normal `graphics::points`
# function, just adding points in the plotting region. The position of
# plotting region is identified by ``sector.index`` and ``track.index``, if they are not
# specified, they are in current sector and current track.
#
# Data points out of the plotting region will be drawed, but with a warning message.
#
# Other graphics parameters which are available in the function are ``pch``, ``col``
# and ``cex`` which have same meaning as those in the `graphics::par`.
circos.points = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
                         pch = par("pch"), col = par("col"), cex = par("cex")) {
    
    if(!has.cell(sector.index, track.index)) {
        stop("'circos.points' can only be used after the plotting region been created\n")
    }

    # whether the points that are out of the plotting region.
    # If there is, throw warnings.
    check.points.position(x, y, sector.index, track.index)
    
    d = circlize(x, y, sector.index, track.index)
    points(polar2Cartesian(d), pch = pch, col = col, cex = cex)
    return(invisible(NULL))
}

# == title 
# Add points to the plotting regions in one track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -track.index  Index for the track
# -pch          Points type
# -col          Points color
# -cex          Points size
#
# The function adds points in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add points in cells corresponding
# to the part of data by calling `circos.points`.
#
# Length of ``pch``, ``col`` and ``cex`` can be one, length of levels of the factors and length of 
# factors. All length will be recycled to the length of factors respectively.
circos.trackPoints = function(factors = NULL, x, y, track.index = get.current.track.index(),
                         pch = par("pch"), col = par("col"), cex = par("cex")) {
    
    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop("Length of data and length of factors differ.\n")
    }
        
    if(!is.factor(factors)) {
        factors = factor(factors)
    }
    
    if(length(setdiff(levels(factors), get.all.sector.index()))) {
        stop("Factors name should be all in existed sector index.\n")
    }
        
    le = levels(factors)
    
    # set these graphic parameters with same length as the factors
    pch = recycle.with.factors(pch, factors)
    col = recycle.with.factors(col, factors)
    cex = recycle.with.factors(cex, factors)
    
    for(i in seq_along(le)) {
        l = factors == le[i]
        
        nx = x[l]
        ny = y[l]
        npch = pch[l]
        ncol = col[l]
        ncex = cex[l]
        circos.points(nx, ny, sector.index = le[i],
                      track.index = track.index,
                      pch = npch, col = ncol, cex = ncex)
            
    }
    return(invisible(NULL))
}

# == title 
# Add lines to the plotting region
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -col          Line color
# -lwd          line width
# -lty          line style
# -type         line type, similar as ``type`` argument in `graphics::lines`, but only in ``c("l", "o", "h", "s")``
# -straight     whether draw straight lines between points
# -area         whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls the color of the line.
# -border       color for border of the area
# -pt.col       if ``type`` is "o", points color
# -cex          if ``type`` is "o", points size
# -pch          if ``type`` is "o", points type
#
# ==details
# Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circos layout.
# But if you do not want to do such transformation you can use this function just drawing straight
# lines between points by setting ``straight`` to ``TRUE``.
#
# Draw areas below lines can help to identify the direction of y-axis in cells. This can be fullfilled by specifying
# ``area`` to ``TURE``.
circos.lines = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
    col = ifelse(area, "grey", "black"), lwd = par("lwd"), lty = par("lty"), type = "l", straight = FALSE,
	area = FALSE, border = "black",
    pt.col = "black", cex = par("cex"), pch = par("pch")) {
    
    if(type == "l") {
        
    } else if(type == "o") {
        circos.points(x, y, sector.index = sector.index, track.index = track.index,
                      col = pt.col, cex = cex, pch = pch)
        circos.lines(x, y, sector.index = sector.index, track.index = track.index,
                     col = col, lwd = lwd, lty = lty, area = area, border = border)
        return(invisible(NULL))
    } else if(type == "h") {
        ylim = get.cell.meta.data("ylim", sector.index, track.index)
        for(i in seq_along(x)) {
            circos.lines(c(x[i], x[i]), c(ylim[1], y[i]),
                         sector.index = sector.index, track.index = track.index, 
                         col = col, lwd = lwd, lty = lty, straight = TRUE)    
        }
        return(invisible(NULL))
    } else if(type == "s") {
		d = matrix(nrow = 0, ncol = 2)
        for(i in seq_along(x)) {
            if(i == 1) {
                next
            }
			d = rbind(d, lines.expand(c(x[i-1], x[i]), c(y[i-1], y[i-1]), sector.index, track.index))
			d = rbind(d, cbind(c(x[i], x[i]), c(y[i-1], y[i])))
        }
		
		if(area) {
			ylim = get.cell.meta.data("ylim", sector.index, track.index)
			d = rbind(d, c(d[nrow(d), 1], ylim[1]))
			d = rbind(d, c(d[1, 1], ylim[1]))
			circos.polygon(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index, 
				   col = col, border = border, lwd = lwd, lty = lty)
		} else {
			circos.lines(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index, 
							 col = col, lwd = lwd, lty = lty)
        }
		return(invisible(NULL))
    }
    
    if(!has.cell(sector.index, track.index)) {
        stop("'circos.lines' can only be used after the plotting region been created\n")
    }
    
    # whether the points that are out of the plotting region.
    check.points.position(x, y, sector.index, track.index)
    
    if(straight) {
        d = cbind(x, y)
    } else {
        d = lines.expand(x, y, sector.index, track.index)
    }
	
	if(area) {
		ylim = get.cell.meta.data("ylim", sector.index, track.index)
		d = rbind(d, c(d[nrow(d), 1], ylim[1]))
		d = rbind(d, c(d[1, 1], ylim[1]))
		circos.polygon(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index, 
		       col = col, border = border, lwd = lwd, lty = lty)
		return(invisible(NULL))
	}
    
    d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
	
    lines(polar2Cartesian(d2), col = col, lwd = lwd, lty = lty)
    return(invisible(NULL))
}

# == title 
# Add lines to the plotting regions in one track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -track.index  Index for the track
# -col          Line color
# -lwd          line width
# -lty          line style
# -type         line type, similar as ``type`` argument in `graphics::lines`, but only in ``c("l", "o", "h", "s")``
# -straight     whether draw straight lines between points
# -area         whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls the color of the line.
# -border       color for border of the area
# -pt.col       if ``type`` is "o", points color
# -cex          if ``type`` is "o", points size
# -pch          if ``type`` is "o", points type
#
# == details
# The function adds lines in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add lines in cells corresponding
# to the part of data by calling `circos.lines`.
circos.trackLines = function(factors, x, y, track.index = get.current.track.index(),
    col = "black", lwd = par("lwd"), lty = par("lty"), type = "l", straight = FALSE,
	area = FALSE, border = "black",
    pt.col = "black", cex = par("cex"), pch = par("pch")) {
    
    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop("Length of data and length of factors differ.\n")
    }
        
    if(!is.factor(factors)) {
        factors = factor(factors)
    }
    
    if(length(setdiff(levels(factors), get.all.sector.index()))) {
        stop("Factors name should be all in existed sector index.\n")
    }
        
    le = levels(factors)
    
    # set these graphic parameters with same length as the factors
    col = recycle.with.factors(col, factors)
    lwd = recycle.with.factors(lwd, factors)
    lty = recycle.with.factors(lty, factors)
    pt.col = recycle.with.factors(pt.col, factors)
    cex = recycle.with.factors(cex, factors)
    pch = recycle.with.factors(pch, factors)
	
	area = recycle.with.levels(area, le)
	border = recycle.with.levels(border, le)
    
    for(i in seq_along(le)) {
        l = factors == le[i]
        nx = x[l]
        ny = y[l]
        ncol = col[l]
        nlwd = lwd[l]
        nlty = lty[l]
        npt.col = pt.col[l]
        ncex = cex[l]
        npch = pch[l]
        circos.lines(nx, ny, sector.index = le[i],
                      track.index = track.index,
                      col = ncol, lwd = nlwd, lty = nlty, area = area[i], border = border[i],
                      pt.col = npt.col, cex = ncex, pch = npch, type = type, straight = straight)
            
    }
    return(invisible(NULL))
}


# == title
# Draw rectangle-like grid
#
# == param
# -xleft        x for the left bottom points
# -ybottom      y for the left bottom points
# -xright       x for the right top points
# -ytop         y for the right top points
# -sector.index Index for the sector
# -track.index  Index for the track
# -col          filled color
# -border       color for the border
# -lty          line style for the border
# -lwd          line width for the border
#
# == details
# Currently, ``xleft``, ``ybottom``, ``xright``, ``ytop`` are all single values, which means
# you can only draw one rectangle at once. Well, the name for this function is `circos.rect`
# because if you imagin the plotting region as Cartesian coordinate, then it is rectangle.
# in the polar coordinate, the up and bottom edge become two arcs.
#
# You just need to specify the coordinates of two diagonal points just similar as 
# `graphics::rect` does.
circos.rect = function(xleft, ybottom, xright, ytop, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
                       col = NA, border = "black", lty = par("lty"), lwd = par("lwd")) {
    if(! (length(xleft) == 1 &&
          length(ybottom) == 1 &&
          length(xright) == 1 &&
          length(ytop) == 1) ) {
        stop("There should only be one data points in 'xleft', 'ybottom', 'xright' or 'ytop'\n")  
    }

    if(!has.cell(sector.index, track.index)) {
        stop("'circos.rect' can only be used after the plotting region been created\n")
    }
    
    # no filled colors, just four edges, here edges colors are controled by ``border``
    if(is.na(col)) {
        # vertical lines in the original coordinate system are still straight lines
        # in the new coordinate system except they now pointing to the circle center.
        circos.lines(c(xleft, xleft), c(ybottom, ytop),
                     sector.index = sector.index, track.index = track.index,
                     col = border, lty = lty, lwd = lwd, straight = TRUE)
        # horizontal lines in the original coordinate system are now arcs and the arcs
        # share the same circle center as the polar coordinate system
        circos.lines(c(xleft, xright), c(ytop, ytop),
                   sector.index = sector.index, track.index = track.index,
                   col = border, lty = lty, lwd = lwd)
        circos.lines(c(xright, xright), c(ytop, ybottom),
                     sector.index = sector.index, track.index = track.index,
                     col = border, lty = lty, lwd = lwd, straight = TRUE)
        circos.lines(c(xleft, xright), c(ybottom, ybottom),
                   sector.index = sector.index, track.index = track.index,
                   col = border, lty = lty, lwd = lwd)
    } else {
        circos.polygon(c(xleft, xleft, xright, xright, xleft),
                       c(ybottom, ytop, ytop, ybottom, ybottom),
                       sector.index = sector.index, track.index = track.index,
                       col = col, border = border, lty = lty, lwd = lwd)
    }
    return(invisible(NULL))
}

# == title
# Draw polygon
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -col          filled color
# -border       color for the border
# -lty          line style for the border
# -lwd          line width for the border
#
# == details
# similar as `graphics::polygon`
circos.polygon = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd")) {
    
    if(!has.cell(sector.index, track.index)) {
        stop("'circos.polygon' can only be used after the plotting region been created\n")
    }
    
    # whether the points that are out of the plotting region.
    check.points.position(x, y, sector.index, track.index)
    
    d = lines.expand(x, y, sector.index, track.index)
    d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
    polygon(polar2Cartesian(d2), col = col, border = border,
            lty = lty, lwd = lwd)
    return(invisible(NULL))
}

# == title
# Draw text in a cell
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -labels       Labels for each points
# -sector.index Index for the sector
# -track.index  Index for the track
# -direction    Direction of the text, should be one of ``c("default", "vertical_left", "vertical_right", "horizontal", "arc")``.
#               How to choose text direction can be found in the vignette.
# -adj          Adjustment for texts
# -cex          Font size
# -col          Font color
# -font         Font style
#
# == details
# The function is similar to `graphics::text`.
circos.text = function(x, y, labels, sector.index = get.current.sector.index(), track.index = get.current.track.index(), 
    direction = c("default", "vertical_left", "vertical_right", "horizontal", "arc"),
    adj = par("adj"), cex = 1, col = "black", font = par("font")) {
    
    if(!has.cell(sector.index, track.index)) {
        stop("'circos.text' can only be used after the plotting region been created\n")
    }
	
	if(length(cex) == 1) {
		cex = rep(cex, length(x))
	}
	if(length(col) == 1) {
		col = rep(col, length(x))
	}
	if(length(font) == 1) {
		font = rep(font, length(x))
	}
	if(length(adj) == 1) {
		adj = c(adj, adj)
	}
	
    # whether the points that are out of the plotting region.
    check.points.position(x, y, sector.index, track.index)
    
    d = circlize(x, y, sector.index, track.index)
    
    direction = direction[1]
    if(! direction %in% c("default", "vertical_left", "vertical_right", "horizontal", "arc")) {
        stop("direction can only be choosen from 'default', 'vertical_left', 'vertical_right', 'horizontal' and 'arc'\n.")
    }
	
	if(direction == "arc") {
		
		chars = strsplit(labels, "")
		nlabel = length(labels)
		strw = lapply(chars, strwidth, cex = cex, font = font)
		strh = lapply(chars, strheight, cex = cex, font = font)
		
		alpha.offset = sapply(strw, function(x) sum(x))*adj[1]/d[, 2] * 180/pi
		rou.offset = sapply(strh, function(x) -x[1]*adj[2])

		for(i in seq_along(labels)) {
			# degree of the bottom center of each char
			theta = numeric(length(strw[[i]]))
			alpha = d[i, 1] + alpha.offset[i]
			rou = d[i, 2] + rou.offset[i]
			
			for(j in  seq_along(strw[[i]])) {
				theta[j] = alpha - asin(strw[[i]][j]/2/d[i, 2])*180/pi
				alpha = alpha - asin(strw[[i]][j]/d[i, 2])*180/pi
			}
			dr = reverse.circlize(theta, rep(rou, length(theta)), sector.index, track.index)
			circos.text(dr[, 1], dr[, 2], labels = chars[[i]], cex = cex[i], col = col[i], font = font[i], adj = c(0.5, 0))
			#circos.points(dr[, 1], dr[, 2], pch = 16, cex = 0.8)
		}
		
	} else {
        
        srt = d[,1]-90    #srt = ifelse(srt > 0, srt, 360 + srt)
        
        if(direction == "vertical_left") {           # pointing to the circle center, but facing left at 90 degree
            srt = srt - 90
        } else if(direction == "vertical_right") {   # pointing to the circle center, but facing right at 90 degree
            srt = srt + 90
        } else if(direction == "horizontal") {       # horizontal at the finnal graph
            srt = rep(0, length(srt))
        }
    
		m = polar2Cartesian(d)
		
		for(i in seq_along(x)) {
			text(m[i, 1], m[i, 2], labels = labels[i], srt = srt[i],
				 cex = cex[i], col = col[i], font = font[i], adj = adj)
		}
    }
	
    return(invisible(NULL))
}

# == title
# Draw text in cells among the whole track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -labels       Labels
# -track.index  Index for the track
# -direction    Text directions
# -adj          Adjustment for texts
# -cex          Font size
# -col          Font color
# -font         Font style
#
# == details
# The function adds texts in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add texts in cells corresponding
# to the part of data by calling `circos.text`.
circos.trackText = function(factors, x, y, labels, track.index = get.current.track.index(),
                       direction = c("default", "vertical_left", "vertical_right", "horizontal"),
                       adj = par("adj"), cex = 1, col = "black", font = par("font")) {
    
    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop("Length of data and length of factors differ.\n")
    }
        
    if(!is.factor(factors)) {
        factors = factor(factors)
    }
    
    if(length(setdiff(levels(factors), get.all.sector.index()))) {
        stop("Factors name should be all in existed sector index.\n")
    }
        
    le = levels(factors)
    
    # set these graphic parameters with same length as the factors
    # ``direction`` and ``adj`` are not recycled
    cex = recycle.with.factors(cex, factors)
    col = recycle.with.factors(col, factors)
    font = recycle.with.factors(font, factors)

    for(i in seq_along(le)) {
        l = factors == le[i]
        nx = x[l]
        ny = y[l]
        nlabels = labels[l]
        ncex = cex[l]
        ncol = col[l]
        nfont = font[l]
        circos.text(nx, ny, sector.index = le[i],
                      track.index = track.index, labels = nlabels,
                      direction = direction, adj = adj,
                      cex = ncex, col = ncol, font = nfont)
            
    }
    return(invisible(NULL))
}

# == title
# Draw x-axis
#
# == param
# -h                position of the x-axis, can be "top", "bottom" or a numeric value
# -major.at         If it is numeric vector, it identifies the poisitions
#                   of the major ticks. It can exceed the xlim value and the exceeding part
#                   would be trimmed automatically. If it is ``NULL``, it would be calculated by `base::pretty`.
# -labels           labels of the major ticks. Also, the exceeding part would be trimmed automatically.
# -major.tick       Whether to draw major tick. If it is set to ``FALSE``, there would be
#                   no minor ticks either. 
# -sector.index     Index for the sector
# -track.index      Index for the track
# -labels.font      font style for the axis labels
# -labels.cex       font size for the axis labels
# -labels.direction font direction for the axis labels
# -direction        whether the axis ticks point to the outside or inside of the circle.
# -minor.ticks      Number of minor ticks between two close major ticks.
# -major.tick.percentage Length of the major ticks. It is the percentage to the ylim in the cell.
# -labels.away.percentage The distance for the axis labels to the major ticks. It is the percentage to the ylim in the cell.
# -lwd              line width for ticks
#
# == details
# It can only draw axis on x-direction.
circos.axis = function(h = "top", major.at = NULL, labels = TRUE, major.tick = TRUE,
	sector.index = get.current.sector.index(), track.index = get.current.track.index(),
	labels.font = par("font"), labels.cex = par("cex"), labels.direction = "default",
	direction = c("outside", "inside"), minor.ticks = 4,
	major.tick.percentage = 0.1, labels.away.percentage = 0.05, lwd = par("lwd")) {
	
	direction = direction[1]
	if(! direction %in% c("outside", "inside")) {
		stop("direction should be in 'outside' and 'inside'.\n")
	}
	
	xlim = get.cell.meta.data("xlim", sector.index, track.index)
	
	cell.data = get.cell.data(sector.index, track.index)
	sector.data = get.sector.data(sector.index)
	
	if(h == "top") {
		h = cell.data$ylim[2]
	} else if(h == "bottom") {
		h = cell.data$ylim[1]
	}
	
	if(is.null(major.at)) {
		n = floor(abs(sector.data["end.degree"] - sector.data["start.degree"])*(1-circos.par("cell.padding")[2]-circos.par("cell.padding")[4]) / 5)
		major.at = pretty(xlim, n = n)
	}
	
	minor.at = NULL
	if(minor.ticks != 0) {
		for(i in seq_along(major.at)) {
			if(i == 1) next
			k = seq_len(minor.ticks) / (minor.ticks + 1)
			minor.at = c(minor.at, k * (major.at[i] - major.at[i - 1]) + major.at[i - 1])
		}
	}
	
	xlim2 = cell.data$xlim
	circos.lines(c(ifelse(major.at[1] >= xlim2[1], major.at[1], xlim2[1]),
	               ifelse(major.at[length(major.at)] <= xlim2[2], major.at[length(major.at)], xlim2[2])), 
				 c(h, h), sector.index = sector.index, track.index = track.index, lwd = lwd)
	
	# ticks
	yrange = get.cell.meta.data("yrange", sector.index, track.index)
	major.tick.length = yrange * major.tick.percentage
	for(i in seq_along(major.at)) {
		
		if(major.at[i] < xlim2[1] || major.at[i] > xlim2[2]) {
			next
		}
	
		if(major.tick) {
			circos.lines(c(major.at[i], major.at[i]), c(h, h + major.tick.length*ifelse(direction == "outside", 1, -1)), straight = TRUE,
			             sector.index = sector.index, track.index = track.index, lwd = lwd)
		}
		
		labels.adj = NULL
		if(direction == "outside") {
			if(labels.direction == "default") {
				labels.adj = c(0.5, 0)
			} else if(labels.direction == "vertical_left") {
				labels.adj = c(1, 0.5)
			} else if(labels.direction == "vertical_right") {
				labels.adj = c(0, 0.5)
			} else {
				labels.adj = c(0.5, 0)
			}
		} else {
			if(labels.direction == "default") {
				labels.adj = c(0.5, 1)
			} else if(labels.direction == "vertical_left") {
				labels.adj = c(0, 0.5)
			} else if(labels.direction == "vertical_right") {
				labels.adj = c(1, 0.5)
			} else {
				labels.adj = c(0.5, 1)
			}
		}
		
		if(is.logical(labels) && labels) {
			circos.text(major.at[i], h + (major.tick.length+yrange*labels.away.percentage)*ifelse(direction == "outside", 1, -1),
			            labels = major.at[i], adj = labels.adj,
						font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
						direction = labels.direction)
		} else if(length(labels)) {
			circos.text(major.at[i], h + (major.tick.length+yrange*labels.away.percentage)*ifelse(direction == "outside", 1, -1),
			            labels = labels[i], adj = labels.adj,
						font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
						direction = labels.direction)
		}
	}
	if(major.tick) {
		for(i in seq_along(minor.at)) {
			if(minor.at[i] < xlim2[1] || minor.at[i] > xlim2[2]) {
				next
			}
		
			circos.lines(c(minor.at[i], minor.at[i]), c(h, h + major.tick.length/2*ifelse(direction == "outside", 1, -1)), straight = TRUE,
			             sector.index = sector.index, track.index = track.index, lwd = lwd)
		}
	}
}

# == title
# Draw links between two points or sections
#
# == param
# -sector.index1 Sector index for one sector
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt.
# -sector.index2 Sector index for the other sector
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt.
# -rou           The position of the 'root' of the link. It is the percentage of the radius of the unit circle.
#                It would be calculated automatically.
# -top.ratio     The height of the quadratic curve
# -col           Color of the link. If the link is a belt, then it is the filled color for the belt.
# -lwd           Line width
# -lty           Line style
# -border        If the link is a belt, then it is the color for the belt border.
#
# == details
# The link is in fact a quadratic curve.
#
# Drawing links does not create any track.
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# link would look nice. However you can also set teh position and the height of the belts by specifying
# ``rou`` and ``top.ratio``. See vignette for explaination.
circos.link = function(sector.index1,
                       point1,
                       sector.index2,
                       point2,
                       rou = get.track.end.position(get.current.track.index()),
                       top.ratio = 0.5,
                       col = "black", lwd = par("lwd"), lty = par("lty"), border = NA) {
    
    sector.data1 = get.sector.data(sector.index1)
    sector.data2 = get.sector.data(sector.index2)
    
    if(length(point1) == 1 && length(point2) == 1) {
        theta1 = sector.data1["end.degree"] - (point1 - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
                 (sector.data1["end.degree"] - sector.data1["start.degree"])
        
        theta2 = sector.data2["end.degree"] - (point2 - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
                 (sector.data2["end.degree"] - sector.data2["start.degree"])
        
        d = rotate.parabola(theta1 = theta1, theta2 = theta2, rou1 = rou, rou.ratio = top.ratio)
        lines(d, col = col, lwd = lwd, lty = lty)
    } else {
        if(length(point1) == 1) {
			current.cell.xrange = get.cell.meta.data("xrange", sector.index1)
            point1 = c(point1, point1 + current.cell.xrange/100)   
        }
        if(length(point2) == 1) {
			current.cell.xrange = get.cell.meta.data("xrange", sector.index2)
            point2 = c(point2, point2 + current.cell.xrange/100)  
        }
        
        theta11 = sector.data1["end.degree"] - (point1[1] - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
            (sector.data1["end.degree"] - sector.data1["start.degree"])
        theta12 =sector.data1["end.degree"] - (point1[2] - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
            (sector.data1["end.degree"] - sector.data1["start.degree"])
        
        theta21 = sector.data2["end.degree"] - (point2[1] - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
            (sector.data2["end.degree"] - sector.data2["start.degree"])
        theta22 = sector.data2["end.degree"] - (point2[2] - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
            (sector.data2["end.degree"] - sector.data2["start.degree"])
        
        # line from theta11, theta21 and line from theta12, theta22
        # uint circle
        k1 = (sin(theta11/180*pi) - sin(theta21/180*pi))/(cos(theta11/180*pi) - cos(theta21/180*pi))
        b1 = sin(theta11/180*pi) - k1*cos(theta11/180*pi)
        k2 = (sin(theta12/180*pi) - sin(theta22/180*pi))/(cos(theta12/180*pi) - cos(theta22/180*pi))
        b2 = sin(theta12/180*pi) - k2*cos(theta12/180*pi)
        
        if(k1 != k2) {
            # cross of the two lines
            cross.x = -(b1 - b2)/(k1 - k2)
            cross.y = (k1*b2 - k2*b1)/(k1 - k2)
            r = sqrt(cross.x^2 + cross.y^2)
            # cross in the circle, swap theta21 and theta22
            if(r < 1) {
                t = theta21
                theta21 = theta22
                theta22 = t
            }
        }
        
        d1 = rotate.parabola(theta1 = theta11, theta2 = theta21, rou1 = rou, rou.ratio = top.ratio)
        d2 = rotate.parabola(theta1 = theta12, theta2 = theta22, rou1 = rou, rou.ratio = top.ratio)

        if(is.points.ordered.on.circle(c(theta11, theta21, theta22, theta12))) {
            d2 = d2[rev(seq_len(nrow(d2))), ]
            r1 = arc.points(theta21, theta22, rou)
            r2 = arc.points(theta12, theta11, rou)
        } else if(is.points.ordered.on.circle(c(theta11, theta21, theta22, theta12), clock.wise = TRUE)) {
            d2 = d2[rev(seq_len(nrow(d2))), ]
            r1 = arc.points(theta21, theta22, rou, clock.wise = TRUE)
            r2 = arc.points(theta12, theta11, rou, clock.wise = TRUE)
        } else if(is.points.ordered.on.circle(c(theta21, theta11, theta12, theta22))) {
            d2 = d2[rev(seq_len(nrow(d2))), ]
            r1 = arc.points(theta21, theta22, rou, clock.wise = TRUE)
            r2 = arc.points(theta12, theta11, rou ,clock.wise = TRUE)
        } else if(is.points.ordered.on.circle(c(theta21, theta11, theta12, theta22), clock.wise = TRUE)) {
            d2 = d2[rev(seq_len(nrow(d2))), ]
            r1 = arc.points(theta21, theta22, rou)
            r2 = arc.points(theta12, theta11, rou)
        } else if(is.points.ordered.on.circle(c(theta11, theta12, theta21, theta22))) {
            r1 = arc.points(theta12, theta21, rou)
            r2 = arc.points(theta22, theta11, rou)
        } else if(is.points.ordered.on.circle(c(theta11, theta12, theta21, theta22), clock.wise = TRUE)) {
            r1 = arc.points(theta12, theta21, rou, clock.wise = TRUE)
            r2 = arc.points(theta22, theta11, rou, clock.wise = TRUE)
        }
        
        d = rbind(d1, r1)
        d = rbind(d, d2)
        d = rbind(d, r2)
        polygon(d, col = col, lty = lty, lwd = lwd, border = border)
    }
	
	# link is the last track in the current version
	#set.track.end.position(0)
    return(invisible(NULL))
}


#####################################################################
#
# simulate high-level graphic functions such as barplot, hist, boxplot ...
#
#####################################################################

# == title
# Draw histogram in cells among a whole track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data on the x-axis
# -track.index  Index for the track which is goning to be updated. Setting it to ``NULL`` means
#               creating the plotting regions in the next newest track.
# -track.height Height of the track. It is the percentage to the radius of the unit circls.
#               If to update a track, this argument is disabled.
# -ylim         Range of data on the y-axis
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``
# -col          Filled color for histogram
# -border       Border color for histogram
# -lty          Line style for histogram
# -lwd          Line width for histogram
# -bg.col       Background color for the plotting regions
# -bg.border    Color for the boder of the plotting regions
# -bg.lty       Line style for the border of the plotting regions
# -bg.lwd       Line width for the border of the plotting regions
# -breaks       see `graphics::hist`
# -include.lowest see `graphics::hist`
# -right          see `graphics::hist`
# -draw.density   see `graphics::hist`
# 
# == details
# It draw histogram in cells among a whole track. It is also an example to show how to draw self-defined
# figures by this package.
circos.trackHist = function(factors, x, track.height = circos.par("default.track.height"),
    track.index = NULL, ylim = NULL, force.ylim = TRUE,
    col = ifelse(draw.density, "black", NA), border = "black", lty = par("lty"), lwd = par("lwd"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE) {
    
    # basic check here
    if(length(x) != length(factors)) {
        stop("Length of data and length of factors differ.\n")
    }
        
    if(!is.factor(factors)) {
        factors = factor(factors)
    }
    
    # calculate the distributions
    le = levels(factors)
    
    xx = NULL
    yy = NULL
    fa = NULL
    
    for(i in seq_along(le)) {
        l = factors == le[i]
        nx = x[l]
        
        h = hist(nx, plot = FALSE, breaks = breaks, include.lowest = include.lowest, right = right)
        
        xx = c(xx, h$breaks)
        if(draw.density) {
            yy = c(yy, 0, h$density)
        } else {
            yy = c(yy, 0, h$counts)
        }
        
        fa = c(fa, rep(le[i], length(h$breaks)))
    }
    
    # create the plotting region
    circos.trackPlotRegion(factors = fa, y=yy, track.height = track.height,
                      track.index = track.index, force.ylim = force.ylim,
                      bg.col = bg.col, bg.border = bg.border, bg.lty = bg.lty, bg.lwd = bg.lwd)
    
    track.index = get.current.track.index()
	
	l3 = logical(0)
	for(i in seq_along(le)) {
		cell.xlim = get.cell.meta.data("xlim", sector.index = le[i], track.index = track.index)
		l = fa == le[i]
		l2 = xx[l] >= cell.xlim[1] & xx[l] <= cell.xlim[2]
		l3 = c(l3, l2)
	}
	
	xx = xx[l3]
	yy = yy[l3]
	fa = fa[l3]
    
    if(draw.density) {
        circos.trackLines(factors = fa, xx, yy, track.index = track.index,
                          col = col, lty = lty, lwd = lwd)
    } else {
        # in each cell, draw rectangles
        col = recycle.with.levels(col, le)
        border = recycle.with.levels(border, le)
        lty = recycle.with.levels(lty, le)
        lwd = recycle.with.levels(lwd, le)
        for(i in seq_along(le)) {
            l = fa == le[i]
            
            nx = xx[l]
            ny = yy[l]

            cell.data = get.cell.data(le[i], track.index)
            nx[nx < cell.data$xlim[1]] = cell.data$xlim[1]    
            nx[nx > cell.data$xlim[2]] = cell.data$xlim[2]
            
            for(j in seq_along(nx)) {
                if(j == 1) {
                    next
                }
                
                circos.rect(nx[j-1], 0, nx[j], ny[j],
                            sector.index = le[i], track.index = track.index,
                            col = col[i], border = border[i], lty = lty[i], lwd = lwd[i])
            }
        }
    }
    return(invisible(NULL))
}

# == title
# Initialize the circos layout with an ideogram
#
# == param
# -file cytoband file. By default it is the cytoband data for human
# -track.height height for the track
#
# == details
# This is not a full functional function. It jus provides a way to show how to
# draw genomics ideogram by this package. How to embed the ideogram into the
# circos layout is really subjective and should be applied according to specific situation.
#
# In fact, draw ideogram with this package is really simple, you can look at the source code
# of this function to get a clue.
#
# The cytoband data for human is downloaded from UCSC ftp site (http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz),
# should be uncompressed.
circos.initializeWithIdeogram = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), track.height = 0.1) {
	
	d = read.table(file, colClasses = c("factor", "numeric", "numeric", "factor", "factor"))

	chromosome = levels(d[[1]])
	chromosome.ind = gsub("chr", "", chromosome)
	chromosome.num = grep("^\\d+$", chromosome.ind, value = TRUE)
	chromosome.letter = chromosome.ind[!grepl("^\\d+$", chromosome.ind)]
	chromosome.num = sort(as.numeric(chromosome.num))
	chromosome.letter = sort(chromosome.letter)
	chromosome.num = paste("chr", chromosome.num, sep = "")
	chromosome.letter = paste("chr", chromosome.letter, sep = "")
	
	chromosome = c(chromosome.num, chromosome.letter)
	
	xlim = matrix(nrow = 0, ncol = 2)
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
	}
	
	circos.clear()
	par(mar = c(1, 1, 1, 1), lwd = 0.5)
	o.cell.padding = circos.par("cell.padding")
	circos.par("cell.padding" = c(0, 0, 0, 0))
	circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
	circos.trackPlotRegion(factors = factor(chromosome, levels = chromosome), ylim = c(0, 1), bg.border = NA, track.height = track.height)
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		n = nrow(d2)
		col = rep("#FFFFFF", n)
		col[d2[[5]] == "acen"] = "#E41A1C"
		col[d2[[5]] == "stalk"] = "#377EB8"
		col[d2[[5]] == "gvar"] = "#404040"
		col[d2[[5]] == "gpos100"] = "#000000"
		col[d2[[5]] == "gpos"] = "#000000"
		col[d2[[5]] == "gpos75"] = "#BFBFBF"
		col[d2[[5]] == "gpos50"] = "#808080"
		col[d2[[5]] == "gpos25"] = "#404040"
		for(i in seq_len(n)) {
			circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr, col = col[i], border = NA)
		}
		circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
		major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 50000000)
		circos.axis(h = 0.5, major.at = major.at, labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, labels.cex = 0.2)
		cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
		circos.text(cell.xlim[1] + mean(cell.xlim), 1.2, labels = gsub("chr", "", chr), sector.index = chr, cex = 0.8)
	}
	circos.par("cell.padding" = o.cell.padding)
}

