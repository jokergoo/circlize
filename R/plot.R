# == title
# Create or update plot regions
#
# == details
# This function pretends to be high-level plotting function, which means, 
# you must firstly call this function to create a plotting region, then those
# low-level-style plotting function such as `circos.points`, `circos.lines` can be
# applied.
#
# It has two different usages. First, it can create a complete track which among several
# sectors. Because currently it does not support creating single cell since it would
# make the layout disordered, this is the only way to create a plotting region.
#
# Currently, all the cells that are created in a same track share same height, which means,
# there is no cell has longer height than others. However, this can be simulated by other
# methods.
#
# Second, it can update a already-created plotRegion if the index for sector and track
# is specified. But, if one plotting region is updated, those parameters such as the position
# of the plotting region can not be changed.
circos.trackPlotRegion = function(x = NULL, y = NULL, factors = NULL, track.start = NULL, track.height = circos.par("default.track.height"),
    sector.index = NULL, track.index = NULL, xlim = NULL, ylim = NULL, forced.ylim = TRUE,
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
	panel.fun = NULL) {
    
	# if ``factor`` has not been specified, arguments of ``track.start``, ``track.height``,
	# ``xlim`` and ``ylim`` which are related to the position of cells are ignored.
	
	if(is.null(factors)) {
        stop("factors should be specified.\n")
    } else {
		
		# ``sector.index`` and ``xlim`` are ignored
		
		# basic check here
		# if ``ylim`` set then do not need ``y``
		if(is.null(ylim) && length(y) != length(factors)) {
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
			set.current.track.index(track.index)
		}
		
		
		# whether force ylim for all cells in a track same
		if(is.null(ylim) && forced.ylim) {
			y.range = range(y)
		}
		
		le = levels(factor(factors))
		bg.col = recycle.with.levels(bg.col, le)
		bg.border = recycle.with.levels(bg.border, le)
		bg.lty = recycle.with.levels(bg.lty, le)
		bg.lwd = recycle.with.levels(bg.lwd, le)
		
		# if ``track.start`` has not been specified, start from the most recently
		# created track
		if(is.null(track.start)) {
			track.start = get.track.end.position(track.index - 1) - circos.par("track.margin")[1]
		}
		
		# check whether there is enough space for the new track and whether the new space
		# overlap with other tracks
		check.track.position(track.index, track.start, track.height)
		
		# if ``panel.fun`` specified, need to check the ``...``
		# in ``...``, arguments only can be vectors
		if(!is.null(panel.fun)) {
			
			if((!is.null(x) && length(x) != length(factors)) || 
			   (!is.null(y) && length(y) != length(factors))) {
				stop("Length of data and length of factors differ.\n")
			}
		}
		
		if(!is.null(ylim)) {
			cell.padding = circos.par("cell.padding")
			ylim[1] = ylim[1] - (ylim[2] - ylim[1])*cell.padding[1]
			ylim[2] = ylim[2] + (ylim[2] - ylim[1])*cell.padding[3]
		}
		
		# now for each factor
		for(i in seq_along(le)) {
			
			l = factors == le[i]
			
			# if ``ylim`` is not forced to be identical in all cells, then each cell has
			# its own ``ylim``
			if(is.null(ylim) && (!forced.ylim)) {
				y.range = range(y[l])
			}
			
			sector.data = get.sector.data(le[i])
			xlim = c(sector.data["start.value"], sector.data["end.value"])
			if(is.null(ylim)) {
				ylim = y.range
				cell.padding = circos.par("cell.padding")
				ylim[1] = ylim[1] - (ylim[2] - ylim[1])*cell.padding[1]
				ylim[2] = ylim[2] + (ylim[2] - ylim[1])*cell.padding[3]
			}
			
			
			# create plotting region for single cell
			circos.createPlotRegion(track.start = track.start,
			                  track.height = track.height, sector.index = le[i],
							  track.index = track.index,
							  xlim = xlim, ylim = ylim, bg.col = bg.col[i],
							  bg.border = bg.border[i], bg.lty = bg.lty[i], bg.lwd = bg.lwd[i])

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
        
		
		# After the track has been created, the default tract starting position is set
		# just next to the most recently created track
		set.track.end.position(track.index, track.start - track.height - circos.par("track.margin")[2])
		return(invisible(NULL))
	}

	
}

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
    circos.rect(xlim[1], ylim[1],
        xlim[2], ylim[2], col = bg.col, border = bg.border,
        lty = bg.lty, lwd = bg.lwd)
    return(invisible(NULL))
}
# == title 
# Add points to the plotting region
#
# == details
# This function can only add points in a specified cell. Pretending a low-level plotting 
# function, it can only be applied in the plottting region which has been created.
#
# You can think the function as the normal `graphics::points`
# function, just adding points in the current plotting region. The position of current
# plotting region is identified by `sector.index` and `track.index`, if they are not
# specified, values would be fetched through `get.current.sector.index` and
# `get.current.track.index`.
#
# Data points out of the plotting region will not be plotted, but with a warning message.
# Currently, the function does not support plotting points that are out of the plotting
# region.
#
# Other graphics parameters which are available in the function are ``pch``, ``col``
# and ``cex`` which have same meaning with those in the `graphics` package.
circos.points = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
                         pch = par("pch"), col = par("col"), cex = par("cex"), ...) {
	
	if(!has.cell(sector.index, track.index)) {
		stop("'circos.points' can only be used after the plotting region been created\n")
	}
	
	# whether the points that are out of the plotting region.
	# If there is, throw warnings.
	check.points.position(x, y, sector.index, track.index)
	
    d = circlize(x, y, sector.index, track.index)
    points(polar2Cartesian(d), pch = pch, col = col, cex = cex, ...)
	return(invisible(NULL))
}

# == title 
# Add points to the plotting regions in one track
#
# The function adds points in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add points in cells corresponding
# to the part of data.
circos.trackPoints = function(x, y, factors, track.index = get.current.track.index(),
                         pch = par("pch"), col = par("col"), cex = par("cex"), ...) {
	
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
					  pch = npch, col = ncol, cex = ncex,
					  ...)
			
	}
	return(invisible(NULL))
}

# straight lines are transformed into curves, so there should be some segmentation
# of the original lines
#
# Normally, straight lines in the Cartesian coordinate would be transformed into curves.
# But if you do not want to do such changings you can use this function just drawing straight
# lines between points.
#
# Sometimes, in such circos layout graph, cells may be small, curves and straight lines may 
# be hard to tell. On the other hand, transforming between straight lines to curves need
# add additional points between two data points to segmentate the lines, transform the coordinate
# and simulate the curve by a lot of tiny line segments. The one consequence of transforming to curves
# is that if you want to generate the graph as `grDevices::pdf` format, you would be surprised 
# that the size of the file is so big.
circos.lines = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index(),
	col = "black", lwd = par("lwd"), lty = par("lty"), type = "l", straight = FALSE,
	pt.col = "black", cex = par("cex"), pch = par("pch"), ...) {
    
	if(type == "l") {
		
	} else if(type == "o") {
		circos.points(x, y, sector.index = sector.index, track.index = track.index,
		              col = pt.col, cex = cex, pch = pch, ...)
		circos.lines(x, y, sector.index = sector.index, track.index = track.index,
	                 col = col, lwd = lwd, lty = lty)
		return(invisible(NULL))
	} else if(type == "h") {
		cell.data = get.cell.data(sector.index, track.index)
		for(i in seq_along(x)) {
			circos.lines(c(x[i], x[i]), c(cell.data$ylim[1], y[i]),
			             sector.index = sector.index, track.index = track.index, 
						 col = col, lwd = lwd, lty = lty, straight = TRUE)	
		}
		return(invisible(NULL))
	} else if(type == "s") {
		for(i in seq_along(x)) {
			if(i == 1) {
				next
			}
			circos.lines(c(x[i-1], x[i]), c(y[i-1], y[i-1]), sector.index = sector.index, track.index = track.index, 
			             col = col, lwd = lwd, lty = lty)
			circos.lines(c(x[i], x[i]), c(y[i-1], y[i]),
			             sector.index = sector.index, track.index = track.index,
						 col = col, lwd = lwd, lty = lty, straight = TRUE)
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
		d = lines.expand(x, y, sector.index)
	}
	
	d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
	lines(polar2Cartesian(d2), col = col, lwd = lwd, lty = lty)
	return(invisible(NULL))
}

circos.trackLines = function(x, y, factors, track.index = get.current.track.index(),
                         col = "black", lwd = par("lwd"), lty = par("lty"), straight = FALSE,
						 pt.col = "black", cex = par("cex"), pch = par("pch"), type = "l", ...) {
	
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
					  col = ncol, lwd = nlwd, lty = nlty,
					  pt.col = npt.col, cex = ncex, pch = npch, type = type, straight = straight, ...)
			
	}
	return(invisible(NULL))
}


# == title
# Draw rectangle-like grid
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
# == details
# similar as `graphics::polygon`
circos.polygon = function(x, y,	sector.index = get.current.sector.index(), track.index = get.current.track.index(),
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd")) {
    
	if(!has.cell(sector.index, track.index)) {
		stop("'circos.polygon' can only be used after the plotting region been created\n")
	}
	
	# whether the points that are out of the plotting region.
	check.points.position(x, y, sector.index, track.index)
	
	d = lines.expand(x, y, sector.index)
    d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
    polygon(polar2Cartesian(d2), col = col, border = border,
	        lty = lty, lwd = lwd)
	return(invisible(NULL))
}

# to-do: text as an arc?
circos.text = function(x, y, labels, sector.index = get.current.sector.index(), track.index = get.current.track.index(), 
    direction = c("default", "vertical_left", "vertical_right", "horizontal"),
	srt = NULL, adj = par("adj"), cex = 1, col = "black", font = par("font")) {
    
	if(!has.cell(sector.index, track.index)) {
		stop("'circos.text' can only be used after the plotting region been created\n")
	}
	
	# whether the points that are out of the plotting region.
	check.points.position(x, y, sector.index, track.index)
	
	d = circlize(x, y, sector.index, track.index)
	
	# if srt not set, srt for text are calculated automaticly
	if(is.null(srt)) {
		direction = direction[1]
		if(! direction %in% c("default", "vertical_left", "vertical_right", "horizontal")) {
			stop("direction can only be choosen from 'default', 'vertical_left', 'vertical_right', 'horizontal'\n.")
		}
		
		srt = d[,1]-90	#srt = ifelse(srt > 0, srt, 360 + srt)
		
		if(direction == "vertical_left") {           # pointing to the circle center, but facing left at 90 degree
			srt = srt - 90
		} else if(direction == "vertical_right") {   # pointing to the circle center, but facing right at 90 degree
			srt = srt + 90
		} else if(direction == "horizontal") {       # horizontal at the finnal graph
			srt = rep(0, length(srt))
		}
	}
	
	m = polar2Cartesian(d)
	# because ``srt`` can only with length 1
	# here we do not expand ``adj``, because the original ``adj``
	# is a vector, after expanding, it would be a matrix. I think 
	# no one would set it as a matrix
	if(length(srt) == 1) {
		srt = rep(srt, length(x))
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
	
	for(i in seq_along(x)) {
		text(m[i, 1], m[i, 2], labels = labels[i], srt = srt[i],
		     cex = cex[i], col = col[i], font = font[i], adj = adj)
	}
	
	return(invisible(NULL))
}

circos.trackText = function(x, y, labels, factors, track.index = get.current.track.index(),
                       direction = c("default", "vertical_left", "vertical_right", "horizontal"),
	                   srt = NULL, adj = par("adj"), cex = 1, col = "black", font = par("font")) {
	
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
	# ``direction``, ``srt`` and ``adj`` are not recycled
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
					  direction = direction, srt = srt, adj = adj,
					  cex = ncex, col = ncol, font = nfont)
			
	}
	return(invisible(NULL))
}

circos.link = function(sector.index1,
                       point1,  # x or c(x, y)
                       sector.index2,
                       point2,
                       rou = get.track.end.position(get.current.track.index()),
                       top.ratio = 0.5,
                       col = "grey", lwd = par("lwd"), lty = par("lty"), border = "black") {
    
    sector.data1 = get.sector.data(sector.index1)
    sector.data2 = get.sector.data(sector.index2)
    
    if(length(point1) == 1 && length(point2) == 1) {
        theta1 = (point1 - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
                 degree.minus(sector.data1["end.degree"] - sector.data1["start.degree"]) + sector.data1["start.degree"]
        
        theta2 = (point2 - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
                 (sector.data2["end.degree"] - sector.data2["start.degree"]) + sector.data2["start.degree"]
        
        d = rotate.parabola(theta1 = theta1, theta2 = theta2, rou1 = rou, rou.ratio = top.ratio)
        lines(d, col = col, lwd = lwd, lty = lty)
    } else {
        if(length(point1) == 1) {
            point1 = c(point1, point1)   
        }
        if(length(point2) == 1) {
            point2 = c(point2, point2)   
        }
        
        theta11 = (point1[1] - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
            (sector.data1["end.degree"] - sector.data1["start.degree"]) + sector.data1["start.degree"]
        theta12 = (point1[2] - sector.data1["start.value"]) / (sector.data1["end.value"] - sector.data1["start.value"]) *
            (sector.data1["end.degree"] - sector.data1["start.degree"]) + sector.data1["start.degree"]
        
        theta21 = (point2[1] - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
            (sector.data2["end.degree"] - sector.data2["start.degree"]) + sector.data2["start.degree"]
        theta22 = (point2[2] - sector.data2["start.value"]) / (sector.data2["end.value"] - sector.data2["start.value"]) *
            (sector.data2["end.degree"] - sector.data2["start.degree"]) + sector.data2["start.degree"]
        
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
        
        w1 = abs(theta11 - theta12)/180*pi*rou
        w2 = abs(theta21 - theta22)/180*pi*rou
        w = max(w1, w2)
        if(abs(theta11 - theta21) > abs(theta12 - theta22)) {
            r1 = 0.5
            r2 = 0.6
        } else {
            r1 = 0.5
            r2 = 0.4
        }
        d1 = rotate.parabola(theta1 = theta11, theta2 = theta21, rou1 = rou, rou.ratio = top.ratio)
        d2 = rotate.parabola(theta1 = theta12, theta2 = theta22, rou1 = rou, rou.ratio = top.ratio)

		if(is.points.ordered.on.circle(c(theta11, theta21, theta22, theta12))) {
			d2 = d2[rev(seq_len(nrow(d2))), ]
			r1 = arc.points(theta12, theta22, rou, clock.wise = TRUE)
            r2 = arc.points(theta21, theta11, rou, clock.wise = TRUE)
		} else if(is.points.ordered.on.circle(c(theta11, theta21, theta22, theta12), clock.wise = TRUE)) {
            d2 = d2[rev(seq_len(nrow(d2))), ]
			r1 = arc.points(theta12, theta22, rou)
            r2 = arc.points(theta21, theta11, rou)
		} else if(is.points.ordered.on.circle(c(theta21, theta11, theta12, theta22))) {
			d2 = d2[rev(seq_len(nrow(d2))), ]
			r1 = arc.points(theta12, theta22, rou)
            r2 = arc.points(theta21, theta11, rou)
		} else if(is.points.ordered.on.circle(c(theta21, theta11, theta12, theta22), clock.wise = TRUE)) {
		    d2 = d2[rev(seq_len(nrow(d2))), ]
			r1 = arc.points(theta12, theta22, rou, clock.wise = TRUE)
            r2 = arc.points(theta21, theta11, rou, clock.wise = TRUE)
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
		d = rbind(d, d2)
        polygon(d, col = col, lty = lty, lwd = lwd, border = border)
    }
    return(invisible(NULL))
}


#####################################################################
#
# simulate high-level graphic functions such as barplot, hist, boxplot ...
#
#####################################################################

# first a global hist
circos.trackHist = function(x, factors, track.start = NULL, track.height = circos.par("default.track.height"),
    track.index = NULL, xlim = NULL, ylim = NULL, forced.ylim = TRUE,
	col = NA, border = "black", lty = par("lty"), lwd = par("lwd"),
	bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
	breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE) {
	
	# basic check here
	if(length(x) != length(factors)) {
		stop("Length of data and length of factors differ.\n")
	}
		
	if(!is.factor(factors)) {
		factors = factor(factors)
	}
	
	if(length(setdiff(levels(factors), get.all.sector.index()))) {
		stop("Factors name should be all in existed sector index.\n")
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
	circos.trackPlotRegion(y=yy, factors = fa, track.start = track.start, track.height = track.height,
                      track.index = track.index, forced.ylim = forced.ylim,
	                  bg.col = bg.col, bg.border = bg.border, bg.lty = bg.lty, bg.lwd = bg.lwd)
	
	track.index = get.current.track.index()
	
	if(draw.density) {
		circos.trackLines(xx, yy, factors = fa, track.index = track.index,
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
