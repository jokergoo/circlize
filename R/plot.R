# == title
# Create plotting regions for a whole track
#
# == param
# -factors      A `factor` or a character vector which represents categories of data, if it is ``NULL``,
#               then it uses all sector index.
# -x            Data on x-axis. It is only used if ``panel.fun`` is set.
# -y            Data on y-axis
# -ylim         Range of data on y-axis
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``. Normally,
#               all cells on a same track should have same ``ylim``.
# -track.index  Index for the track which is going to be created/updated. If the specified track has already
#               been created, this function just updated corresponding track with new plot. If the specified track
#               is ``NULL`` or has not been created, this function just creates it. Note the value for this
#               argument should not exceed maximum track index plus 1.
# -track.height Height of the track. It is the percentage to the radius of the unit circles. The value can be set by `uh` to an absolute unit.
#               If updating a track (with proper ``track.index`` value), this argument is ignored.
# -track.margin only affect current track
# -cell.padding only affect current track
# -bg.col       Background color for the plotting regions. It can be vector which has the same length of sectors.
# -bg.border    Color for the border of the plotting regions. It can be vector which has the same length of sectors.
# -bg.lty       Line style for the border of the plotting regions. It can be vector which has the same length of sectors.
# -bg.lwd       Line width for the border of the plotting regions. It can be vector which has the same length of sectors.
# -panel.fun    Panel function to add graphics in each cell, see "details" section
#               and vignette for explanation.
#
# == details
# This function tends to be a high-level plotting function, which means,
# you must first call this function to create plotting regions, then those
# low-level graphic function such as `circos.points`, `circos.lines` can be
# applied.
#
# Currently, all the cells that are created in a same track sharing same height, which means,
# there is no cell has larger height than others.
#
# Since ranges for values on x-axis has already been defined by `circos.initialize`, only
# ranges for values on y-axis should be specified in this function.
# There are two ways to identify the ranges for values on y-axes either by ``y``
# or ``ylim``. If ``y`` is set, it must has the same length as ``factors`` and the ``ylim`` for each cell is calculated
# from y values. Also, the ylim can be specified from ``ylim`` which can be a two-element vector or a matrix which
# has two columns and the number of rows is the same as the length of the levels of the factors.
#
# If there is no enough space for the new track or the new track overlaps with other tracks,
# there will be an error.
#
# If ``factors`` does not cover all sectors, the cells in remaining unselected
# sectors would also be created but without drawing anything. The ``ylim`` for these cells
# are the same as that in the last created cell.
#
# The function can also update a already-created track if the index for the track
# is specified. If updating an existed track, those parameters related to the position (such as track height and track margin)
# of the plotting region can not be changed.
#
# == Panel function
#
# ``panel.fun`` provides a convenient way to add graphics in each cell when initializing the
# tracks. The self-defined function needs two arguments: ``x`` and ``y`` which correspond to the data points
# in the current cell. When ``factors``, ``x``, and ``y`` are set in `circos.trackPlotRegion`, a subset of ``x``
# and ``y`` are split by ``factors`` and are sent to ``panel.fun`` in the "current" cell.
# `circos.trackPlotRegion` creates plotting regions one by one on the track and
# ``panel.fun`` adds graphics in the 'current' cell after the plotting region for a certain cell has been
# created.
#
# See vignette for examples of how to use this feature.
#
# == seealso
# http://jokergoo.github.io/circlize_book/book/circular-layout.html
circos.trackPlotRegion = function(factors = NULL, x = NULL, y = NULL, ylim = NULL,
    force.ylim = TRUE, track.index = NULL,
	track.height = circos.par("track.height"),
	track.margin = circos.par("track.margin"),
	cell.padding = circos.par("cell.padding"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    panel.fun = function(x, y) {NULL}) {

    if(!is.circos.initialized()) {
    	stop_wrap("Your circular plot has not been initialized yet!")
    }

	o.track.margin = circos.par("track.margin")
	o.cell.padding = circos.par("cell.padding")
	circos.par(track.margin = track.margin)
	circos.par(cell.padding = cell.padding)

	# if there is no factors, default are all the available factors
	if(is.null(factors)) {
		factors = get.all.sector.index()
		factors = factor(factors, levels = factors)
	}

    # although ``x`` and ``y`` are not necessary, but once they are set, they must
	# have same length as ``factors``
    if(!is.null(y) && length(y) != length(factors) ||
	   !is.null(x) && length(x) != length(factors)) {
        stop_wrap("Length of data and length of factors differ.")
    }

	# need to be a factor
    if(!is.factor(factors)) {
        factors = factor(factors)
    }

	# check whether there are some categories that are not in the circle
	setdiff.factors = setdiff(levels(factors), get.all.sector.index())
    if(length(setdiff.factors)) {
        stop_wrap("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".")
    }

	tracks = get.all.track.index()
	last.track.index = ifelse(length(tracks), tracks[length(tracks)], 0)
	flag_createNewTrack = 0
    if(is.null(track.index)) {
        # new track should inside the most recently created track
        set.current.track.index(last.track.index + 1)
        track.index = get.current.track.index()
		flag_createNewTrack = 1
    } else if(track.index == last.track.index + 1) {
		# if the track.index is next to the most recently created track
		set.current.track.index(track.index)
        track.index = get.current.track.index()
		flag_createNewTrack = 1
	} else {
		if(track.index > last.track.index + 1) {
			stop_wrap("Wrong track index: it should be no more than ", last.track.index + 1, ".")
		}
		# update an existed track
		if(track.index <= tracks[length(tracks)]) {
			# ignore track.height from args
			track.height = get.cell.meta.data("track.height", sector.index = factors[1], track.index = track.index)
			# ignore track.margin
			circos.par("track.margin" = get.cell.meta.data("track.margin", sector.index = factors[1], track.index = track.index))
			circos.par("cell.padding" = get.cell.meta.data("cell.padding", sector.index = factors[1], track.index = track.index))
		}
        if(is.null(ylim) && is.null(y)) {
            for(sid in get.all.sector.index()) {
                ylim = rbind(ylim, get.cell.meta.data("ylim", sector.index = sid, track.index = track.index))
            }
        }
        set.current.track.index(track.index)
    }


    le = levels(factors)
	nlevel = length(le)
    bg.col = recycle.with.levels(bg.col, le)
    bg.border = recycle.with.levels(bg.border, le)
    bg.lty = recycle.with.levels(bg.lty, le)
    bg.lwd = recycle.with.levels(bg.lwd, le)

     # whether to force ylim for all cells in a track same
    if(is.null(ylim)) {
		if(is.null(y)) {
			stop_wrap("You have to specify either `y` or `ylim`.")
		}

		if(force.ylim) {
			y.range = range(y)
			y.range = matrix(rep(y.range, nlevel), ncol = 2, byrow = TRUE)
		} else {
			y.range = tapply(y, factors, range)
			y.range = matrix(unlist(y.range), ncol = 2, byrow = TRUE)
		}
	}

	if(flag_createNewTrack) {
		if(track.index == 1) {
			track.start = 1 - circos.par("track.margin")[2]
		} else {
			track.start = get.cell.meta.data("cell.bottom.radius", track.index = track.index - 1) -
			              get.cell.meta.data("track.margin", track.index = track.index - 1)[1] -
						  circos.par("track.margin")[2]
		}
    } else {
		track.start = get.cell.meta.data("cell.top.radius", track.index = track.index)
	}

    # check whether there is enough space for the new track and whether the new space
    # overlap with other tracks. Only for creatation mode.
	if(flag_createNewTrack) {
		check.track.position(track.index, track.start, track.height)
    }

	# if `ylim` is specified
    if(!is.null(ylim)) {
		if(is.vector(ylim) && length(ylim) == 2) {
			ylim = matrix(rep(ylim, length(le)), ncol = 2, byrow = TRUE)
		} else if(is.matrix(ylim) && ncol(ylim) == 2 && nrow(ylim) == length(le)) {

		} else {
			stop_wrap("Wrong `ylim` format.")
		}
    }

    # now for each factor, create plotting region
    for(i in seq_along(le)) {
		# `ylim` is prior to `y`
        if(is.null(ylim)) {
			ylim2 = y.range[i, ]
        } else {
			ylim2 = ylim[i, ]
		}

        # create plotting region for single cell
        circos.createPlotRegion(track.start = track.start,
                              track.height = track.height, sector.index = le[i],
                              track.index = track.index,
                              ylim = ylim2, bg.col = bg.col[i],
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

			# ylim is the most recent ``ylim2``
			circos.createPlotRegion(track.start = track.start,
								  track.height = track.height, sector.index = le2[i],
								  track.index = track.index,
								  ylim = ylim2, bg.col = NA,
								  bg.border = NA)
		}
	}

	circos.par(track.margin = o.track.margin)
	circos.par(cell.padding = o.cell.padding)

    return(invisible(NULL))

}

# == title
# Create plotting regions for a whole track
#
# == param
# -... pass to `circos.trackPlotRegion`
#
# == details
# Shortcut function of `circos.trackPlotRegion`.
#
circos.track = function(...) {
	circos.trackPlotRegion(...)
}

# == title
# Update the plotting region in an existed cell
#
# == param
# -sector.index Index for the sector
# -track.index  Index for the track
# -bg.col       Background color for the plotting region
# -bg.border    Color for the border of the plotting region
# -bg.lty       Line style for the border of the plotting region
# -bg.lwd       Line width for the border of the plotting region
#
# == details
# You can update an existed cell by this function by erasing all the graphics.
# But the ``xlim`` and ``ylim`` inside the cell still remain unchanged.
#
# Note if you use `circos.track` to update an already created track,
# you can re-define ``ylim`` in these cells.
circos.updatePlotRegion = function(sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd")) {

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("You can only update an existed cell.")
    }

    cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sector.index, track.index = track.index)

    set.current.sector.index(sector.index)
    set.current.track.index(track.index)

    # cover the exsited region by fill with white
    lwd = get.cell.meta.data("bg.lwd", sector.index = sector.index, track.index = track.index)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2],
        col = "white", border = "white", lty = 1, lwd = lwd)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2],
        col = bg.col, border = bg.border, lty = bg.lty, lwd = bg.lwd)
    return(invisible(NULL))
}

# == title
# Create plotting regions for a whole track
#
# == param
# -... pass to `circos.updatePlotRegion`
#
# == details
# shortcut function of `circos.updatePlotRegion`.
#
circos.update = function(...)  {
	circos.updatePlotRegion(...)
}

# internal, so we do not need to check arguments
circos.createPlotRegion = function(track.start, track.height = circos.par("track.height"),
    sector.index = get.cell.meta.data("sector.index"), track.index = get.cell.meta.data("track.index"), ylim,
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd")) {

	# we do not have such meta for the cell, so we need to calculate them
	sector.data = get.sector.data(sector.index)
    cell.xlim = c(sector.data["min.value"], sector.data["max.value"])
	names(cell.xlim) = NULL

	cell.padding = circos.par("cell.padding")

	xlim = c(sector.data["min.data"], sector.data["max.data"])

	if(cell.padding[1] + cell.padding[3] >= track.height) {
		stop_wrap("Summation of cell padding on y-direction are larger than the height of the cells.")
	}

	if(ylim[2] == ylim[1]) {
		stop_wrap("range of `ylim` should be different.")
	}

	yl = numeric(2)
	yl[1] = ylim[1] - (ylim[2] - ylim[1])*cell.padding[1] / track.height
    yl[2] = ylim[2] + (ylim[2] - ylim[1])*cell.padding[3] / track.height

    set.cell.data(sector.index = sector.index,
        track.index = track.index,
		xlim = xlim,
		ylim = ylim,
        cell.xlim = cell.xlim,
        cell.ylim = yl,
        track.start = track.start,
        track.height = track.height,
		track.margin = circos.par("track.margin"),
		cell.padding = circos.par("cell.padding"),
        bg.col = bg.col,
        bg.border = bg.border,
        bg.lty = bg.lty,
        bg.lwd = bg.lwd)

    set.current.sector.index(sector.index)

    # The plotting region is a rectangle
	cell.ylim = yl
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], sector.index = sector.index, track.index = track.index,
        col = bg.col, border = bg.border, lty = bg.lty, lwd = bg.lwd)
    return(invisible(NULL))
}

# == title
# Add points to a plotting region
#
# == param
# -x            Data points on x-axis, measured in "current" data coordinate
# -y            Data points on y-axis, measured in "current" data coordinate
# -sector.index Index for the sector
# -track.index  Index for the track
# -pch          Point type
# -col          Point color
# -cex          Point size
# -bg           backgrond of points
#
# == details
# This function can only add points in one specified cell. Pretending a low-level plotting
# function, it can only be applied in plotting region which has been created.
#
# You can think the function similar as the normal `graphics::points`
# function, just adding points in the circular plotting region. The position of
# cell is identified by ``sector.index`` and ``track.index``, if they are not
# specified, they are in 'current' sector and 'current' track.
#
# Data points out of the plotting region will also be added, but with warning messages.
#
# Other graphics parameters which are available in the function are ``pch``, ``col``
# and ``cex`` which have same meaning as those in the `graphics::par`.
#
# It is recommended to use `circos.points` inside ``panel.fun`` in `circos.trackPlotRegion` so that
# it draws points directly on "curent" cell.
circos.points = function(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    pch = par("pch"), col = par("col"), cex = par("cex"), bg = par("bg")) {

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.points' can only be used after the plotting region has been created")
    }

    len_x = length(x)
    len_y = length(y)
    if(len_x == 1) x = rep(x, len_y)
    if(len_y == 1) y = rep(y, len_x)

	if(length(x) != length(y)) {
		stop_wrap("Length of x and y differ.")
	}

    # whether the points that are out of the plotting region.
    # If there is, throw warnings.
    check.points.position(x, y, sector.index, track.index)

    d = circlize(x, y, sector.index = sector.index, track.index = track.index)
    points(polar2Cartesian(d), pch = pch, col = col, cex = cex, bg = bg)
    return(invisible(NULL))
}

# == title
# Add points to the plotting regions in a same track
#
# == param
# -factors      A `factor` or a character vector which represents the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -track.index  Index for the track
# -pch          Point type
# -col          Point color
# -cex          Point size
# -bg           backgrond color
#
# == details
# The function adds points in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then adding points in each cell by calling `circos.points`.
#
# Length of ``pch``, ``col`` and ``cex`` can be one, length of levels of the factors or length of
# factors.
#
# This function can be replaced by a ``for`` loop containing `circos.points`.
#
circos.trackPoints = function(factors = NULL, x, y, track.index = get.cell.meta.data("track.index"),
    pch = par("pch"), col = par("col"), cex = par("cex"), bg = par("bg")) {

    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop_wrap("Length of data and length of factors differ.\n")
    }

    if(!is.factor(factors)) {
        factors = factor(factors)
    }

	# check whether there are some categories that are not in the circle
	setdiff.factors = setdiff(levels(factors), get.all.sector.index())
    if(length(setdiff.factors)) {
        stop_wrap("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".")
    }

    le = levels(factors)

    # set these graphic parameters with same length as the factors
    pch = recycle.with.factors(pch, factors)
    col = recycle.with.factors(col, factors)
    cex = recycle.with.factors(cex, factors)
    bg = recycle.with.factors(bg, factors)

    for(i in seq_along(le)) {
        l = factors == le[i]

        nx = x[l]
        ny = y[l]
        npch = pch[l]
        ncol = col[l]
        ncex = cex[l]
        nbg = bg[l]
        circos.points(nx, ny, sector.index = le[i],
                      track.index = track.index,
                      pch = npch, col = ncol, cex = ncex, bg = nbg)

    }
    return(invisible(NULL))
}

# == title
# Add lines to the plotting region
#
# == param
# -x            Data points on x-axis, measured in "current" data coordinate
# -y            Data points on y-axis, measured in "current" data coordinate
# -sector.index Index for the sector
# -track.index  Index for the track
# -col          Line color
# -lwd          line width
# -lty          line style
# -type         line type, similar as ``type`` argument in `graphics::lines`, but only in ``c("l", "o", "h", "s")``
# -straight     whether draw straight lines between points.
# -area         whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls color of the line.
# -area.baseline deprecated, use ``baseline`` instead.
# -baseline     the base line to draw areas. By default it is the minimal of y-range (bottom). It can be a string or a number.
#               If a string, it should be one of ``bottom`` and ``top``. This argument also works if ``type`` is set to ``h``.
# -border       color for border of the area
# -pt.col       if ``type`` is "o", point color
# -cex          if ``type`` is "o", point size
# -pch          if ``type`` is "o", point type
#
# ==details
# Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circular layout.
# But if you do not want to do such transformation you can use this function just drawing straight
# lines between points by setting ``straight`` to ``TRUE``.
#
# Drawing areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be done by specifying
# ``area`` to ``TURE``.
circos.lines = function(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    col = ifelse(area, "grey", par("col")), lwd = par("lwd"), lty = par("lty"),
    type = "l", straight = FALSE, area = FALSE, area.baseline = NULL,
    border = "black", baseline = "bottom", pt.col = par("col"), cex = par("cex"),
    pch = par("pch")) {

	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning_wrap("`area.baseline` is deprecated, please use `baseline` instead.")
	}

	if(length(x) != length(y)) {
		stop_wrap("Length of x and y differ.")
	}

	if(baseline == "bottom") {
		baseline = get.cell.meta.data("ylim", sector.index, track.index)[1]
	} else if(baseline == "top") {
		baseline = get.cell.meta.data("ylim", sector.index, track.index)[2]
	}

    if(type == "l") {

    } else if(type == "o") {
        circos.points(x, y, sector.index = sector.index, track.index = track.index,
                      col = pt.col, cex = cex, pch = pch)
        circos.lines(x, y, sector.index = sector.index, track.index = track.index,
                     col = col, lwd = lwd, lty = lty, area = area, border = border)
        return(invisible(NULL))
    } else if(type == "h") {
    	if(length(col) == 1) col = rep(col, length(x))
    	if(length(lwd) == 1) lwd = rep(lwd, length(x))
    	if(length(lty) == 1) lty = rep(lty, length(x))
        for(i in seq_along(x)) {
            circos.lines(c(x[i], x[i]), c(baseline, y[i]),
                         sector.index = sector.index, track.index = track.index,
                         col = col[i], lwd = lwd[i], lty = lty[i], straight = TRUE)
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
			d = rbind(d, c(d[nrow(d), 1], baseline))
			d = rbind(d, c(d[1, 1], baseline))
			circos.polygon(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index,
				   col = col, border = border, lwd = lwd, lty = lty)
		} else {
			circos.lines(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index,
							 col = col, lwd = lwd, lty = lty)
        }
		return(invisible(NULL))
    }

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.lines' can only be used after the plotting region been created.")
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
		d = rbind(d, c(d[nrow(d), 1], baseline))
		d = rbind(d, c(d[1, 1], baseline))
		circos.polygon(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index,
		       col = col, border = border, lwd = lwd, lty = lty)
		return(invisible(NULL))
	}

    d2 = circlize(d[, 1], d[, 2], sector.index = sector.index, track.index = track.index)

    lines(polar2Cartesian(d2), col = col, lwd = lwd, lty = lty)
    return(invisible(NULL))
}

# == title
# Add lines to the plotting regions in a same track
#
# == param
# -factors      A `factor` or a character vector which represents the categories of data
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
# -area.baseline deprecated, use ``baseline`` instead.
# -baseline the base line to draw area, pass to `circos.lines`.
# -border       color for border of the area
# -pt.col       if ``type`` is "o", points color
# -cex          if ``type`` is "o", points size
# -pch          if ``type`` is "o", points type
#
# == details
# The function adds lines in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add lines in cells by calling `circos.lines`.
#
# This function can be replaced by a ``for`` loop containing `circos.lines`.
circos.trackLines = function(factors, x, y, track.index = get.cell.meta.data("track.index"),
    col = par("col"), lwd = par("lwd"), lty = par("lty"), type = "l", straight = FALSE,
	area = FALSE, area.baseline = NULL, border = "black", baseline = "bottom",
    pt.col = par("col"), cex = par("cex"), pch = par("pch")) {

	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning_wrap("`area.baseline` is deprecated, please use `baseline` instead.")
	}

    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop_wrap("Length of data and length of factors differ.")
    }

    if(!is.factor(factors)) {
        factors = factor(factors)
    }

    # check whether there are some categories that are not in the circle
	setdiff.factors = setdiff(levels(factors), get.all.sector.index())
    if(length(setdiff.factors)) {
        stop_wrap("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".")
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
	baseline = recycle.with.levels(baseline, le)
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
					  baseline = baseline[i],
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
# -... pass to `graphics::polygon`
#
# == details
# The name for this function is `circos.rect`
# because if you imagine the plotting region as Cartesian coordinate, then it is rectangle.
# in the polar coordinate, the up and bottom edge become two arcs.
#
# This function can be vectorized.
circos.rect = function(xleft, ybottom, xright, ytop,
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), ...) {

    # if(! (length(xleft) == 1 &&
    #       length(ybottom) == 1 &&
    #       length(xright) == 1 &&
    #       length(ytop) == 1) ) {
    #     stop("There should only be one data points in 'xleft', 'ybottom', 'xright' or 'ytop'.\n")
    # }

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.rect' can only be used after the plotting region been created.")
    }

    n1 = length(xleft)
    n2 = length(ybottom)
    n3 = length(xright)
    n4 = length(ytop)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) xleft = rep(xleft, n)
    if(n2 == 1) ybottom = rep(ybottom, n)
    if(n3 == 1) xright = rep(xright, n)
    if(n4 == 1) ytop = rep(ytop, n)

    if(! (length(xleft) == length(ybottom) && length(ybottom) == length(xright) && length(xright) == length(ytop)) ) {
		stop_wrap("xleft, ybottom, xright, ytop should have same length.")
	}

    # # no filled colors, just four edges, here edges colors are controled by ``border``
    # if(is.na(col)) {
    #     # vertical lines in the original coordinate system are still straight lines
    #     # in the new coordinate system except they now pointing to the circle center.
    #     circos.lines(c(xleft, xleft), c(ybottom, ytop),
    #                  sector.index = sector.index, track.index = track.index,
    #                  col = border, lty = lty, lwd = lwd, straight = TRUE)
    #     # horizontal lines in the original coordinate system are now arcs and the arcs
    #     # share the same circle center as the polar coordinate system
    #     circos.lines(c(xleft, xright), c(ytop, ytop),
    #                sector.index = sector.index, track.index = track.index,
    #                col = border, lty = lty, lwd = lwd)
    #     circos.lines(c(xright, xright), c(ytop, ybottom),
    #                  sector.index = sector.index, track.index = track.index,
    #                  col = border, lty = lty, lwd = lwd, straight = TRUE)
    #     circos.lines(c(xleft, xright), c(ybottom, ybottom),
    #                sector.index = sector.index, track.index = track.index,
    #                col = border, lty = lty, lwd = lwd)
    # } else {
    #     circos.polygon(c(xleft, xleft, xright, xright, xleft),
    #                    c(ybottom, ytop, ytop, ybottom, ybottom),
    #                    sector.index = sector.index, track.index = track.index,
    #                    col = col, border = border, lty = lty, lwd = lwd)
    # }

    x = unlist(lapply(seq_along(xleft), function(i) c(xleft[i], xleft[i], xright[i], xright[i], xleft[i], NA)))
    y = unlist(lapply(seq_along(ybottom), function(i) c(ybottom[i], ytop[i], ytop[i], ybottom[i], ybottom[i], NA)))
    x = x[-length(x)]
    y = y[-length(y)]
    circos.polygon(x, y, sector.index = sector.index, track.index = track.index, ...)

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
# -... pass to `graphics::polygon`
#
# == details
# similar as `graphics::polygon`.
#
# Note: start point should overlap with the end point,
#
circos.polygon = function(x, y, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), ...) {

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.polygon' can only be used after the plotting region been created.")
    }

    # whether the points that are out of the plotting region.
    check.points.position(x, y, sector.index, track.index)

    d = lines.expand(x, y, sector.index, track.index)
    d2 = circlize(d, sector.index = sector.index, track.index = track.index)
    polygon(polar2Cartesian(d2), ...)
    return(invisible(NULL))
}

# == title
# Draw segments through pairwise of points
#
# == param
# -x0 x coordinates for starting points
# -y0 y coordinates for ending points
# -x1 x coordinates for starting points
# -y1 y coordinates for ending points
# -sector.index Index for the sector
# -track.index  Index for the track
# -straight whether the segment is a straight line
# -col color of the segments
# -lwd line width of the segments
# -lty line type of the segments
# -... pass to `graphics::lines`
#
circos.segments = function(x0, y0, x1, y1, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), straight = FALSE,
	col = par("col"), lwd = par("lwd"), lty = par("lty"), ...) {

	n1 = length(x0)
    n2 = length(y0)
    n3 = length(x1)
    n4 = length(y1)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) x0 = rep(x0, n)
    if(n2 == 1) y0 = rep(y0, n)
    if(n3 == 1) x1 = rep(x1, n)
    if(n4 == 1) y1 = rep(y1, n)

	if(! (length(x0) == length(y0) && length(y0) == length(x1) && length(x1) == length(y1)) ) {
		stop_wrap("x0, y0, x1, y1 should have same length.")
	}

	if(length(col) == 1 && length(lwd) ==1 && length(lty) == 1) {

	} else {
		np = length(x0)
		if(length(straight) == 1) straight = rep(straight, np)
		if(length(col) == 1) col = rep(col, np)
		if(length(lwd) == 1) lwd = rep(lwd, np)
		if(length(lty) == 1) lty = rep(lty, np)

		for(i in seq_len(np)) {
			circos.lines(c(x0[i], x1[i]), c(y0[i], y1[i]), sector.index = sector.index,
				straight = straight[i], track.index = track.index, col = col[i],
				lwd = lwd[i], lty = lty[i], ...)
		}
		return(invisible(NULL))
	}


	if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.polygon' can only be used after the plotting region been created.")
    }

	np = length(x0)
	if(length(straight) == 1) straight = rep(straight, np)
	if(length(col) == 1) col = rep(col, np)
	if(length(lwd) == 1) lwd = rep(lwd, np)
	if(length(lty) == 1) lty = rep(lty, np)
	x = NULL
	y = NULL

	col2 = NULL
	lwd2 = NULL
	lty2 = NULL
	for(i in seq_along(x0)) {
		if(straight[i]) {
			x = c(x, c(x0[i], x1[i], NA))
			y = c(y, c(y0[i], y1[i], NA))
			col2 = c(col2, col[i])
			lwd2 = c(lwd2, lwd[i])
			lty2 = c(lty2, lty[i])
		} else {
			d = lines.expand(c(x0[i], x1[i]), c(y0[i], y1[i]), sector.index, track.index)
			x = c(x, c(d[, 1], NA))
			y = c(y, c(d[, 2], NA))
			nd = nrow(d)
			col2 = c(col2, rep(col[i], nd))
			lwd2 = c(lwd2, rep(lwd[i], nd))
			lty2 = c(lty2, rep(lty[i], nd))
		}
	}

	n2 = length(x)
	x = x[-n2]
	y = y[-n2]
	col2 = col2[-n2]
	lwd2 = lwd2[-n2]
	lty2 = lty2[-n2]
	d2 = circlize(x, y, sector.index = sector.index, track.index = track.index)
	d3 = polar2Cartesian(d2)
	lines(d3[,1], d3[,2], col = col2, lwd = lwd2, lty = lty2, ...)
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
# -direction    deprecated, use ``facing`` instead.
# -facing       Facing of text. Please refer to vignette for different settings
# -niceFacing   Should the facing of text be adjusted to fit human eyes?
# -adj          offset for text. By default the text position adjustment is either horizontal or vertical
#           in the canvas coordinate system. The "circular horizontal" offset can be set as a value in degree
#           unit and the value should be wrapped by `degree`.
# -...       Pass to `graphics::text`
# -cex          Font size
# -col          Font color
# -font         Font style
#
# == details
# The function is similar to `graphics::text`. All you need to note is the ``facing`` settings.
#
# == seealso
# http://jokergoo.github.io/circlize_book/book/graphics.html#text
circos.text = function(x, y, labels, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), direction = NULL,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
	"downward", "bending", "bending.inside", "bending.outside"), niceFacing = FALSE,
	adj = par("adj"), cex = 1, col = par("col"), font = par("font"), ...) {

    len_x = length(x)
    len_y = length(y)
    if(len_x == 1) x = rep(x, len_y)
    if(len_y == 1) y = rep(y, len_x)

	if(length(x) != length(y)) {
		stop_wrap("Length of x and y differ.")
	}

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.text' can only be used after the plotting region been created.")
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

	labels = as.vector(labels)

    ## check direction or facing
    if(!is.null(direction)) {
        warning_wrap("`direction` is deprecated, please use `facing` instead.")
        facing = switch(direction[1],
                        default = "inside",
                        default2 = "outside",
                        vertical_left = "reverse.clockwise",
                        vertical_right = "clockwise",
                        horizontal = "downward",
                        arc = "bending.inside")
        if(is.null(facing)) {
            stop_wrap("Wrong `direction` value, please use `facing` instead.")
        }
    }

    facing = match.arg(facing)[1]
    if(facing == "bending") {
    	facing = "bending.inside"
    }

    d = circlize(x, y, sector.index = sector.index, track.index = track.index)

    # adjust positions by `adj`
    if(inherits(adj, "list")) {
	    labels_width = strwidth(labels, cex = cex, font = font)
	    labels_height = strheight(labels, cex = cex, font = font)
	    if(facing == "clockwise") {
	    	rou_offset = -(adj[[1]] - 0.5) * labels_width
	    	#theta_offset = as.degree(asin(-(adj[2] - 0.5)*labels_height/2/d[, "rou"]))
	    	theta_offset = adj[[2]]
	    	if(!inherits(theta_offset, "degree")) stop_wrap("The second item in `adj` should be wrapped by `degree()` if facing is clockwise.")
	    } else if(facing == "reverse.clockwise") {
	    	rou_offset = (adj[[1]] - 0.5) * labels_width
	    	#theta_offset = as.degree(asin((adj[2] - 0.5)*labels_height/2/d[, "rou"]))
	    	theta_offset = adj[[2]]
	    	if(!inherits(theta_offset, "degree")) stop_wrap("The second item in `adj` should be wrapped by `degree()` if facing is reverse clockwise.")
	    } else if(facing == "inside") {
	    	rou_offset = -(adj[[2]] - 0.5) * labels_height
	    	#theta_offset = as.degree(asin(-(adj[1] - 0.5)*labels_width/2/d[, "rou"]))
	    	theta_offset = adj[[1]]
	    	if(!inherits(theta_offset, "degree")) stop_wrap("The first item in `adj` should be wrapped by `degree()` if facing is inside.")
	    } else if(facing == "outside") {
	    	rou_offset = (adj[[2]] - 0.5) * labels_height
	    	#theta_offset = as.degree(asin((adj[1] - 0.5)*labels_width/2/d[, "rou"]))
	    	theta_offset = adj[[1]]
	    	if(!inherits(theta_offset, "degree")) stop_wrap("The first item in `adj` should be wrapped by `degree()` if facing is outside.")
	    }

	    if(facing %in% c("clockwise", "reverse.clockwise", "inside", "outside")) {
			d2 = d
			d2[, "rou"] = d[, "rou"] + rou_offset
			d2[, "theta"] = d[, "theta"] + theta_offset
			dd = reverse.circlize(d2, sector.index = sector.index, track.index = track.index)
			x = dd[, 1]
			y = dd[, 2]
			#circos.points(x, y)
			adj = c(0.5, 0.5)
			d = circlize(x, y, sector.index = sector.index, track.index = track.index)
	    }
	}

    # whether the points that are out of the plotting region.
    check.points.position(x, y, sector.index, track.index)

	if(niceFacing && facing %in% c("clockwise", "reverse.clockwise", "inside", "outside", "bending.inside", "bending.outside")) {
		if(facing %in% c("clockwise", "reverse.clockwise")) {
			degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
			degree = degree %% 360
			l1 = degree >= 90 & degree < 270  # should be reverse.clockwise
			l2 = !l1  # should be clockwise
			if(facing == "reverse.clockwise") {
				adj1 = adj
				adj2 = 1 - adj
				facing1 = "reverse.clockwise"
				facing2 = "clockwise"
			} else {
				adj1 = 1- adj
				adj2 = adj
				facing1 = "reverse.clockwise"
				facing2 = "clockwise"
			}
		} else if(facing %in% c("inside", "outside", "bending.inside", "bending.outside")) {
			degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
			degree = degree %% 360
			l1 = degree > 0 & degree < 180  # should be inside
			l2 = !l1   # should be outside
			if(facing == "inside") {
				adj1 = adj
				adj2 = 1 - adj
				facing1 = "inside"
				facing2 = "outside"
			} else if(facing == "outside") {
				adj1 = 1 - adj
				adj2 = adj
				facing1 = "inside"
				facing2 = "outside"
			} else if(facing == "bending.inside") {
				adj1 = adj
				adj2 = 1 - adj
				facing1 = "bending.inside"
				facing2 = "bending.outside"
			} else if(facing == "bending.outside") {
				adj1 = 1 - adj
				adj2 = adj
				facing1 = "bending.inside"
				facing2 = "bending.outside"
			}
		}
		if(sum(l1)) {
			circos.text(x[l1], y[l1], labels[l1], sector.index = sector.index,
				track.index = track.index, facing = facing1, niceFacing = FALSE, adj = adj1,
				cex = cex[l1], col = col[l1], font = font[l1], ...)
		}

		if(sum(l2)) {
			circos.text(x[l2], y[l2], labels[l2], sector.index = sector.index,
				track.index = track.index, facing = facing2, niceFacing = FALSE, adj = adj2,
				cex = cex[l2], col = col[l2], font = font[l2], ...)
		}
		return(invisible(NULL))
	}

	if(grepl("bending", facing)) {

		chars = strsplit(labels, "")
		if(facing == "bending.outside") {
     		chars = lapply(chars, rev)
		}

    	nlabel = length(labels)
		strw = lapply(chars, strwidth, cex = cex, font = font)
		strh = lapply(chars, strheight, cex = cex, font = font)

		if(facing == "bending.outside") {
			adj = 1 - adj
		}

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
			dr = reverse.circlize(theta, rep(rou, length(theta)), sector.index = sector.index, track.index = track.index)

			if(facing == "bending.inside") {
				circos.text(dr[, 1], dr[, 2], labels = chars[[i]], sector.index = sector.index, track.index = track.index, cex = cex[i], col = col[i], font = font[i], facing = "inside", adj = c(0.5, 0), ...)
			} else if(facing == "bending.outside") {
				circos.text(dr[, 1], dr[, 2], labels = chars[[i]], sector.index = sector.index, track.index = track.index, cex = cex[i], col = col[i], font = font[i], facing = "outside", adj = c(0.5, 1), ...)
			}
			#circos.points(dr[, 1], dr[, 2], pch = 16, cex = 0.8)
		}

	} else {

        srt = d[,1]-90    #srt = ifelse(srt > 0, srt, 360 + srt)

        if(facing == "reverse.clockwise") {           # pointing to the circle center, but facing left at 90 degree
            srt = srt - 90
        } else if(facing == "clockwise") {   # pointing to the circle center, but facing right at 90 degree
            srt = srt + 90
        } else if(facing == "downward") {       # horizontal at the finnal graph
            srt = rep(0, length(srt))
        } else if(facing == "outside") {
			srt = srt + 180
		}

		m = polar2Cartesian(d)

		for(i in seq_along(x)) {
			text(m[i, 1], m[i, 2], labels = labels[i], srt = srt[i],
				 cex = cex[i], col = col[i], font = font[i], adj = adj, ...)
		}
    }

    return(invisible(NULL))
}

# == title
# Convert fontsize to cex
#
# == param
# -x value for fontsize
#
fontsize = function(x) {
	x/par("ps")
}

# == title
# Mark the value as a degree value
#
# == param
# -x degree value
#
# == value
# a ``degree`` object
#
degree = function(x) {
	class(x) = "degree"
	list(x)
}

# == title
# Draw text in cells among the whole track
#
# == param
# -factors      A `factor` or a character vector which represents the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -labels       Labels
# -track.index  Index for the track
# -direction    deprecated, use ``facing`` instead.
# -facing       Facing of text
# -niceFacing   Should the facing of text be adjusted to fit human eyes?
# -adj          Adjustment for text
# -cex          Font size
# -col          Font color
# -font         Font style
#
# == details
# The function adds texts in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add texts in cells by calling `circos.text`.
#
# This function can be replaced by a ``for`` loop containing `circos.text`.
circos.trackText = function(factors, x, y, labels, track.index = get.cell.meta.data("track.index"),
    direction = NULL, facing = c("inside", "outside", "reverse.clockwise", "clockwise",
	"downward", "bending", "bending.inside", "bending.outside"), niceFacing = FALSE,
    adj = par("adj"), cex = 1, col = par("col"), font = par("font")) {

    # basic check here
    if(length(x) != length(factors) || length(y) != length(factors)) {
        stop_wrap("Length of data and length of factors differ.\n")
    }

    if(!is.factor(factors)) {
        factors = factor(factors)
    }

    # check whether there are some categories that are not in the circle
	setdiff.factors = setdiff(levels(factors), get.all.sector.index())
    if(length(setdiff.factors)) {
        stop_wrap("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".")
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
                      direction = direction, facing = facing, niceFacing = niceFacing,
					  adj = adj, cex = ncex, col = ncol, font = nfont)

    }
    return(invisible(NULL))
}

# == title
# Draw x-axis
#
# == param
# -h                Position of the x-axis, can be "top", "bottom" or a numeric value
# -major.at         If it is numeric vector, it identifies the positions
#                   of the major ticks. It can exceed ``xlim`` value and the exceeding part
#                   would be trimmed automatically. If it is ``NULL``, about every 10 degrees there is a major tick.
# -labels           labels of the major ticks. Also, the exceeding part would be trimmed automatically.
#                   The value can also be logical (either an atomic value or a vector) which represents
#                   which labels to show.
# -major.tick       Whether to draw major tick. If it is set to ``FALSE``, there would be
#                   no minor ticks.
# -sector.index     Index for the sector
# -track.index      Index for the track
# -labels.font      font style for the axis labels
# -labels.cex       font size for the axis labels
# -labels.direction deprecated, use ``facing`` instead.
# -labels.facing    facing of labels on axis, passing to `circos.text`
# -labels.niceFacing Should facing of axis labels be human-easy
# -direction        whether the axis ticks point to the outside or inside of the circle.
# -minor.ticks      Number of minor ticks between two close major ticks.
# -major.tick.percentage not used. Length of the major ticks. It is the percentage to the height of the cell.
# -labels.away.percentage not used. The distance for the axis labels to the major ticks. It is the percentage to the height of the cell.
# -major.tick.length length of the major ticks, measured in "current" data coordinate. `convert_y` can be
#                   used to convert an absolute unit to the data coordinate.
# -lwd              line width for ticks
# -col              color for the axes
# -labels.col       color for the labels
# -labels.pos.adjust  whether to adjust the positions of the first label and the last label. The value can be a vector
#                    of length two which correspond to the first label and the last label.
#
# == details
# It can only draw axes on x-direction.
#
# == seealso
# `circos.yaxis` draws axes on y-direction.
circos.axis = function(h = "top", major.at = NULL, labels = TRUE, major.tick = TRUE,
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"),
	labels.font = par("font"), labels.cex = par("cex"),
	labels.facing = "inside", labels.direction = NULL, labels.niceFacing = TRUE,
	direction = c("outside", "inside"), minor.ticks = 4,
	major.tick.percentage = 0.1, labels.away.percentage = major.tick.percentage/2,
	major.tick.length = convert_y(1, "mm", sector.index, track.index),
	lwd = par("lwd"), col = par("col"), labels.col = par("col"), labels.pos.adjust = TRUE) {

    if(!is.null(labels.direction)) {
        labels.facing = switch(labels.direction[1],
                        default = "inside",
                        default2 = "outside",
                        vertical_left = "reverse.clockwise",
                        vertical_right = "clockwise",
                        horizontal = "downward",
                        arc = "bending")
        warning_wrap("`labels.direction` is deprecated, please use `labels.facing` instead.")
    }

	direction = direction[1]
	if(! direction %in% c("outside", "inside")) {
		stop_wrap("Direction should be in 'outside' and 'inside'.")
	}

	xlim = get.cell.meta.data("xlim", sector.index, track.index)

	sector.data = get.sector.data(sector.index)

	if(h == "top") {
		h = get.cell.meta.data("cell.ylim", sector.index, track.index)[2]
	} else if(h == "bottom") {
		h = get.cell.meta.data("cell.ylim", sector.index, track.index)[1]
	}

	if(is.null(major.at)) {
		major.by = .default.major.by(sector.index, track.index)
		major.at = seq(floor(xlim[1]/major.by)*major.by, xlim[2], by = major.by)
		major.at = c(major.at, major.at[length(major.at)] + major.by)
	}

	minor.at = NULL
	if(minor.ticks != 0) {
		for(i in seq_along(major.at)) {
			if(i == 1) next
			k = seq_len(minor.ticks) / (minor.ticks + 1)
			minor.at = c(minor.at, k * (major.at[i] - major.at[i - 1]) + major.at[i - 1])
		}
	}

	xlim2 = xlim
	circos.lines(c(ifelse(major.at[1] >= xlim2[1], major.at[1], xlim2[1]),
	               ifelse(major.at[length(major.at)] <= xlim2[2], major.at[length(major.at)], xlim2[2])),
				 c(h, h), sector.index = sector.index, track.index = track.index, lwd = lwd, col = col)

	# ticks
	yrange = get.cell.meta.data("yrange", sector.index, track.index)
	# major.tick.length = yrange * major.tick.percentage
	# major.tick.length = convert_y(2, "mm", sector.index, track.index)

	op = circos.par("points.overflow.warning")
	circos.par("points.overflow.warning" = FALSE)
	l = major.at >= xlim2[1] & major.at <= xlim2[2]
	if(major.tick) {
		circos.segments(major.at[l], rep(h, sum(l)), major.at[l], rep(h, sum(l)) + major.tick.length*ifelse(direction == "outside", 1, -1), straight = TRUE,
			             sector.index = sector.index, track.index = track.index, lwd = lwd, col = col)
	}
	#for(i in seq_along(major.at)) {

		# if(major.at[i] < xlim2[1] || major.at[i] > xlim2[2]) {
		# 	next
		# }

		# if(major.tick) {
		# 	circos.lines(c(major.at[i], major.at[i]), c(h, h + major.tick.length*ifelse(direction == "outside", 1, -1)), straight = TRUE,
		# 	             sector.index = sector.index, track.index = track.index, lwd = lwd)
		# }

	labels.adj = NULL
	if(direction == "outside") {
		if(labels.facing == "inside") {
			labels.adj = c(0.5, 0)
		} else if(labels.facing == "outside") {
			labels.adj = c(0.5, 1)
		} else if(labels.facing == "reverse.clockwise") {
			labels.adj = c(1, 0.5)
		} else if(labels.facing == "clockwise") {
			labels.adj = c(0, 0.5)
		} else if(labels.facing == "downward") {
			labels.adj = c(0.5, 0.5)
		} else {
			labels.adj = c(0.5, 0)
		}
	} else {
		if(labels.facing == "inside") {
			labels.adj = c(0.5, 1)
		} else if(labels.facing == "outside") {
			labels.adj = c(0.5, 0)
		} else if(labels.facing == "reverse.clockwise") {
			labels.adj = c(0, 0.5)
		} else if(labels.facing == "clockwise") {
			labels.adj = c(1, 0.5)
		} else if(labels.facing == "downward") {
			labels.adj = c(0.5, 0.5)
		} else {
			labels.adj = c(0.5, 1)
		}
	}

	add_axis_labels = function(x, y, labels, h, col, labels.pos.adjust, ...) {
		arg_list = list(...)

		n = length(x)
		first_label_width = convert_x(strwidth(labels[1], units = "inches", cex = arg_list$cex), "inches", arg_list$sector.index, arg_list$track.index, h = h)
        first_label_height = convert_x(strheight(labels[1], units = "inches", cex = arg_list$cex), "inches", arg_list$sector.index, arg_list$track.index, h = h)
            
        if(n >= 1) {
			last_label_width = convert_x(strwidth(labels[n], units = "inches", cex = arg_list$cex), "inches", arg_list$sector.index, arg_list$track.index, h = h)
			last_label_height = convert_x(strheight(labels[n], units = "inches", cex = arg_list$cex), "inches", arg_list$sector.index, arg_list$track.index, h = h)
		} else {
            last_label_width = 0
            last_label_height = 0
        }

		if(labels.facing == "inside") {
			offset.first = first_label_width/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_width/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		} else if(labels.facing == "outside") {
			offset.first = first_label_width/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_width/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		} else if(labels.facing == "reverse.clockwise") {
			offset.first = first_label_height/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_height/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		} else if(labels.facing == "clockwise") {
			offset.first = first_label_height/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_height/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		} else if(labels.facing == "downward") {
			offset.first = first_label_width/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_width/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		} else {
			offset.first = first_label_width/2 - (x[1] - get.cell.meta.data("cell.xlim", sector.index, track.index)[1])
			offset.last = last_label_width/2 - abs(x[n] - get.cell.meta.data("cell.xlim", sector.index, track.index)[2])
		}

		if(length(labels.pos.adjust) == 1) labels.pos.adjust = rep(labels.pos.adjust, 2)
		if(!labels.pos.adjust[1]) {
			offset.first = 0
		}
		if(!labels.pos.adjust[2]) {
			offset.last = 0
		}

		if(n == 1) {
			circos.text(x + ifelse(offset.first > 0, offset.first, 0), y, labels, col = col, ...)
		} else if(n == 2) {
			circos.text(x[1] + ifelse(offset.first > 0, offset.first, 0), y[1], labels[1], col = col, ...)
			circos.text(x[2] - ifelse(offset.last > 0, offset.last, 0), y[2], labels[2], col = col, ...)
		} else if(n > 2) {
			circos.text(x[1] + ifelse(offset.first > 0, offset.first, 0), y[1], labels[1], col = col, ...)
			circos.text(x[2:(n-1)], y[2:(n-1)], labels[2:(n-1)], col = col, ...)
			circos.text(x[n] - ifelse(offset.last > 0, offset.last, 0), y[n], labels[n], col = col, ...)
		}

		# circos.text(x, y, labels, ...)
	}

	if(is.logical(labels) && labels) {
		add_axis_labels(major.at[l], rep(h, sum(l)) + (major.tick.length + convert_y(0.5, "mm", sector.index, track.index))*ifelse(direction == "outside", 1, -1),
		           labels = major.at[l], adj = labels.adj,
		           font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
		           facing = labels.facing, niceFacing = labels.niceFacing, h = h, col = labels.col,
		           labels.pos.adjust = labels.pos.adjust)
	} else if(is.logical(labels) && !labels) {

    } else if(length(labels)) {
		add_axis_labels(major.at[l], rep(h, sum(l)) + (major.tick.length + convert_y(0.5, "mm", sector.index, track.index))*ifelse(direction == "outside", 1, -1),
		            labels = labels[l], adj = labels.adj,
		            font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
			        facing = labels.facing, niceFacing = labels.niceFacing, h = h, col = labels.col,
			        labels.pos.adjust = labels.pos.adjust)
	}

	#}
	if(major.tick) {
		# for(i in seq_along(minor.at)) {
		# 	if(minor.at[i] < xlim2[1] || minor.at[i] > xlim2[2]) {
		# 		next
		# 	}

		# 	circos.lines(c(minor.at[i], minor.at[i]), c(h, h + major.tick.length/2*ifelse(direction == "outside", 1, -1)), straight = TRUE,
		# 	             sector.index = sector.index, track.index = track.index, lwd = lwd)
		# }

		l = minor.at >= xlim2[1] & minor.at <= xlim2[2]
		circos.segments(minor.at[l], rep(h, sum(l)), minor.at[l], rep(h, sum(l)) + major.tick.length/2*ifelse(direction == "outside", 1, -1), straight = TRUE,
			sector.index = sector.index, track.index = track.index, lwd = lwd, col = col)
	}

	circos.par("points.overflow.warning" = op)
	return(invisible(NULL))
}

# == title
# Draw x-axis
#
# == param
# -... all pass to `circos.axis`
#
circos.xaxis = function(...) {
	circos.axis(...)
}

.default.major.by = function(sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")) {
	# start.degree - end.degree is always a positive value.
	d = circos.par("major.by.degree")
	cell.start.degre = get.cell.meta.data("cell.start.degree", sector.index, track.index)
	tm = reverse.circlize(c(cell.start.degre, cell.start.degre-d), rep(get.cell.meta.data("cell.bottom.radius", sector.index = sector.index, track.index = track.index), 2))
	major.by = abs(tm[1, 1] - tm[2, 1])
	digits = as.numeric(gsub("^.*e([+-]\\d+)$", "\\1", sprintf("%e", major.by)))
	major.by = round(major.by, digits = -1*digits)
	return(major.by)
}

# == title
# Draw y-axis
#
# == param
# -side add the y-axis on the left or right of the cell
# -at         If it is numeric vector, it identifies the positions
#                   of the ticks. It can exceed ``ylim`` value and the exceeding part
#                   would be trimmed automatically.
# -labels           labels of the ticks. The exceeding part would be trimmed automatically.
#                   The value can also be logical (either an atomic value or a vector) which represents
#                   which labels to show.
# -tick       Whether to draw ticks.
# -sector.index     Index for the sector
# -track.index      Index for the track
# -labels.font      font style for the axis labels
# -labels.cex       font size for the axis labels
# -labels.niceFacing Should facing of axis labels be human-easy
# -tick.length      length of the tick
# -lwd              line width for ticks
# -col              color for the axes
# -labels.col       color for the labels
#
# == details
# Note, you need to set the gap between sectors manually by `circos.par` to make sure there is enough space
# for y-axis.
#
circos.yaxis = function(side = c("left", "right"), at = NULL, labels = TRUE, tick = TRUE,
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"),
	labels.font = par("font"), labels.cex = par("cex"),
	labels.niceFacing = TRUE,
	tick.length = convert_x(1, "mm", sector.index, track.index),
	lwd = par("lwd"), col = par("col"), labels.col = par("col")) {

	ylim = get.cell.meta.data("ylim", sector.index, track.index)

	side = match.arg(side)[1]
	if(side == "left") {
		v = get.cell.meta.data("cell.xlim", sector.index, track.index)[1]
	} else if(side == "right") {
		v = get.cell.meta.data("cell.xlim", sector.index, track.index)[2]
	}

	if(is.null(at)) {
		at = grid.pretty(ylim)
		labels = at
	}

	ylim2 = ylim

	circos.lines(rep(v, 2),
		get.cell.meta.data("cell.ylim", sector.index, track.index),
		sector.index = sector.index, track.index = track.index, lwd = lwd, col = col)

	# ticks
	yrange = get.cell.meta.data("yrange", sector.index, track.index)
	xrange = get.cell.meta.data("xrange", sector.index, track.index)
	# tick.length = tick.length/abs(get.cell.meta.data("cell.start.degree", sector.index, track.index) - get.cell.meta.data("cell.end.degree", sector.index, track.index)) * xrange
	# tick.length = convert_x(2, "mm", sector.index, track.index)

	op = circos.par("points.overflow.warning")
	circos.par("points.overflow.warning" = FALSE)
	l = at >= ylim2[1] & at <= ylim2[2]
	if(tick) {
		circos.segments(rep(v, sum(l)), at[l], rep(v, sum(l)) + tick.length*ifelse(side == "right", 1, -1), at[l], straight = TRUE,
			             sector.index = sector.index, track.index = track.index, lwd = lwd, col = col)
	}

	labels.adj = NULL
	if(side == "left") {
		labels.adj = c(1, 0.5)
	} else {
		labels.adj = c(0, 0.5)
	}

	if(is.logical(labels) && labels) {
		circos.text(rep(v, sum(l)) + (tick.length + convert_x(0.5, "mm", sector.index, track.index))*ifelse(side == "right", 1, -1), at[l],
		           labels = at[l], adj = labels.adj,
		           font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
		           facing = "inside", niceFacing = labels.niceFacing, col = labels.col)
	} else if(is.logical(labels) && !labels) {

    } else if(length(labels)) {
		circos.text(rep(v, sum(l)) + (tick.length + convert_x(0.5, "mm", sector.index, track.index))*ifelse(side == "right", 1, -1), at[l],
		            labels = labels[l], adj = labels.adj,
		            font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
			        facing = "inside", niceFacing = labels.niceFacing, col = labels.col)
	}

	circos.par("points.overflow.warning" = op)
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
# -track.index  Index for the track which is going to be updated. Setting it to ``NULL`` means
#               creating the plotting regions in the next newest track.
# -track.height Height of the track. It is the percentage to the radius of the unit circle.
#               If to update a track, this argument is disabled.
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``. Btw, ``ylim`` is calculated automatically.
# -col          Filled color for histogram
# -border       Border color for histogram
# -lty          Line style for histogram
# -lwd          Line width for histogram
# -bg.col       Background color for the plotting regions
# -bg.border    Color for the border of the plotting regions
# -bg.lty       Line style for the border of the plotting regions
# -bg.lwd       Line width for the border of the plotting regions
# -breaks       see `graphics::hist`
# -include.lowest see `graphics::hist`
# -right          see `graphics::hist`
# -draw.density   whether draw density lines instead of histogram bars.
# -area           whether to fill the area below the density lines. If it is set to ``TRUE``, ``col`` controls the filled color in the area and ``border`` controls color of the line.
# -bin.size size of the bins of the histogram
#
# == details
# It draw histogram in cells among a whole track. It is also an example to show how to add self-defined
# high-level graphics by this package.
circos.trackHist = function(factors, x, track.height = circos.par("track.height"),
    track.index = NULL, force.ylim = TRUE, col = ifelse(draw.density, "black", NA),
	border = "black", lty = par("lty"), lwd = par("lwd"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE,
    bin.size = NULL, area = FALSE) {

    # basic check here
    if(length(x) != length(factors)) {
        stop_wrap("Length of data and length of factors differ.")
    }

    if(!is.factor(factors)) {
        factors = factor(factors)
    }

	# check whether there are some categories that are not in the circle
	setdiff.factors = setdiff(levels(factors), get.all.sector.index())
    if(length(setdiff.factors)) {
        stop_wrap("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".")
    }

    # calculate the distributions
    le = levels(factors)

    xx = NULL
    yy = NULL
    fa = NULL

    for(i in seq_along(le)) {
        l = factors == le[i]
        nx = x[l]

        if(!is.null(bin.size)) {
        	breaks = seq(min(nx), max(nx), by = bin.size)
        	if(breaks[length(breaks)] < max(nx)) {
	        	breaks = c(breaks, breaks[length(breaks)] + bin.size)
	        }
        }

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
		xlim = get.cell.meta.data("xlim", sector.index = le[i], track.index = track.index)
		l = fa == le[i]
		l2 = xx[l] >= xlim[1] & xx[l] <= xlim[2]
		l3 = c(l3, l2)
	}

	xx = xx[l3]
	yy = yy[l3]
	fa = fa[l3]

    if(draw.density) {
        circos.trackLines(factors = fa, xx, yy, track.index = track.index,
                          col = col, lty = lty, lwd = lwd, area = area, border = border)
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

            cell.xlim = get.cell.meta.data("cell.xlim", le[i], track.index)
            nx[nx < cell.xlim[1]] = cell.xlim[1]
            nx[nx > cell.xlim[2]] = cell.xlim[2]

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
# Draw sectors or rings in a circle
#
# == param
# -start.degree   start degree for the sector
# -end.degree     end degree for the sector
# -rou1           Radius for one of the arc in the sector
# -rou2           Radius for the other arc in the sector
# -center         Center of the circle
# -clock.wise     The direction from ``start.degree`` to ``end.degree``
# -col            Filled color
# -border         Border color
# -lwd            Line width
# -lty            Line style
#
# == details
# If the interval between ``start`` and ``end`` (larger or equal to 360 or smaller or equal to -360)
# it would draw a full circle or ring. If ``rou2`` is set, it would draw part of a ring.
#
draw.sector = function(start.degree = 0, end.degree = 360, rou1 = 1, rou2 = NULL,
	center = c(0, 0), clock.wise = TRUE, col = NA, border = "black", lwd = par("lwd"),
	lty = par("lty")) {

	is.circular = function(start.degree, end.degree) {
		(end.degree - start.degree) %% 360 == 0 && (end.degree - start.degree) != 0
	}

	degree_diff = function(start, end, clock.wise = TRUE) {
		if(is.circular(start, end)) {
			360
		} else {
			start = start %% 360
			end = end %% 360
			if(clock.wise) (start - end) %% 360
			else (end - start) %% 360
		}
	}

	# from start to end
	degree_seq = function(start, end, clock.wise = TRUE, ...) {
		if(is.circular(start, end)) {
			seq(0, 360, ...)
		} else {
			start = start %% 360
			end = end %% 360
			if(clock.wise) {
				# make start is larger than end, but the difference is less than 360
				if(start < end) start = start + 360
				seq(start, end, ...)
			} else {
				if(start > end) start = start - 360
				seq(start, end, ...)
			}
		}
	}

	d1 = NULL

	# calculate the number of segments of the up arc
	l1 = as.radian(degree_diff(start.degree, end.degree, clock.wise)) * rou1
	ncut1 = l1/ (2*pi/circos.par("unit.circle.segments"))
    ncut1 = floor(ncut1)
	ncut1 = ifelse(ncut1 < 2, 2, ncut1)

	# d1 is from the start.degree to end.degree
	d1 = rbind(d1, cbind(degree_seq(start.degree, end.degree, clock.wise, length.out = ncut1), rep(rou1, ncut1)))

	# d2 is from end.degree to start.degree
	d2 = NULL
	if(!is.null(rou2)) {
		# calculate the number of segments of the bottom arc
		l2 = as.radian(degree_diff(start.degree, end.degree, clock.wise)) * rou2
		ncut2 = l2/ (2*pi/circos.par("unit.circle.segments"))
		ncut2 = floor(ncut2)
		ncut2 = ifelse(ncut2 < 2, 2, ncut2)

		d2 = rbind(d2, cbind(degree_seq(end.degree, start.degree, !clock.wise, length.out = ncut2), rep(rou2, ncut2)))
	}

	if(is.null(rou2)) {
		m1 = polar2Cartesian(d1)
		if(is.circular(start.degree, end.degree)) {  # it is a circle
			m = m1
		} else {
			m = rbind(m1, c(0, 0))
		}

		# and shift to the center
		m[, 1] = m[, 1] + center[1]
		m[, 2] = m[, 2] + center[2]
		polygon(m, col = col, border = border, lwd = lwd, lty = lty)
	} else {
		m1 = polar2Cartesian(d1)
		m2 = polar2Cartesian(d2)

		if(is.circular(start.degree, end.degree)) {  # a ring
			m = rbind(m1, m2[rev(seq_len(nrow(m2))), ,drop = FALSE])

			m[, 1] = m[, 1] + center[1]
			m[, 2] = m[, 2] + center[2]
			polygon(m, col = col, border = NA, lwd = 0.1)
			# two borders
			#lines(m1[, 1]+center[1], m1[, 2]+center[2], col = "white", lwd = lwd, lty = 1)
			#lines(m2[, 1]+center[1], m2[, 2]+center[2], col = "white", lwd = lwd, lty = 1)
			lines(m1[, 1]+center[1], m1[, 2]+center[2], col = border, lwd = lwd, lty = lty)
			lines(m2[, 1]+center[1], m2[, 2]+center[2], col = border, lwd = lwd, lty = lty)

		} else {
			m = rbind(m1, m2)

			m[, 1] = m[, 1] + center[1]
			m[, 2] = m[, 2] + center[2]
			polygon(m, col = col, border = border, lwd = lwd, lty = lty)
		}

	}

    return(invisible(NULL))
}

# == title
# Highlight sectors and tracks
#
# == param
# -sector.index A vector of sector index
# -track.index A vector of track index that you want to highlight
# -col Color for highlighting. Note the color should be semi-transparent.
# -border Border of the highlighted region
# -lwd Width of borders
# -lty Style of borders
# -padding Padding for the highlighted region. It should contain four values
#          representing ratios of the width or height of the highlighted region
# -text text added in the highlight region, only support plotting one string at a time
# -text.vjust adjustment on 'vertical' (radical) direction. Besides to set it as numeric values,
#            the value can also be a string contain absoute unit, e.g. "2.1mm", "-1 inche", but only
#            "mm", "cm", "inches"/"inche" are allowed.
# -text.col color for the text
# -... pass to `circos.text`
#
# == details
# You can use `circos.info` to find out index for all sectors and all tracks.
#
# The function calls `draw.sector`.
highlight.sector = function(sector.index, track.index = get.all.track.index(),
	col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
	padding = c(0, 0, 0, 0), text = NULL, text.col = par("col"),
	text.vjust = 0.5, ...) {

	sectors = get.all.sector.index()
	if(!all(sector.index %in% sectors)) {
		stop_wrap("`chr` contains index that does not beling to available sectors.")
	}
	tracks = get.all.track.index()
	if(!all(track.index %in% tracks)) {
		stop_wrap("`track.index` contains index that does not belong to available tracks.")
	}

	y_offset = NULL
	if(!is.numeric(text.vjust)) {
		y_offset = parse_unit(text.vjust)
		text.vjust = 0.5
	}

	# if all sectors are selected
	if(length(setdiff(sectors, sector.index)) == 0) {
		track.index = sort(unique(track.index))
		ts = continuousIndexSegment(track.index)

		for(i in seq_along(ts)) {
			track.index.vector = ts[[i]]
			start.degree = 0
			end.degree = 360
			rou1 = get.cell.meta.data("cell.top.radius", sectors[1], track.index.vector[1])
			rou2 = get.cell.meta.data("cell.bottom.radius", sectors[1], track.index.vector[length(track.index.vector)])

			d2 = rou1 - rou2
			rou1 = rou1 + d2*padding[3]
			rou2 = rou2 - d2*padding[1]

			draw.sector(start.degree = start.degree, end.degree = end.degree, rou1 = rou1, rou2 = rou2, col = col, border = border, lwd = lwd, lty = lty)

			if(!is.null(text)) {
				# map to most recent cell
				pos = reverse.circlize((start.degree + end.degree)/2 + ifelse(start.degree < end.degree, 180, 0), (rou1 + rou2)/2)
				op_warning = circos.par("points.overflow.warning")
				circos.par("points.overflow.warning" = FALSE)
				if(is.null(y_offset)) {
        			circos.text(pos[1,1], pos[1,2], text, adj = c(0.5, text.vjust), col = text.col, ...)
        		} else {
        			circos.text(pos[1,1], pos[1,2] + uy(y_offset[[1]], y_offset[[2]]), text, adj = c(0.5, 0.5), col = text.col, ...)
        		}
        		circos.par(points.overflow.warning = op_warning)
			}
		}

	} else {

		sector.numeric.index = which(sectors %in% sector.index)
		ss = continuousIndexSegment(sector.numeric.index, n = length(sectors), loop = TRUE)

		track.index = sort(unique(track.index))
		ts = continuousIndexSegment(track.index)

		for(j in seq_along(ss)) {
			sector.index.vector = sectors[ ss[[j]] ]
			for(i in seq_along(ts)) {
				track.index.vector = ts[[i]]
				start.degree = get.cell.meta.data("cell.start.degree", sector.index.vector[1], track.index = 1)
				end.degree = get.cell.meta.data("cell.end.degree", sector.index.vector[length(sector.index.vector)], track.index = 1)
				rou1 = get.cell.meta.data("cell.top.radius", sector.index.vector[1], track.index.vector[1])
				rou2 = get.cell.meta.data("cell.bottom.radius", sector.index.vector[1], track.index.vector[length(track.index.vector)])

				d1 = end.degree - start.degree
				d2 = rou1 - rou2
				start.degree = start.degree - d1*padding[2]
				end.degree = end.degree + d1*padding[4]
				rou1 = rou1 + d2*padding[3]
				rou2 = rou2 - d2*padding[1]

				draw.sector(start.degree = start.degree, end.degree = end.degree, rou1 = rou1, rou2 = rou2, col = col, border = border, lwd = lwd, lty = lty)
				if(!is.null(text)) {
					# map to most recent cell
					pos = reverse.circlize((start.degree + end.degree)/2 + ifelse(start.degree < end.degree, 180, 0), (rou1 + rou2)/2)
					op_warning = circos.par("points.overflow.warning")
					circos.par("points.overflow.warning" = FALSE)
					if(is.null(y_offset)) {
	        			circos.text(pos[1,1], pos[1,2], text, adj = c(0.5, text.vjust), col = text.col, ...)
	        		} else {
	        			circos.text(pos[1,1], pos[1,2] + uy(y_offset[[1]], y_offset[[2]]), text, adj = c(0.5, 0.5), col = text.col, ...)
	        		}
	        		circos.par(points.overflow.warning = op_warning)
				}
			}
		}
	}
}

parse_unit = function(str) {
	if(grepl("^(-?\\d+(\\.\\d+)?)\\s*(mm|cm|inche|inches)$", str)) {
		m = regexpr("^(-?\\d+(\\.\\d+)?)", str)
		v = regmatches(str, m)
		m = regexpr("(mm|cm|inche|inches)$", str)
		u = regmatches(str, m)
		return(list(value = as.numeric(v), unit = u))
	} else {
		stop_wrap("Format of the unit is incorrect. It should be like '2mm', '-2.1 inches'.")
	}
}

# == title
# Add circular dendrograms
#
# == param
# -dend A `stats::dendrogram` object.
# -facing Is the dendromgrams facing inside to the circle or outside.
# -max_height Maximum height of the dendrogram. This is important if more than one dendrograms
#             are drawn in one track and making them comparable.
# -use_x_attr Whether use the ``x`` attribute to determine node positions in the dendrogram, used internally.
#
# == details
# Assuming there are ``n`` nodes in the dendrogram, the positions for leaves on x-axis is ``0.5, 1.5, ..., n - 0.5``.
# So you must be careful with ``xlim`` when you initialize the cirular layout.
#
# You can use the ``dendextend`` package to render the dendrograms.
#
circos.dendrogram = function(dend, facing = c("outside", "inside"), max_height = NULL, use_x_attr = FALSE) {

	facing = match.arg(facing)[1]

	if(is.null(max_height)) {
		max_height = attr(dend, "height")
	}

    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }
	
    use_x_attr = use_x_attr

    lines_par = function(col = par("col"), lty = par("lty"), lwd = par("lwd"), ...) {
    	return(list(col = col, lty = lty, lwd = lwd))
    }

    points_par = function(col = par("col"), pch = par("pch"), cex = par("cex"), ...) {
    	return(list(col = col, pch = pch, cex = cex))
    }

    draw.d = function(dend, max_height, facing = "outside", max_width = 0) {
        leaf = attr(dend, "leaf")
        d1 = dend[[1]]  # child tree 1
        d2 = dend[[2]]  # child tree 2
        height = attr(dend, "height")
        midpoint = attr(dend, "midpoint")
	
        if(use_x_attr) {
            x1 = attr(d1, "x")	
        } else {
            if(is.leaf(d1)) {
                x1 = x[as.character(attr(d1, "label"))]
            } else {
                x1 = attr(d1, "midpoint") + x[as.character(labels(d1))[1]]
            }
        }
        y1 = attr(d1, "height")

        if(use_x_attr) {
            x2 = attr(d2, "x")	
        } else {
            if(is.leaf(d2)) {
                x2 = x[as.character(attr(d2, "label"))]
            } else {
                x2 = attr(d2, "midpoint") + x[as.character(labels(d2))[1]]
            }
        }
        y2 = attr(d2, "height")

        # graphic parameter for current branch
        # only for lines, there are lwd, col, lty
        edge_par1 = do.call("lines_par", as.list(attr(d1, "edgePar")))  # as.list to convert NULL to list()
        edge_par2 = do.call("lines_par", as.list(attr(d2, "edgePar")))
        node_par = attr(dend, "nodePar")
        if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend, "nodePar")))

        # plot the connection line
        if(facing == "outside") {
        	circos.lines(c(x1, x1), max_height - c(y1, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd, straight = TRUE)
        	circos.lines(c(x1, (x1+x2)/2), max_height - c(height, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd)
       		circos.lines(c(x2, x2), max_height - c(y2, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd, straight = TRUE)
       		circos.lines(c(x2, (x1+x2)/2), max_height - c(height, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd)
       		if(!is.null(node_par)) {
       			circos.points((x1+x2)/2, max_height - height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
       		}
       	} else if(facing == "inside") {
      		circos.lines(c(x1, x1), c(y1, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd, straight = TRUE)
        	circos.lines(c(x1, (x1+x2)/2), c(height, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd)
        	circos.lines(c(x2, x2), c(y2, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd, straight = TRUE)
       		circos.lines(c(x2, (x1+x2)/2), c(height, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd)
       		if(!is.null(node_par)) {
       			circos.points((x1+x2)/2, height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
       		}
       	}

        # do it recursively
        if(is.leaf(d1)) {
        	node_par = attr(d1, "nodePar")
        	if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(d1, "nodePar")))
        	if(facing == "outside") {
        		if(!is.null(node_par)) {
	       			circos.points(x1, max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
	       		}
        	} else if(facing == "inside") {
        		if(!is.null(node_par)) {
	       			circos.points(x1, 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
	       		}
        	}
        } else {
            draw.d(d1, max_height, facing, max_width)
        }

        if(is.leaf(d2)) {
        	node_par = attr(d2, "nodePar")
        	if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(d2, "nodePar")))
        	if(facing == "outside") {
        		if(!is.null(node_par)) {
	       			circos.points(x2, max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
	       		}
        	} else if(facing == "inside") {
        		if(!is.null(node_par)) {
	       			circos.points(x2, 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
	       		}
        	}
        } else {
            draw.d(d2, max_height, facing, max_width)
        }
    }

    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5

    names(x) = labels
    n = length(labels)

    draw.d(dend, max_height, facing, max_width = n)
}
