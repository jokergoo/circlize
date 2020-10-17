# == title
# Create plotting regions for a whole track
#
# == param
# -sectors      A `factor` or a character vector which represents categories of data, if it is ``NULL``,
#               then it uses all sector index.
# -factors      The same as ``sectors``. It will be removed in future versions. 
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
# https://jokergoo.github.io/circlize_book/book/circular-layout.html
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# set.seed(123)
# df = data.frame(fa = sample(letters[1:8], 100, replace = TRUE),
#                 x = runif(100), y = rnorm(100))
# circos.track(ylim = c(0, 1), bg.col = rand_color(8))
# circos.track(df$fa, x = df$x, y = df$y, panel.fun = function(x, y) {
#     circos.points(x, y)
# }, track.height = 0.2, bg.border = rand_color(8))
# circos.clear()
circos.trackPlotRegion = function(
	sectors = NULL, 
	x = NULL, y = NULL,
	ylim = NULL,
    force.ylim = TRUE,
    track.index = NULL,
	track.height = circos.par("track.height"),
	track.margin = circos.par("track.margin"),
	cell.padding = circos.par("cell.padding"),
    bg.col = NA,
    bg.border = "black",
    bg.lty = par("lty"),
    bg.lwd = par("lwd"),
    panel.fun = function(x, y) {NULL},
    factors = sectors) {

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

    if(is.function(factors) || is.function(x) || is.function(y) || is.function(ylim)) {
        stop_wrap("The panel function should be set explicitly with the argument name `panel.fun = ...`.")
    }

    # although ``x`` and ``y`` are not necessary, but once they are set, they must
	# have same length as ``factors``
    if(!is.null(y) && length(y) != length(factors) ||
	   !is.null(x) && length(x) != length(factors)) {
        stop_wrap("Length of data and length of sectors differ.")
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

    all_le = get.all.sector.index()
    le = levels(factors)
    nlevel = length(all_le)
    bg.col = recycle.with.levels(bg.col, all_le)
    bg.border = recycle.with.levels(bg.border, all_le)
    bg.lty = recycle.with.levels(bg.lty, all_le)
    bg.lwd = recycle.with.levels(bg.lwd, all_le)

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
			ylim = matrix(rep(ylim, nlevel), ncol = 2, byrow = TRUE)
		} else if(is.matrix(ylim) && ncol(ylim) == 2 && nrow(ylim) == nlevel) {

		} else {
			stop_wrap("Wrong `ylim` format.")
		}
    }

    # now for each factor, create plotting region
    for(i in seq_along(all_le)) {
		# `ylim` is prior to `y`
        if(is.null(ylim)) {
			ylim2 = y.range[i, ]
        } else {
			ylim2 = ylim[i, ]
		}

        # create plotting region for single cell
        circos.createPlotRegion(track.start = track.start,
                              track.height = track.height, sector.index = all_le[i],
                              track.index = track.index,
                              ylim = ylim2, bg.col = bg.col[i],
                              bg.border = bg.border[i], bg.lty = bg.lty[i], bg.lwd = bg.lwd[i])

        if(all_le[i] %in% le) {
    		l = factors == all_le[i]
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
    }

	circos.par(track.margin = o.track.margin)
	circos.par(cell.padding = o.cell.padding)

    return(invisible(NULL))

}

# == title
# Create plotting regions for a whole track
#
# == param
# -... Pass to `circos.trackPlotRegion`.
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
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index)
# })
# circos.update(sector.index = "b", track.index = 1)
# circos.rect(CELL_META$cell.xlim[1], CELL_META$cell.ylim[1],
#             CELL_META$cell.xlim[2], CELL_META$cell.ylim[2],
#             col = "#FF000080")
# circos.clear()
circos.updatePlotRegion = function(
	sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    bg.col = NA,
    bg.border = "black",
    bg.lty = par("lty"),
    bg.lwd = par("lwd")) {

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
circos.createPlotRegion = function(
	track.start,
	track.height = circos.par("track.height"),
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    ylim,
    bg.col = NA,
    bg.border = "black",
    bg.lty = par("lty"),
    bg.lwd = par("lwd")) {

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
# == example
# plot(c(-1, 1), c(-1, 1), type = "n", axes = FALSE, ann = FALSE, asp = 1)
# draw.sector(20, 0)
# draw.sector(30, 60, rou1 = 0.8, rou2 = 0.5, clock.wise = FALSE, col = "#FF000080")
# draw.sector(350, 1000, col = "#00FF0080", border = NA)
# draw.sector(0, 180, rou1 = 0.25, center = c(-0.5, 0.5), border = 2, lwd = 2, lty = 2)
# draw.sector(0, 360, rou1 = 0.7, rou2 = 0.6, col = "#0000FF80")
#
# sectors = letters[1:8]
# circos.initialize(sectors, xlim = c(0, 1))
# for(i in 1:3) {
#     circos.trackPlotRegion(ylim = c(0, 1))
# }
# circos.info(plot = TRUE)
#
# draw.sector(get.cell.meta.data("cell.start.degree", sector.index = "a"),
#             get.cell.meta.data("cell.end.degree", sector.index = "a"),
#             rou1 = 1, col = "#FF000040")
#
# draw.sector(0, 360,
#     rou1 = get.cell.meta.data("cell.top.radius", track.index = 1),
#     rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1),
#     col = "#00FF0040")
#
# draw.sector(get.cell.meta.data("cell.start.degree", sector.index = "e"),
#             get.cell.meta.data("cell.end.degree", sector.index = "f"),
#             get.cell.meta.data("cell.top.radius", track.index = 2),
#             get.cell.meta.data("cell.bottom.radius", track.index = 3),
#             col = "#0000FF40")
#
# pos = circlize(c(0.2, 0.8), c(0.2, 0.8), sector.index = "h", track.index = 2)
# draw.sector(pos[1, "theta"], pos[2, "theta"], pos[1, "rou"], pos[2, "rou"],
#     clock.wise = TRUE, col = "#00FFFF40")
# circos.clear()
draw.sector = function(
	start.degree = 0,
	end.degree = 360,
	rou1 = 1,
	rou2 = NULL,
	center = c(0, 0),
	clock.wise = TRUE,
	col = NA,
	border = "black",
	lwd = par("lwd"),
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
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/graphics.html#highlight-sectors-and-tracks
#
# == example
# sectors = letters[1:8]
# circos.initialize(sectors, xlim = c(0, 1))
# for(i in 1:4) {
#     circos.trackPlotRegion(ylim = c(0, 1))
# }
# circos.info(plot = TRUE)
#
# highlight.sector(c("a", "h"), track.index = 1)
# highlight.sector("c", col = "#00FF0040")
# highlight.sector("d", col = NA, border = "red", lwd = 2)
# highlight.sector("e", col = "#0000FF40", track.index = c(2, 3))
# highlight.sector(c("f", "g"), col = NA, border = "green",
#     lwd = 2, track.index = c(2, 3))
# highlight.sector(sectors, col = "#FFFF0040", track.index = 4)
# circos.clear()
highlight.sector = function(
	sector.index,
	track.index = get.all.track.index(),
	col = "#FF000040",
	border = NA,
	lwd = par("lwd"),
	lty = par("lty"),
	padding = c(0, 0, 0, 0),
	text = NULL,
	text.col = par("col"),
	text.vjust = 0.5,
	...) {

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
                if(circos.par("clock.wise")) {
    				start.degree = get.cell.meta.data("cell.start.degree", sector.index.vector[1], track.index = 1)
    				end.degree = get.cell.meta.data("cell.end.degree", sector.index.vector[length(sector.index.vector)], track.index = 1)
				} else {
                    end.degree = get.cell.meta.data("cell.end.degree", sector.index.vector[1], track.index = 1)
                    start.degree = get.cell.meta.data("cell.start.degree", sector.index.vector[length(sector.index.vector)], track.index = 1)
                }
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
# Set gaps between tracks
#
# == param
# -gap Gap between two tracks. Use `mm_h`/`cm_h`/`inches_h` to set in absolute units.
#
# == example
# circos.initialize(letters[1:10], xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# set_track_gap(mm_h(2))
# circos.track(ylim = c(0, 1))
# circos.clear()
set_track_gap = function(gap = 0.02) {
    circos.par$track.margin = c(0, gap)
}
