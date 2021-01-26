
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
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/graphics.html#points
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.points(runif(10), runif(10))
# })
# circos.points(runif(10), runif(10), sector.index = "c", pch = 16, col = "red")
# circos.clear()
circos.points = function(
	x, y,
	sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    pch = par("pch"),
    col = par("col"),
    cex = par("cex"),
    bg = par("bg")) {

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.points' can only be used after the plotting region has been created")
    }

    if(missing(y)) {
        if(ncol(x) == 2) {
            y = x[, 2]
            x = x[, 1]
        }
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
# -sectors      A `factor` or a character vector which represents the categories of data
# -factors      The same as ``sectors``. It will be removed in future versions. 
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
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# df = data.frame(sectors = sample(letters[1:8], 100, replace = TRUE),
#                 x = runif(100), y = runif(100))
# circos.track(ylim = c(0, 1))
# circos.trackPoints(df$sectors, x = df$x, y = df$y, pch = 16, col = as.numeric(factor(df$fa)))
# circos.clear()
circos.trackPoints = function(
    sectors,
	x, y,
	track.index = get.current.track.index(),
    pch = par("pch"),
    col = par("col"),
    cex = par("cex"),
    bg = par("bg"),
    factors = sectors
    ) {

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
# -x            Data points on x-axis, measured in "current" data coordinate.
# -y            Data points on y-axis, measured in "current" data coordinate.
# -sector.index Index for the sector.
# -track.index  Index for the track.
# -col          Line color.
# -lwd          Line width.
# -lty          Line style.
# -type         Line type, similar as ``type`` argument in `graphics::lines`, but only in ``c("l", "o", "h", "s")``
# -straight     Whether draw straight lines between points.
# -area         Whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls color of the line.
# -area.baseline deprecated, use ``baseline`` instead.
# -baseline     The base line to draw areas. By default it is the minimal of y-range (bottom). It can be a string or a number.
#               If a string, it should be one of ``bottom`` and ``top``. This argument also works if ``type`` is set to ``h``.
# -border       color for border of the area.
# -pt.col       If ``type`` is "o", point color.
# -cex          If ``type`` is "o", point size.
# -pch          If ``type`` is "o", point type.
#
# == details
# Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circular layout.
# But if you do not want to do such transformation you can use this function just drawing straight
# lines between points by setting ``straight`` to ``TRUE``.
#
# Drawing areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be done by specifying
# ``area`` to ``TURE``.
#
# == example
# sectors = letters[1:9]
# circos.par(points.overflow.warning = FALSE)
# circos.initialize(sectors, xlim = c(0, 10))
# circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.5)
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
# circos.text(5, 9, "type = 'l'", sector.index = "a", facing = "outside")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
# circos.text(5, 9, "type = 'o'", sector.index = "b", facing = "outside")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
# circos.text(5, 9, "type = 'h'", sector.index = "c", facing = "outside")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "h", baseline = 5)
# circos.text(5, 9, "type = 'h', baseline = 5", sector.index = "d", facing = "outside")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", type = "s")
# circos.text(5, 9, "type = 's'", sector.index = "e", facing = "outside")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f", area = TRUE)
# circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "f")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g", type = "o", area = TRUE)
# circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "g")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "h", type = "s", area = TRUE)
# circos.text(5, 9, "type = 's', area = TRUE", sector.index = "h")
#
# circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "i", area = TRUE, baseline = "top")
# circos.text(5, 9, "type = 'l', area = TRUE, baseline = 'top'", sector.index = "i")
#
# circos.clear()
circos.lines = function(
	x, y,
	sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    col = ifelse(area, "grey", par("col")),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    straight = FALSE,
    area = FALSE,
    area.baseline = NULL,
    border = "black",
    baseline = "bottom",
    pt.col = par("col"),
    cex = par("cex"),
    pch = par("pch")) {

	if(!is.null(area.baseline)) {
		baseline = area.baseline
		warning_wrap("`area.baseline` is deprecated, please use `baseline` instead.")
	}

    if(missing(y)) {
        if(ncol(x) == 2) {
            y = x[, 2]
            x = x[, 1]
        }
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
# -sectors      A `factor` or a character vector which represents the categories of data.
# -factors      The same as ``sectors``. It will be removed in future versions. 
# -x            Data points on x-axis.
# -y            Data points on y-axis.
# -track.index  Index for the track.
# -col          Line color.
# -lwd          Line width.
# -lty          Line style.
# -type         Line type, similar as ``type`` argument in `graphics::lines`, but only in ``c("l", "o", "h", "s")``.
# -straight     Whether draw straight lines between points.
# -area         Whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls the color of the line.
# -area.baseline Deprecated, use ``baseline`` instead.
# -baseline The base line to draw area, pass to `circos.lines`.
# -border       Color for border of the area.
# -pt.col       If ``type`` is "o", points color.
# -cex          If ``type`` is "o", points size.
# -pch          If ``type`` is "o", points type.
#
# == details
# The function adds lines in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add lines in cells by calling `circos.lines`.
#
# This function can be replaced by a ``for`` loop containing `circos.lines`.
circos.trackLines = function(
    sectors,
	x, y,
	track.index = get.current.track.index(),
    col = par("col"),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    straight = FALSE,
	area = FALSE,
	area.baseline = NULL,
	border = "black",
	baseline = "bottom",
    pt.col = par("col"),
    cex = par("cex"),
    pch = par("pch"),
    factors = sectors) {

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
# -rot          Rotation of the rectangles. The value is measured clockwise in degree.
#               Rotation is relative to the center of the rectangles.
# -... pass to `graphics::polygon`
#
# == details
# The name for this function is `circos.rect`
# because if you imagine the plotting region as Cartesian coordinate, then it is rectangle.
# in the polar coordinate, the up and bottom edge become two arcs.
#
# This function can be vectorized.
#
# == examples
# circos.initialize(c("a", "b", "c", "d"), xlim = c(0, 10))
# circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
#     for(rot in seq(0, 360, by = 30)) {
#         circos.rect(2, 2, 6, 6, rot = rot)
#     }
# }, track.height = 0.5)
#
circos.rect = function(
	xleft, ybottom, xright, ytop,
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(),
	rot = 0,
	...) {

    # if(! (length(xleft) == 1 &&
    #       length(ybottom) == 1 &&
    #       length(xright) == 1 &&
    #       length(ytop) == 1) ) {
    #     stop("There should only be one data points in 'xleft', 'ybottom', 'xright' or 'ytop'.\n")
    # }

    if(!has.cell(sector.index, track.index)) {
        stop_wrap("'circos.rect' can only be used after the plotting region been created.")
    }

    if(missing(xleft) && missing(ybottom) && missing(xright) && missing(ytop)) {
        cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
        cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sector.index, track.index = track.index)
        xleft = cell.xlim[1]
        ybottom = cell.ylim[1]
        xright = cell.xlim[2]
        ytop = cell.ylim[2]
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

    np = length(xleft)
    if(rot == 0) {
        x = unlist(lapply(seq_len(np), function(i) c(xleft[i], xleft[i], xright[i], xright[i], xleft[i], NA)))
        y = unlist(lapply(seq_len(np), function(i) c(ybottom[i], ytop[i], ytop[i], ybottom[i], ybottom[i], NA)))
        x = x[-length(x)]
        y = y[-length(y)]
    } else {
        xcenter = (xleft + xright)/2
        ycenter = (ybottom + ytop)/2

        p1 = list(x = xleft, y = ybottom)
        p2 = list(x = xright, y = ybottom)
        p3 = list(x = xright, y = ytop)
        p4 = list(x = xleft, y = ytop)

        center = list(x = c(p1$x + p3$x)/2, y = (p1$y + p3$y)/2)

        q1 = .rotate(p1$x, p1$y, center$x, center$y, rot/180*pi)
        q2 = .rotate(p2$x, p2$y, center$x, center$y, rot/180*pi)
        q3 = .rotate(p3$x, p3$y, center$x, center$y, rot/180*pi)
        q4 = .rotate(p4$x, p4$y, center$x, center$y, rot/180*pi)

        x = unlist(lapply(seq_len(np), function(i) {
                c(q1$x[i], q2$x[i], q3$x[i], q4$x[i], q1$x[i], NA)
            }))
        y = unlist(lapply(seq_len(np), function(i) {
                c(q1$y[i], q2$y[i], q3$y[i], q4$y[i], q1$y[i], NA)
            }))
        x = x[-length(x)]
        y = y[-length(y)]
    }
    circos.polygon(x, y, sector.index = sector.index, track.index = track.index, ...)

    return(invisible(NULL))
}

# == title
# Draw triangles
#
# == param
# -x1 x-coordinates for the first point.
# -y1 y-coordinates for the first point.
# -x2 x-coordinates for the second point.
# -y2 y-coordinates for the second point.
# -x3 x-coordinates for the third point.
# -y3 y-coordinates for the third point.
# -... Pass to `circos.polygon`.
#
# == example
# circos.initialize(c("a", "b", "c", "d"), xlim = c(0, 10))
# circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
#     circos.triangle(c(2, 2), c(2, 8),
#                     c(8, 8), c(2, 8),
#                     c(5, 5), c(8, 2))
# }, track.height = 0.5)
#
circos.triangle = function(x1, y1, x2, y2, x3, y3, ...) {
    n1 = length(x1)
    n2 = length(y1)
    n3 = length(x2)
    n4 = length(y2)
    n5 = length(x3)
    n6 = length(y3)

    n = max(c(n1, n2, n3, n4, n5, n6))
    if(n1 == 1) x1 = rep(x1, n)
    if(n2 == 1) y1 = rep(y1, n)
    if(n3 == 1) x2 = rep(x2, n)
    if(n4 == 1) y2 = rep(y2, n)
    if(n5 == 1) x3 = rep(x3, n)
    if(n6 == 1) y3 = rep(y3, n)

    if(! (length(x1) == length(y1) && length(y1) == length(x2) && length(x2) == length(y2) && length(y2) == length(x3) && length(x3) == length(y3)) ) {
        stop_wrap("x1, y1, x2, y2, x3, y3 should have same length.")
    }

    np = length(x1)
    x = unlist(lapply(seq_len(np), function(i) {
            c(x1[i], x2[i], x3[i], x1[i], NA)
        }))
    y = unlist(lapply(seq_len(np), function(i) {
            c(y1[i], y2[i], y3[i], y1[i], NA)
        }))
    x = x[-length(x)]
    y = y[-length(y)]

    circos.polygon(x, y, ...)
}

.rotate = function(x, y, cx, cy, theta) {
    x2 = x - cx
    y2 = y - cy
    rho = sqrt(x2^2 + y2^2)
    alpha = atan(y2/x2)
    alpha = ifelse(x2 < 0, alpha + pi, ifelse(y2 < 0, alpha + 2*pi, alpha))
    beta = alpha + theta
    nx = rho*cos(beta) + cx
    ny = rho*sin(beta) + cy
    return(list(x = nx, y = ny))
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
# Note: start point should overlap with the end point.
#
# == example
# set.seed(123)
# sectors = letters[1:4]
# circos.initialize(sectors, xlim = c(0, 1))
# circos.trackPlotRegion(ylim = c(-3, 3), track.height = 0.4, panel.fun = function(x, y) {
#     x1 = runif(20)
#     y1 = x1 + rnorm(20)
#     or = order(x1)
#     x1 = x1[or]
#     y1 = y1[or]
#     loess.fit = loess(y1 ~ x1)
#     loess.predict = predict(loess.fit, x1, se = TRUE)
#     d1 = c(x1, rev(x1))
#     d2 = c(loess.predict$fit + loess.predict$se.fit,
#         rev(loess.predict$fit - loess.predict$se.fit))
#     circos.polygon(d1, d2, col = "#CCCCCC", border = NA)
#     circos.points(x1, y1, cex = 0.5)
#     circos.lines(x1, loess.predict$fit)
# })
# circos.clear()
circos.polygon = function(
	x, y,
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(),
	...) {

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
# -x0 x coordinates for starting points.
# -y0 y coordinates for ending points.
# -x1 x coordinates for starting points.
# -y1 y coordinates for ending points.
# -sector.index Index for the sector.
# -track.index  Index for the track.
# -straight Whether the segment is a straight line.
# -col Color of the segments.
# -lwd Line width of the segments.
# -lty Line type of the segments.
# -... Pass to `graphics::lines`.
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1), track.height = 0.3, panel.fun = function(x, y) {
#     x = seq(0.2, 0.8, by = 0.2)
#     y = seq(0.2, 0.8, by = 0.2)
#
#     circos.segments(x, 0.1, x, 0.9)
#     circos.segments(0.1, y, 0.9, y)
# })
# circos.clear()
circos.segments = function(
	x0, y0, x1, y1,
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(),
	straight = FALSE,
	col = par("col"),
	lwd = par("lwd"),
	lty = par("lty"),
	...) {

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
        stop_wrap("'circos.segments' can only be used after the plotting region been created.")
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
# https://jokergoo.github.io/circlize_book/book/graphics.html#text
#
# == example
# sectors = letters[1:4]
# circos.par(points.overflow.warning = FALSE)
# circos.initialize(sectors, xlim = c(0, 10))
# circos.trackPlotRegion(sectors, ylim = c(0, 10),
#   track.height = 0.5, panel.fun = function(x, y) {
#     circos.text(3, 1, "inside", facing = "inside", cex = 0.8)
#     circos.text(7, 1, "outside", facing = "outside", cex = 0.8)
#     circos.text(0, 5, "reverse.clockwise", facing = "reverse.clockwise",
#         adj = c(0.5, 0), cex = 0.8)
#     circos.text(10, 5, "clockwise", facing = "clockwise", adj = c(0.5, 0),
#         cex = 0.8)
#     circos.text(5, 5, "downward", facing = "downward", cex = 0.8)
#     circos.text(3, 9, "====bending.inside====", facing = "bending.inside",
#         cex = 0.8)
#     circos.text(7, 9, "====bending.outside====", facing = "bending.outside",
#         cex = 0.8)
# })
# circos.clear()
circos.text = function(
	x, y,
	labels,
	sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    direction = NULL,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
	    "downward", "bending", "bending.inside", "bending.outside"),
    niceFacing = FALSE,
	adj = par("adj"),
	cex = 1,
	col = par("col"),
	font = par("font"),
	...) {


    if(missing(y)) {
        if(ncol(x) == 2) {
            y = x[, 2]
            x = x[, 1]
        }
    }
    
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
# -sectors      A `factor` or a character vector which represents the categories of data
# -factors      The same as ``sectors``. It will be removed in future versions. 
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
circos.trackText = function(
	sectors, 
	x, y,
	labels,
	track.index = get.current.track.index(),
    direction = NULL,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
	    "downward", "bending", "bending.inside", "bending.outside"),
    niceFacing = FALSE,
    adj = par("adj"),
    cex = 1,
    col = par("col"),
    font = par("font"),
    factors = sectors) {

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
# -major.tick       Whether to draw major tick. If it is set to ``FALSE``, there will be
#                   no minor ticks neither.
# -sector.index     Index for the sector.
# -track.index      Index for the track.
# -labels.font      Font style for the axis labels.
# -labels.cex       Font size for the axis labels.
# -labels.direction Deprecated, use ``facing`` instead.
# -labels.facing    Facing of labels on axis, passing to `circos.text`
# -labels.niceFacing Should facing of axis labels be human-easy.
# -direction        Whether the axis ticks point to the outside or inside of the circle.
# -minor.ticks      Number of minor ticks between two close major ticks.
# -major.tick.length Length of the major ticks, measured in "current" data coordinate. `convert_y` can be
#                   used to convert an absolute unit to the data coordinate.
# -major.tick.percentage Not used any more, please directly use ``major.tick.length``.
# -lwd              Line width for ticks.
# -col              Color for the axes.
# -labels.col       Color for the labels.
# -labels.pos.adjust  Whether to adjust the positions of the first label and the last label so that the first label 
#                     align to its left and the last label align to its right if they exceed the range on axes. The value can be a vector
#                    of length two which correspond to the first label and the last label.
#
# == details
# It only draws axes on x-direction.
#
# == seealso
# `circos.yaxis` draws axes on y-direction.
#
# https://jokergoo.github.io/circlize_book/book/graphics.html#axes
#
# == example
# sectors = letters[1:8]
# circos.par(points.overflow.warning = FALSE)
# circos.initialize(sectors, xlim = c(0, 10))
# circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.1,
#     bg.border = NA, panel.fun = function(x, y) {
#         circos.text(5, 10, get.cell.meta.data("sector.index"))
# })
#
# circos.trackPlotRegion(sectors, ylim = c(0, 10))
# circos.axis(sector.index = "a")
# circos.axis(sector.index = "b", direction = "inside", labels.facing = "outside")
# circos.axis(sector.index = "c", h = "bottom")
# circos.axis(sector.index = "d", h = "bottom", direction = "inside",
#     labels.facing = "reverse.clockwise")
# circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
# circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9),
#     labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
# circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9),
#     labels = c("a1", "c1", "e1", "g1", "f1"), major.tick = FALSE,
#     labels.facing = "reverse.clockwise")
# circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9),
#     labels = c("a1", "c1", "e1", "g1", "f1"), minor.ticks = 2, 
#     major.tick.length = mm_y(5), labels.facing = "clockwise")
# circos.clear()
#
# if(FALSE) {
#
# ############### real-time clock #################
# factors = letters[1]
#
# circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
# circos.initialize(sectors, xlim = c(0, 12))
# circos.trackPlotRegion(sectors, ylim = c(0, 1), bg.border = NA)
# circos.axis(sector.index = "a", major.at = 0:12, labels = "",
#     direction = "inside", major.tick.length = mm_y(3))
# circos.text(1:12, rep(0.5, 12), 1:12, facing = "downward")
#
# while(1) {
#     current.time = as.POSIXlt(Sys.time())
#     sec = ceiling(current.time$sec)
#     min = current.time$min
#     hour = current.time$hour
#
#     # erase the clock hands
#     draw.sector(rou1 = 0.8, border = "white", col = "white")
#
#     sec.degree = 90 - sec/60 * 360
#     arrows(0, 0, cos(sec.degree/180*pi)*0.8, sin(sec.degree/180*pi)*0.8)
#
#     min.degree = 90 - min/60 * 360
#     arrows(0, 0, cos(min.degree/180*pi)*0.7, sin(min.degree/180*pi)*0.7, lwd = 2)
#
#     hour.degree = 90 - hour/12 * 360 - min/60 * 360/12
#     arrows(0, 0, cos(hour.degree/180*pi)*0.4, sin(hour.degree/180*pi)*0.4, lwd = 2)
#
#     Sys.sleep(1)
# }
# circos.clear()
# }
circos.axis = function(
	h = "top",
	major.at = NULL,
	labels = TRUE,
	major.tick = TRUE,
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(),
	labels.font = par("font"),
	labels.cex = par("cex"),
	labels.facing = "inside",
	labels.direction = NULL,
	labels.niceFacing = TRUE,
	direction = c("outside", "inside"),
	minor.ticks = 4,
	major.tick.length = mm_y(1),
    major.tick.percentage = 0.5,
	lwd = par("lwd"),
	col = par("col"),
	labels.col = par("col"),
	labels.pos.adjust = TRUE) {

    set.current.cell(sector.index, track.index)

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
    if(!missing(major.tick.percentage)) {
        message("`major.tick.percentage` is not used any more, please directly use argument `major.tick.length`.")
    }
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

	if(is.logical(labels)) {
        if(labels[1]) {
    		add_axis_labels(major.at[l], rep(h, sum(l)) + (major.tick.length + mm_y(0.5, sector.index, track.index))*ifelse(direction == "outside", 1, -1),
    		           labels = major.at[l], adj = labels.adj,
    		           font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
    		           facing = labels.facing, niceFacing = labels.niceFacing, h = h, col = labels.col,
    		           labels.pos.adjust = labels.pos.adjust)
    	}
    } else if(is.function(labels)) {
        add_axis_labels(major.at[l], rep(h, sum(l)) + (major.tick.length + mm_y(0.5, sector.index, track.index))*ifelse(direction == "outside", 1, -1),
                       labels = labels(major.at[l]), adj = labels.adj,
                       font = labels.font, cex = labels.cex, sector.index = sector.index, track.index = track.index,
                       facing = labels.facing, niceFacing = labels.niceFacing, h = h, col = labels.col,
                       labels.pos.adjust = labels.pos.adjust)

    } else if(length(labels)) {
		add_axis_labels(major.at[l], rep(h, sum(l)) + (major.tick.length + mm_y(0.5, sector.index, track.index))*ifelse(direction == "outside", 1, -1),
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
# -... All pass to `circos.axis`.
#
# == details
# This function is identical to `circos.axis`.
#
circos.xaxis = function(...) {
	circos.axis(...)
}

.default.major.by = function(
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index()) {
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
# == example
# op = par(no.readonly = TRUE)
#
# sectors = letters[1:8]
# circos.par(points.overflow.warning = FALSE)
# circos.par(gap.degree = 8)
# circos.initialize(sectors, xlim = c(0, 10))
# circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.5)
# par(cex = 0.8)
# for(a in letters[2:4]) {
#   circos.yaxis(side = "left", sector.index = a)
# }
# for(a in letters[5:7]) {
#   circos.yaxis(side = "right", sector.index = a)
# }
# circos.clear()
#
# par(op)
circos.yaxis = function(
	side = c("left", "right"),
	at = NULL,
	labels = TRUE,
	tick = TRUE,
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index(),
	labels.font = par("font"),
	labels.cex = par("cex"),
	labels.niceFacing = TRUE,
	tick.length = convert_x(1, "mm", sector.index, track.index),
	lwd = par("lwd"),
	col = par("col"),
	labels.col = par("col")) {

	ylim = get.cell.meta.data("ylim", sector.index, track.index)

	side = match.arg(side)[1]
	if(side == "left") {
		v = get.cell.meta.data("cell.xlim", sector.index, track.index)[1]
	} else if(side == "right") {
		v = get.cell.meta.data("cell.xlim", sector.index, track.index)[2]
	}

	if(is.null(at)) {
		at = grid.pretty(ylim)
        if(is.function(labels)) {
            labels = labels(at)
        } else {
		  labels = at
        }
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
# -sectors      A `factor` or a character vector which represents the categories of data
# -factors      The same as ``sectors``. It will be removed in future versions. 
# -x            Data on the x-axis
# -track.index  Index for the track which is going to be updated. Setting it to ``NULL`` means
#               creating the plotting regions in the next newest track.
# -track.height Height of the track. It is the percentage to the radius of the unit circle.
#               If to update a track, this argument is disabled.
# -ylim         Ranges on y-direction. By default, ``ylim`` is calculated automatically.
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``.
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
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-plots.html#histograms
#
# == example
# \donttest{
# x = rnorm(1600)
# sectors = sample(letters[1:16], 1600, replace = TRUE)
# circos.initialize(sectors, x = x)
# circos.trackHist(sectors, x = x, col = "#999999",
#   border = "#999999")
# circos.trackHist(sectors, x = x, bin.size = 0.1,
#   col = "#999999", border = "#999999")
# circos.trackHist(sectors, x = x, draw.density = TRUE,
#   col = "#999999", border = "#999999")
# circos.clear()
# }
circos.trackHist = function(
	sectors,
	x,
	track.height = circos.par("track.height"),
    track.index = NULL,
    ylim = NULL,
    force.ylim = TRUE,
    col = ifelse(draw.density, "black", NA),
	border = "black",
	lty = par("lty"),
	lwd = par("lwd"),
    bg.col = NA,
    bg.border = "black",
    bg.lty = par("lty"),
    bg.lwd = par("lwd"),
    breaks = "Sturges",
    include.lowest = TRUE,
    right = TRUE,
    draw.density = FALSE,
    bin.size = NULL,
    area = FALSE,
    factors = sectors) {

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
    circos.trackPlotRegion(sectors = fa, y=yy, track.height = track.height,
                      track.index = track.index, ylim = ylim, force.ylim = force.ylim,
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
        circos.trackLines(sectors = fa, xx, yy, track.index = track.index,
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
# Add circular dendrograms
#
# == param
# -dend A `stats::dendrogram` object.
# -facing Is the dendromgrams facing inside to the circle or outside?
# -max_height Maximum height of the dendrogram. This is important if more than one dendrograms
#             are drawn in one track and making them comparable. The height of a dendrogram
#             can be obtained by ``attr(dend, "height")``.
# -use_x_attr Whether use the ``x`` attribute to determine node positions in the dendrogram, used internally.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# Assuming there are ``n`` nodes in the dendrogram, the positions for leaves on x-axis are always ``0.5, 1.5, ..., n - 0.5``.
# So you must be careful with ``xlim`` when you initialize the cirular layout.
#
# You can use the ``dendextend`` package to render the dendrograms.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-plots.html#phylogenetic-trees
#
# == example
# load(system.file(package = "circlize", "extdata", "bird.orders.RData"))
#
# labels = hc$labels  # name of birds
# ct = cutree(hc, 6)  # cut tree into 6 pieces
# n = length(labels)  # number of bird species
# dend = as.dendrogram(hc)
#
# circos.par(cell.padding = c(0, 0, 0, 0))
# circos.initialize(sectors = "a", xlim = c(0, n)) # only one sector
# max_height = attr(dend, "height")  # maximum height of the trees
# circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.3,
#     panel.fun = function(x, y) {
#         for(i in seq_len(n)) {
#             circos.text(i-0.5, 0, labels[i], adj = c(0, 0.5),
#                 facing = "clockwise", niceFacing = TRUE,
#                 col = ct[labels[i]], cex = 0.7)
#         }
# })
#
# suppressPackageStartupMessages(require(dendextend))
# dend = color_branches(dend, k = 6, col = 1:6)
#
# circos.trackPlotRegion(ylim = c(0, max_height), bg.border = NA,
#     track.height = 0.4, panel.fun = function(x, y) {
#         circos.dendrogram(dend, max_height = max_height)
# })
# circos.clear()
circos.dendrogram = function(
    dend,
    facing = c("outside", "inside"),
    max_height = NULL,
    use_x_attr = FALSE,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    set.current.cell(sector.index, track.index)
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
        height = attr(dend, "height")
        midpoint = attr(dend, "midpoint")
        n = length(dend)

        xl = numeric(n)
        yl = numeric(n)
        for(i in seq_len(n)) {
            if(use_x_attr) {
                xl[i] = attr(dend[[i]], "x")
            } else {
                if(is.leaf(dend[[i]])) {
                    xl[i] = x[as.character(attr(dend[[i]], "label"))]
                } else {
                    xl[i] = attr(dend[[i]], "midpoint") + x[as.character(labels(dend[[i]]))[1]]
                }
            }
            yl[i] = attr(dend[[i]], "height")
        }

        # graphic parameter for current branch
        # only for lines, there are lwd, col, lty
        edge_par_lt = vector("list", n)
        for(i in seq_len(n)) {
            edge_par_lt[[i]] = do.call("lines_par", as.list(attr(dend[[i]], "edgePar")))  # as.list to convert NULL to list()
        }
        node_par = attr(dend, "nodePar")
        if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend, "nodePar")))

        # plot the connection line
        if(facing == "outside") {
            if(n == 1) {
                circos.lines(c(xl[1], xl[1]), max_height - c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
            } else {
                circos.lines(c(xl[1], xl[1]), max_height - c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
                circos.lines(c(xl[1], (xl[1]+xl[2])/2), max_height - c(height, height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd)
                if(n > 2) {
                    for(i in seq(2, n-1)) {
                        circos.lines(c(xl[i], xl[i]), max_height - c(yl[i], height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd, straight = TRUE)
                        circos.lines(c((xl[i-1]+xl[i])/2, (xl[i]+xl[i+1])/2), max_height - c(height, height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd)
                    }
                }
                circos.lines(c(xl[n], xl[n]), max_height - c(yl[n], height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd, straight = TRUE)
                circos.lines(c(xl[n], (xl[n]+xl[n-1])/2), max_height - c(height, height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd)
            }
            if(!is.null(node_par)) {
                circos.points(mean(xl)/2, max_height - height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
            }
        } else if(facing == "inside") {
            if(n == 1) {
                circos.lines(c(xl[1], xl[1]), c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
            } else {
                circos.lines(c(xl[1], xl[1]), c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
                circos.lines(c(xl[1], (xl[1]+xl[2])/2), c(height, height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd)
                if(n > 2) {
                    for(i in seq(2, n-1)) {
                        circos.lines(c(xl[i], xl[i]), c(yl[i], height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd, straight = TRUE)
                        circos.lines(c((xl[i-1]+xl[i])/2, (xl[i]+xl[i+1])/2), c(height, height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd)
                    }
                }
                circos.lines(c(xl[n], xl[n]), c(yl[n], height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd, straight = TRUE)
                circos.lines(c(xl[n], (xl[n]+xl[n-1])/2), c(height, height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd)
            }
            if(!is.null(node_par)) {
                circos.points(mean(xl)/2, height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
            }
        }

        # do it recursively
        for(i in seq_len(n)) {
            if(is.leaf(dend[[i]])) {
                node_par = attr(dend[[i]], "nodePar")
                if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend[[i]], "nodePar")))
                if(facing == "outside") {
                    if(!is.null(node_par)) {
                        circos.points(xl[i], max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
                    }
                } else if(facing == "inside") {
                    if(!is.null(node_par)) {
                        circos.points(xl[i], 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
                    }
                }
            } else {
                draw.d(dend[[i]], max_height, facing, max_width)
            }
        }
    }

    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5

    names(x) = labels
    n = length(labels)

    if(!is.leaf(dend)) draw.d(dend, max_height, facing, max_width = n)
}

# == title
# Draw barplots
#
# == param
# -value A numeric vector or a matrix. If it is a matrix, columns correspond to the height of bars.
# -pos Positions of the bars.
# -bar_width Width of bars. It assumes the bars locating at ``x = 1, 2, ...``.
# -col Filled color of bars.
# -border Color for the border.
# -lwd Line width.
# -lty Line style.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# If the input variable is a matrix, it draws a stacked barplot.
#
# Please note, the x-values of barplots are normally integer indices. Just be careful
# when initializing the circular layout.
#
# == example
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     value = runif(10)
#     circos.barplot(value, 1:10 - 0.5, col = 1:10)
# })
# circos.track(ylim = c(-1, 1), panel.fun = function(x, y) {
#     value = runif(10, min = -1, max = 1)
#     circos.barplot(value, 1:10 - 0.5, col = ifelse(value > 0, 2, 3))
# })
# circos.clear()
#
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 4), panel.fun = function(x, y) {
#     value = matrix(runif(10*4), ncol = 4)
#     circos.barplot(value, 1:10 - 0.5, col = 2:5)
# })
# circos.clear()
circos.barplot = function(value, pos, bar_width = 0.6,
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    set.current.cell(sector.index, track.index)

    if(is.matrix(value)) {
        if(nrow(value) != length(pos)) {
            stop("nrow of `value` should be the same as the length of `pos`.")
        }

        if(any(value < 0)) {
            stop("`value` should all be positive if it is a matrix.")
        }

        n = ncol(value)
        if(length(col) == 1) col = rep(col, n)
        if(length(border) == 1) border = rep(border, n)
        if(length(lwd) == 1) lwd = rep(lwd, n)
        if(length(lty) == 1) lty = rep(lty, n)
        if(length(bar_width) == 1) bar_width = rep(bar_width, n)
            
        for(i in 1:n) {
            if(i == 1) {
                circos.rect(pos - bar_width/2, 0, pos + bar_width/2, rowSums(value[, seq_len(i), drop = FALSE]), 
                    col = col[i], border = border[i], lwd = lwd[i], lty = lty[i])
            } else {
                circos.rect(pos - bar_width/2, rowSums(value[, seq_len(i-1), drop = FALSE]), pos + bar_width/2, rowSums(value[, seq_len(i)]), 
                    col = col[i], border = border[i], lwd = lwd[i], lty = lty[i])
            }
        }
    } else if(is.atomic(value)) {
        if(length(value) != length(pos)) {
            stop("`value` and `pos` should have the same length.")
        }
        circos.rect(pos - bar_width/2, 0, pos + bar_width/2, value, col = col, border = border, lwd = lwd, lty = lty)
    } else {
        stop("`value` should be a vector or a matrix.")
    }
}


# == title
# Draw boxplots
#
# == param
# -value A numeric vector, a matrix or a list. If it is a matrix, boxplots are made by columns (each column is a box).
# -pos Positions of the boxes.
# -outline Whether to draw outliers.
# -box_width Width of boxes.
# -col Filled color of boxes.
# -border Color for the border as well as the quantile lines.
# -lwd Line width.
# -lty Line style
# -cex Point size.
# -pch Point type.
# -pt.col Point color.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == detail
# Please note, the x-values of boxplots are normally integer indices. Just be careful
# when initializing the circular layout.
# 
# == example
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     for(pos in seq(0.5, 9.5, by = 1)) {
#         value = runif(10)
#         circos.boxplot(value, pos)
#     }
# })
# circos.clear()
#
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     value = replicate(runif(10), n = 10, simplify = FALSE)
#     circos.boxplot(value, 1:10 - 0.5, col = 1:10)
# })
# circos.clear()
circos.boxplot = function(value, pos, outline = TRUE, box_width = 0.6,
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    cex = par("cex"), pch = 1, pt.col = par("col"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    set.current.cell(sector.index, track.index)

    single_boxplot = function(value, pos, outline = TRUE, box_width = 0.6,
        col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
        cex = par("cex"), pch = 1, pt.col = par("col")) {

        boxplot_stats = boxplot(value, plot = FALSE)$stats
        box_height = boxplot_stats[4, 1] - boxplot_stats[2, 1]

        circos.rect(pos - 0.5* box_width, boxplot_stats[2, 1], pos + 0.5 * box_width, boxplot_stats[4, 1],
            col = col, border = border, lty = lty, lwd = lwd)
        circos.segments(pos - 0.5 * box_width, boxplot_stats[5, 1], pos + 0.5 * box_width, boxplot_stats[5, 1],
            col = border, lty = lty, lwd = lwd)
        circos.segments(pos, boxplot_stats[5, 1], pos, boxplot_stats[4, 1],
            col = border, lty = lty, lwd = lwd)
        circos.segments(pos, boxplot_stats[1, 1], pos, boxplot_stats[2, 1],
            col = border, lty = lty, lwd = lwd)
        circos.segments(pos - 0.5 * box_width, boxplot_stats[1, 1], pos + 0.5 * box_width, boxplot_stats[1, 1],
            col = border, lty = lty, lwd = lwd)
        circos.segments(pos - 0.5 * box_width, boxplot_stats[3, 1], pos + 0.5 * box_width, boxplot_stats[3, 1],
            col = border, lty = lty, lwd = lwd)
        if (outline) {
            l1 = value > boxplot_stats[5, 1]
            if (any(l1)) circos.points(x = rep(pos, sum(l1)), y = value[l1], cex = cex, col = pt.col, pch = pch)
            l2 = value < boxplot_stats[1, 1]
            if (any(l2)) circos.points(x = rep(pos, sum(l2)), y = value[l2], cex = cex, col = pt.col, pch = pch)
        }
    }

    if(is.matrix(value)) {
        value = as.data.frame(value)   
    } 

    if(is.list(value)) {
        n = length(value)
        if(length(pos) != n) {
            stop_wrap("Length of `pos` should be same as number of boxes.")
        }

        if(length(box_width) == 1) box_width = rep(box_width, n)

        if(length(col) == 1) col = rep(col, n)
        if(length(border) == 1) border = rep(border, n)
        if(length(lwd) == 1) lwd = rep(lwd, n)
        if(length(lty) == 1) lty = rep(lty, n)
        if(length(cex) == 1) cex = rep(cex, n)
        if(length(pch) == 1) pch = rep(pch, n)
        if(length(pt.col) == 1) pt.col = rep(pt.col, n)

        for(i in 1:n) {
            single_boxplot(value[[i]], pos = pos[i], outline = outline, box_width = box_width[i],
                col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], cex = cex[i], 
                pch = pch[i], pt.col = pt.col[i])
        }
    } else if(is.atomic(value)) {
        single_boxplot(value, pos = pos, outline = outline, box_width = box_width,
            col = col, border = border, lwd = lwd, lty = lty, cex = cex, pch = pch, pt.col = pt.col)
    }
}

# == title
# Draw violin plots
#
# == param
# -value A numeric vector, a matrix or a list. If it is a matrix, boxplots are made by columns.
# -pos Positions of the boxes.
# -violin_width Width of violins.
# -col Filled color of boxes.
# -border Color for the border as well as the quantile lines.
# -lwd Line width.
# -lty Line style
# -show_quantile Whether to show the quantile lines.
# -cex Point size.
# -pch Point type.
# -pt.col Point color
# -max_density The maximal density value across several violins. It is used to compare between violins.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == example
# \donttest{
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     for(pos in seq(0.5, 9.5, by = 1)) {
#         value = runif(10)
#         circos.violin(value, pos)
#     }
# })
# circos.clear()
#
# circos.initialize(letters[1:4], xlim = c(0, 10))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     value = replicate(runif(10), n = 10, simplify = FALSE)
#     circos.violin(value, 1:10 - 0.5, col = 1:10)
# })
# circos.clear()
# }
circos.violin = function(value, pos, violin_width = 0.8, 
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    show_quantile = TRUE, pt.col = par("col"), cex = par("cex"), pch = 16,
    max_density = NULL, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    set.current.cell(sector.index, track.index)

    single_violin = function(density, pos, violin_width = 0.8, 
        col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
        show_quantile = TRUE, pt.col = par("col"), cex = par("cex"), pch = 16,
        max_d = max(density$y), value = NULL) {

        y = density$x
        x = density$y

        x = x/max_d * (violin_width/2)
        y = c(y, rev(y))
        x = c(-x + pos, rev(x + pos))
        box_stat = boxplot(value, plot = FALSE)$stat

        circos.polygon(x, y, border = border, col = col, lwd = lwd, lty = lty)
        if(show_quantile) {
            circos.lines(c(pos, pos), box_stat[1:2, 1])
            circos.lines(x = c(pos, pos), y = box_stat[4:5, 1])
            circos.points(pos, box_stat[3, 1], cex = cex, col = pt.col, pch = pch)
        }
    }

    if(is.matrix(value)) {
        value = as.data.frame(value)   
    } 

    if(is.list(value)) {
        n = length(value)
        if(length(pos) != n) {
            stop_wrap("Length of `pos` should be same as number of violins.")
        }

        if(length(col) == 1) col = rep(col, n)
        if(length(border) == 1) border = rep(border, n)
        if(length(lwd) == 1) lwd = rep(lwd, n)
        if(length(lty) == 1) lty = rep(lty, n)
        if(length(cex) == 1) cex = rep(cex, n)
        if(length(pch) == 1) pch = rep(pch, n)
        if(length(pt.col) == 1) pt.col = rep(pt.col, n)
        if(length(violin_width) == 1) violin_width = rep(violin_width, n)

        density_list = lapply(value, density, na.rm = TRUE)
        
        for(i in seq_along(density_list)) {
            density = density_list[[i]]
            l = density$x >= min(value[[i]], na.rm = TRUE) & density$x <= max(value[[i]], na.rm = TRUE); l[is.na(l)] = FALSE
            density$x = density$x[l]
            density$y = density$y[l]
            density_list[[i]] = density
        }

        max_d = max(sapply(density_list, function(d) max(d$y)))
        if(!is.null(max_density)) max_d = max_density

        for(i in 1:n) {
            single_violin(density_list[[i]], pos = pos[i], violin_width = violin_width,
                col = col[i], border = border[i], lwd = lwd[i], lty = lty[i],
                show_quantile = show_quantile, pt.col = pt.col[i], cex = cex[i], pch = pch[i],
                max_d = max_d, value = value[[i]])
        }
    } else if(is.atomic(value)) {
        density = density(value, na.rm = TRUE)
        l = density$x >= min(value, na.rm = TRUE) & density$x <= max(value, na.rm = TRUE); l[is.na(l)] = FALSE
        density$x = density$x[l]
        density$y = density$y[l]
        max_d = max(density$y)
        if(!is.null(max_density)) max_d = max_density

        single_violin(density, pos = pos, violin_width = violin_width, 
            col = col, border = border, lwd = lwd, lty = lty,
            show_quantile = show_quantile, pt.col = pt.col, cex = cex, pch = pch,
            max_d = max(density$y), value = value)
    }
}




get_bezier_points = function(x1, y1, x2, y2, xlim, ylim) {

    x1 = (x1 - xlim[1])/(xlim[2] - xlim[1])
    x2 = (x2 - xlim[1])/(xlim[2] - xlim[1])
    y1 = (y1 - ylim[1])/(ylim[2] - ylim[1])
    y2 = (y2 - ylim[1])/(ylim[2] - ylim[1])

    r = 0.6 - abs(x2 - x1)/2
    p = cbind(c(0, 0, 1, 1), c(0, r, r, 1))
    pt = bezier::bezier(t = seq(0, 1, length = 50), p = p)

    wx = x2 - x1
    x = pt[, 1] * wx + x1

    wy = y2 - y1
    y = pt[, 2]*wy + y1

    x = x*(xlim[2] - xlim[1]) + xlim[1]
    y = y*(ylim[2] - ylim[1]) + ylim[1]

    data.frame(x = x, y = y)
}

# == title
# Draw connecting lines/ribons between two sets of points
#
# == param
# -x0 x coordinates for point set 1. The value can also be a two-column matrix.
# -y0 y coordinates for point set 1.
# -x1 x coordinates for point set 2. The value can also be a two-column matrix.
# -y1 y coordinates for point set 2.
# -sector.index Index for the sector.
# -track.index  Index for the track.
# -type Which type of connections. Values can be "normal", "segments" and "bezier".
# -segments.ratio When ``type`` is set to ``segments``, each connecting line is segmented into three parts.
#        This argument controls the length of the three parts of sub-segments.
# -col Color of the segments.
# -border Border color of the links.
# -lwd Line width of the segments.
# -lty Line type of the segments.
# -... Other arguments.
#
# == example
# \donttest{
# circos.initialize(c("a"), xlim = c(0, 1))
# circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
#     panel.fun = function(x, y) {
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
#     x0 = runif(100)
#     x1 = runif(100)
#    
#     circos.connect(x0, 0, x1, 1, 
#         type = "normal", border = NA,
#         col = rand_color(100, luminosity = "bright", transparency = 0.75))
# })
#
# circos.initialize(c("a"), xlim = c(0, 1))
# circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
#     panel.fun = function(x, y) {
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
#     x0 = runif(100)
#     x1 = runif(100)
#    
#     circos.connect(x0, 0, x1, 1, 
#         type = "bezier", border = NA,
#         col = rand_color(100, luminosity = "bright", transparency = 0.75))
# })
#
# circos.initialize(c("a"), xlim = c(0, 1))
# circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
#     panel.fun = function(x, y) {
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
#     x0 = sort(runif(200))
#     x0 = matrix(x0, ncol = 2, byrow = TRUE)
#     x1 = sort(runif(200))
#     x1 = matrix(x1, ncol = 2, byrow = TRUE)
#
#     circos.connect(x0, 0, x1, 1, 
#         type = "normal", border = NA,
#         col = rand_color(100, luminosity = "bright", transparency = 0.5))
# })
#
# circos.initialize(c("a"), xlim = c(0, 1))
# circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
#     panel.fun = function(x, y) {
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
#     circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
#     x0 = sort(runif(500))
#     x0 = matrix(x0, ncol = 2, byrow = TRUE)
#     x0 = x0[sample(nrow(x0), nrow(x0)), ]
#     x1 = sort(runif(500))
#     x1 = matrix(x1, ncol = 2, byrow = TRUE)
#     x1 = x1[sample(nrow(x1), nrow(x1)), ]
#
#     l = abs(x0[, 1] - x1[, 1]) < 0.5
#
#     circos.connect(x0[l ,], 0, x1[l, ], 1, 
#         type = "bezier", border = NA,
#         col = rand_color(sum(l), luminosity = "bright", transparency = 0.5))
# })
# }
circos.connect = function(x0, y0, x1, y1,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    type = c("normal", "segments", "bezier"),
    segments.ratio = c(1, 1, 1),
    col = par("col"),
    border = "black",
    lwd = par("lwd"),
    lty = par("lty"),
    ...) {

    type = match.arg(type)[1]

    if(type == "bezier") {
        if(!requireNamespace("bezier")) {
            stop_wrap("You need to install 'bezier' package from CRAN.")
        }
    }

    if(is.null(nrow(x0))) {
        n1 = length(x0)
    } else {
        x0 = x0[, 1:2]
        n1 = nrow(x0)
    }
    n2 = length(y0)
    if(is.null(nrow(x1))) {
        n3 = length(x1)
    } else {
        x1 = x1[, 1:2]
        n3 = nrow(x1)
    }
    n4 = length(y1)
    n = max(c(n1, n2, n3, n4))
    if(n1 == 1) { 
        if(is.null(nrow(x0))) {
            x0 = rep(x0, n)
        } else {
            x0 = matrix(rep(x0, each = n), ncol = 2)
        }
        n1 = n
    }
    if(n2 == 1) { y0 = rep(y0, n); n2 = n }
    if(n3 == 1) { 
        if(is.null(nrow(x1))) {
            x1 = rep(x1, n)
        } else {
            x1 = matrix(rep(x1, each = n), ncol = 2)
        }
        n3 = n
    }
    if(n4 == 1) { y1 = rep(y1, n); n4 = n }

    if(! (n1 == n2 && n2 == n3 && n3 == n4) ) {
        stop_wrap("x0, y0, x1, y1 should have same length.")
    }

    if(!is.matrix(x0)) x0 = matrix(x0, ncol = 1)
    if(!is.matrix(x1)) x1 = matrix(x1, ncol = 1)

    if(length(col) == 1) col = rep(col, n)
    if(length(border) == 1) border = rep(border, n)
    if(length(lwd) == 1) lwd = rep(lwd, n)
    if(length(lty) == 1) lty = rep(lty, n)


    if(ncol(x0) == 1 && ncol(x1) == 1) {
        if(type == "normal") {
            circos.segments(x0, y0, x1, y1, sector.index = sector.index, track.index = track.index,
                col = col, lwd = lwd, lty = lty, ...)
        } else if(type == "segments") {
            if(length(segments.ratio) == 1) segments.ratio = rep(segments.ratio, 3)
            segments.ratio = segments.ratio[1:3]
            segments.ratio = segments.ratio/sum(segments.ratio)
            for(i in 1:n) {
                w = x1[i] - x0[i]
                h = y1[i] - y0[i]
                circos.segments(x0[i], y0[i], x0[i], y0[i] + h*segments.ratio[1], col = col[i], lwd = lwd[i], lty = lty[i], straight = TRUE, 
                    sector.index = sector.index, track.index = track.index)
                circos.segments(x0[i], y0[i]+h*segments.ratio[1], x1[i], y0[i] + h*sum(segments.ratio[1:2]), col = col[i], lwd = lwd[i], lty = lty[i],
                    sector.index = sector.index, track.index = track.index)
                circos.segments(x1[i], y0[i]+h*sum(segments.ratio[1:2]), x1[i], y1[i], col = col[i], lwd = lwd[i], lty = lty[i], straight = TRUE,
                    sector.index = sector.index, track.index = track.index)
            }
        } else if(type == "bezier") {
            for(i in 1:n) {
                pt = get_bezier_points(x0[i], y0[i], x1[i], y1[i], 
                    xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                    ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                )
                circos.lines(pt[, 1], pt[, 2], col = col[i], lwd = lwd[i], lty = lty[i],
                    sector.index = sector.index, track.index = track.index)
            }
        }
    } else {
        if(type == "segments") {
            stop_wrap("'segments' is only allowed for connections as lines where `x0` and `x1` are both set as vectors.")
        }

        if(type == "normal") {
            for(i in 1:n) {
                if(ncol(x0) == 2 && ncol(x1) == 2) {
                    circos.polygon(c(x0[i, 1], x0[i, 2], x1[i, 2], x1[i, 1], x0[i, 1]),
                                   c(y0[i], y0[i], y1[i], y1[i], y0[i]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } else if(ncol(x0) == 2 && ncol(x1) == 1) {
                    circos.polygon(c(x0[i, 1], x0[i, 2], x1[i, 1], x0[i, 1]),
                                   c(y0[i], y0[i], y1[i], y0[i]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } else if(ncol(x0) == 2 && ncol(x1) == 2) {
                    circos.polygon(c(x0[i, 1], x1[i, 2], x1[i, 1], x0[i, 1]),
                                   c(y0[i], y1[i], y1[i], y0[i]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } 
            }
        } else if(type == "bezier") {
            for(i in 1:n) {
                if(ncol(x0) == 2 && ncol(x1) == 2) {
                    pt1 = get_bezier_points(x0[i, 2], y0[i], x1[i, 2], y1[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    pt2 = get_bezier_points(x1[i, 1], y1[i], x0[i, 1], y0[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    circos.polygon(c(pt1[, 1], pt2[, 1], pt1[1, 1]),
                                   c(pt1[, 2], pt2[, 2], pt1[1, 2]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } else if(ncol(x0) == 2 && ncol(x1) == 1) {
                    pt1 = get_bezier_points(x0[i, 2], y0[i], x1[i, 1], y1[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    pt2 = get_bezier_points(x1[i, 1], y1[i], x0[i, 1], y0[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    circos.polygon(c(pt1[, 1], pt2[, 1], pt1[1, 1]),
                                   c(pt1[, 2], pt2[, 2], pt1[1, 2]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } else if(ncol(x0) == 2 && ncol(x1) == 2) {
                    pt1 = get_bezier_points(x0[i, 1], y0[i], x1[i, 2], y1[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    pt2 = get_bezier_points(x1[i, 1], y1[i], x0[i, 1], y0[i], 
                        xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index),
                        ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
                    )
                    circos.polygon(c(pt1[, 1], pt2[, 1], pt1[1, 1]),
                                   c(pt1[, 2], pt2[, 2], pt1[1, 2]),
                                   col = col[i], border = border[i], lwd = lwd[i], lty = lty[i], 
                                   sector.index = sector.index, track.index = track.index)
                } 
            }
        }
    }
}


# == title
# Add a label track
#
# == param
# -sectors A vector of sector names.
# -x Positions of the labels.
# -labels A vector of labels.
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
# This function creates two tracks, one for the connection lines and one for the labels.
#
# If two labels are too close and overlap, this function automatically adjusts the positions of neighouring labels.
circos.labels = function(
    sectors, x, labels, 
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

    if(missing(sectors) && missing(x)) {
        env = circos.par("__tempenv__")
        if(identical(env$circos.heatmap.initialized, TRUE)) {
            subset = unlist(lapply(env$sector.meta.data, function(x) x$subset))
            x = unlist(lapply(env$sector.meta.data, function(x) x$cell_middle))
            labels = labels[subset]
            sectors = rep(names(env$sector.meta.data), times = sapply(env$sector.meta.data, function(x) length(x$subset)))
        }
    }
    bed = data.frame(sectors, x, x)

    circos.genomicLabels(bed, labels = labels, facing = facing, niceFacing = niceFacing, col = col, cex = cex,
        font = font, padding = padding, connection_height = connection_height, line_col = line_col,
        line_lwd = line_lwd, line_lty = line_lty, labels_height = labels_height, side = side, labels.side = labels.side,
        track.margin = track.margin)
}

