# == title
# Convert to polar coordinate system
#
# == param
# -x            Data points on x-axis. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.
# -y            Data points on y-axis.
# -sector.index Index for the sector to convert the coordinates
# -track.index  Index for the track to convert the coordinates
#
# == details
# This is the core function in the package. It transform data points from data coordinate system (in a specific cell) to the polar coordinate system.
#
# == values
# A matrix with two columns (``theta`` and ``rou``). ``rou`` is measured in degree.
#
circlize = function(x, y, sector.index = get.current.sector.index(),
	track.index = get.current.track.index()) {

    if(missing(y)) {
        if(ncol(x) >= 2) {
            y = x[, 2]
            x = x[, 1]
        }
    }
    
    sector.data = get.sector.data(sector.index)
       
    theta = sector.data["start.degree"] - (x - sector.data["min.value"]) / (sector.data["max.value"] - sector.data["min.value"]) *
            abs(sector.data["start.degree"] - sector.data["end.degree"])
    
	if(track.index == 0) {
		rou = rep(1, length(theta))
	} else {
		cell.data = get.cell.data(sector.index, track.index)
		cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)  
		y.range = cell.ylim[2] - cell.ylim[1] 
		rou = cell.data$track.start - (cell.ylim[2] - y) / y.range * cell.data$track.height
    }
	
    m = cbind(theta, rou)
    colnames(m) = c("theta", "rou")
    rownames(m) = NULL
    
    return(m)
}

# == title
# Convert to data coordinate system
#
# == param
# -x   degree values. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.
# -y   distance to the circle center (the radius)
# -sector.index Index for the sector where the data coordinate is used
# -track.index  Index for the track where the data coordinate is used
#
# == details
# This is the reverse function of `circlize`. It transform data points from polar coordinate system to a specified data coordinate system.
#
# == values
# A matrix with two columns (``x`` and ``y``)
reverse.circlize = function(x, y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    if(missing(y)) {
        if(ncol(x) >= 2) {
            y = x[, 2]
            x = x[, 1]
        }
    }

    theta = x
    rou = y

	sector.data = get.sector.data(sector.index)
    if(track.index > 0) {
        cell.data = get.cell.data(sector.index, track.index)
    	cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
	}

	x = (sector.data["start.degree"] - theta) / abs(sector.data["end.degree"] - sector.data["start.degree"]) *
	    (sector.data["max.value"] - sector.data["min.value"]) + sector.data["min.value"]
	if(track.index > 0) {
        y = (cell.data$track.height - (cell.data$track.start - rou)) / cell.data$track.height * (cell.ylim[2] - cell.ylim[1]) + cell.ylim[1]
	} else {
        y = rep(1, length(x))
    }
	m = cbind(x, y)
	colnames(m) = c("x", "y")
	rownames(m) = NULL
	return(m)
}

polar2Cartesian = function(d) {
    theta = as.radian(d[, 1])
    rou = d[, 2]
    x = rou * cos(theta)
    y = rou * sin(theta)
    return(cbind(x, y))
}

# expand breakpoints in two points to draw an arc
# x and y are transformed and re-mapped points
lines.expand = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
    sector.data = get.sector.data(sector.index)
	cell.data = get.cell.data(sector.index, track.index)
    nx = x[1]
    ny = y[1]
	
    for(i in seq_along(x)) {
        if(i == 1) {
            next   
        }
        if(is.na(x[i]) || is.na(y[i])) {
        	nx = c(nx, NA)
        	ny = c(ny, NA)
        	next
        }
        if(is.na(x[i-1]) || is.na(y[i-1])) {
        	next
        }
	
		td = cbind(c(x[i-1], x[i]), c(y[i-1], y[i]))
        td = td[order(td[, 1]), ]
		td2 = circlize(td[, 1], td[, 2], sector.index = sector.index, track.index = track.index)
		
		a = as.radian(abs(td2[1, 1] - td2[2, 1]))
		b = abs(td2[1, 2] - td2[2, 2])
		l = sqrt(a^2 + b^2)
		
		ncut = l/ (2*pi/circos.par("unit.circle.segments"))
        ncut = floor(ncut)

		if(ncut) {
			j = seq_len(ncut) / (ncut + 1)
				
			nx = c(nx, x[i-1] + (x[i] - x[i-1])*j, x[i])
			ny = c(ny, y[i-1] + (y[i] - y[i-1])*j, y[i])
		} else {
            nx = c(nx, x[i])
            ny = c(ny, y[i])
        }
    }
   
    d = cbind(nx, ny)
    return(d)
}

# if x has same length as levels of factors
# the order of x is same as the order of levels
recycle.with.factors = function(x, factors) {
    le = levels(factors)
    if(length(x) == 1) {
        x = rep(x, length(factors))
    } else if(length(x) == length(le)) {
        b = factors
        levels(b) = x
        x = as.vector(b)
    }
    return(x)
}

recycle.with.levels = function(x, levels) {
    
    if(length(x) == 1) {
        x = rep(x, length(levels))
    } 
    return(x)
}

check.track.position = function(track.index, track.start, track.height) {

    track.margin = circos.par("track.margin")
    if(track.start - track.height - track.margin[2] < 0 ||
       track.start - track.height < 0 ||
       track.start < 0) {
        stop_wrap(paste("not enough space for cells at track index '", track.index, "'.\n", sep = ""))
    }
    if(track.start - track.margin[1] - track.height - track.margin[2] < 0) {
        stop_wrap(paste("not enough space for bottom margin of cells at track index '", track.index, "'.\n", sep = ""))
    }
    
    if(track.index > 1) {
        
        if(track.start > get.cell.meta.data("cell.bottom.radius", track.index = track.index - 1)) {
            stop_wrap("Track overlaps with previous track.\n")
        }
    }
}

check.points.position = function(x, y, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")) {
        
    cell.xlim = get.cell.meta.data("cell.xlim", sector.index, track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
    
    l = is.na(x) | is.na(y)
    x = x[!l]
    y = y[!l]

    xrange = cell.xlim[2] - cell.xlim[1]
    yrange = cell.ylim[2] - cell.ylim[1]
    l1 = x < cell.xlim[1] | x > cell.xlim[2]
    l2 = y < cell.ylim[1] | y > cell.ylim[2]
    l = l1 | l2
    if(sum(l) && circos.par("points.overflow.warning")) {
        message_wrap(paste("Note: ", sum(l), " point", ifelse(sum(l) == 1, " is", "s are"), " out of plotting region in sector '", sector.index, "', track '", track.index, "'.\n", sep = ""))
    }

    return(invisible(NULL))
}

as.radian = function(degree) {
	return(degree/180*pi)
}

as.degree = function(radian) {
	return(radian/pi*180)
}


# == title
# Color interpolation
#
# == param
# -breaks A vector indicating numeric breaks
# -colors A vector of colors which correspond to values in ``breaks``
# -transparency A single value in ``[0, 1]``. 0 refers to no transparency and 1 refers to full transparency
# -space color space in which colors are interpolated. Value should be one of "RGB", "HSV", "HLS", "LAB", "XYZ", "sRGB", "LUV", see `colorspace::color-class` for detail.
#
# == details
# Colors are linearly interpolated according to break values and corresponding colors through CIE Lab color space (`colorspace::LAB`) by default. 
# Values exceeding breaks will be assigned with corresponding maximum or minimum colors.
#
# == values
# It returns a function which accepts a vector of numeric values and returns interpolated colors.
#
# == seealso
# `col2value` converts back to the original values by providing the color mapping function generated by `colorRamp2`.
#
# == example
# col_fun = colorRamp2(c(-1, 0, 1), c("green", "white", "red"))
# col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))
colorRamp2 = function(breaks, colors, transparency = 0, space = "LAB") {

  if(length(breaks) != length(colors)) {
    stop_wrap("Length of `breaks` should be equal to `colors`.\n")
  }
  
  colors = colors[order(breaks)]
  breaks = sort(breaks)

  l = duplicated(breaks)
  breaks = breaks[!l]
  colors = colors[!l]

  if(length(breaks) == 1) {
    stop_wrap("You should have at least two distinct break values.")
  } 


  if(! space %in% c("RGB", "HSV", "HLS", "LAB", "XYZ", "sRGB", "LUV")) {
    stop_wrap("`space` should be in 'RGB', 'HSV', 'HLS', 'LAB', 'XYZ', 'sRGB', 'LUV'")
  }
  
  colors = t(col2rgb(colors)/255)

  attr = list(breaks = breaks, colors = colors, transparency = transparency, space = space)

  if(space == "LUV") {
    i = which(apply(colors, 1, function(x) all(x == 0)))
    colors[i, ] = 1e-5
  }
  
  transparency = 1-ifelse(transparency > 1, 1, ifelse(transparency < 0, 0, transparency))[1]
  transparency_str = sprintf("%X", round(transparency*255))
  if(nchar(transparency_str) == 1) transparency_str = paste0("0", transparency_str)
  
  fun = function(x = NULL, return_rgb = FALSE, max_value = 1) {
    if(is.null(x)) {
      stop_wrap("Please specify `x`\n")
    }

    att = attributes(x)
    if(is.data.frame(x)) x = as.matrix(x)

    l_na = is.na(x)
    if(all(l_na)) {
      return(rep(NA, length(l_na)))
    }

    x2 = x[!l_na]
    
    x2 = ifelse(x2 < breaks[1], breaks[1],
               ifelse(x2 > breaks[length(breaks)], breaks[length(breaks)],
                      x2
               ))
    ibin = .bincode(x2, breaks, right = TRUE, include.lowest = TRUE)
    res_col = character(length(x2))
    for(i in unique(ibin)) {
      l = ibin == i
      res_col[l] = .get_color(x2[l], breaks[i], breaks[i+1], colors[i, ], colors[i+1, ], space = space)
    }
    res_col = paste(res_col, transparency_str[1], sep = "")
    
    if(return_rgb) {
      res_col = t(col2rgb(as.vector(res_col), alpha = TRUE)/255)
    }
    res_col2 = character(length(x))
    res_col2[l_na] = NA
    res_col2[!l_na] = res_col

    attributes(res_col2) = att
    return(res_col2)
  }
  
  attributes(fun) = attr
  return(fun)
}

.restrict_in = function(x, lower, upper) {
  x[x > upper] = upper
  x[x < lower] = lower
  x
}

# x: vector
# break1 single value
# break2 single value
# rgb1 vector with 3 elements
# rgb2 vector with 3 elements
.get_color = function(x, break1, break2, col1, col2, space) {

  col1 = coords(as(sRGB(col1[1], col1[2], col1[3]), space))
  col2 = coords(as(sRGB(col2[1], col2[2], col2[3]), space))

  res_col = matrix(ncol = 3, nrow = length(x))
  for(j in 1:3) {
    xx = (x - break2)*(col2[j] - col1[j]) / (break2 - break1) + col2[j]
    res_col[, j] = xx
  }
  
  res_col = get(space)(res_col)
  res_col = coords(as(res_col, "sRGB"))
  res_col[, 1] = .restrict_in(res_col[,1], 0, 1)
  res_col[, 2] = .restrict_in(res_col[,2], 0, 1)
  res_col[, 3] = .restrict_in(res_col[,3], 0, 1)
  hex(sRGB(res_col))
}

# will be considered in the future
circos.approx = function(x, y, resolution = 0.1, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"),
	approxFun = function(x) sample(x, 1)) {
	
	od = order(x)
	x = x[od]
	y = y[od]
	
	xplot = get.cell.meta.data("xplot", sector.index = sector.index, track.index = track.index)
	cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
	
	window.size = resolution/(xplot[1] - xplot[2])*(cell.xlim[2] - cell.xlim[1])
	window = seq(cell.xlim[1], cell.xlim[2], by = window.size)
	
	newx = rep(NA, length(x))
	newy = rep(NA, length(y))
	
	for(i in seq_along(window)[-1]) {
		l = x >= window[i-1] & x < window[i]
		# if there are points in current window
		if(sum(l)) {
			newx[i] = (window[i-1] + window[i])/2
			newy[i] = approxFun(y[l])
		}
	}
	
	newx = newx[!is.na(newx)]
	newy = newy[!is.na(newy)]
	
	return(list(x = newx, y = newy))
}

# == title
# Add transparency to colors
#
# == param
# -col a vector of colors
# -transparency transparency, numeric value between 0 and 1
#
# == value
# A vector of colors
#
# == example
# add_transparency("red", 0.5)
# add_transparency(1, 0.5)
# add_transparency("#FF000080", 0.2)
add_transparency = function (col, transparency = 0) {
    rgb(t(col2rgb(col)/255), alpha = 1 - transparency)
}

get_most_inside_radius = function() {
	tracks = get.all.track.index()
	if(length(tracks) == 0) {
	    1
	} else {
	    n = length(tracks)
	    get.cell.meta.data("cell.bottom.radius", track.index = tracks[n]) - get.cell.meta.data("track.margin", track.index = tracks[n])[1] - circos.par("track.margin")[2]
	}
}

# == title
# Convert adjacency list to adjacency matrix
#
# == param
# -lt a data frame which contains adjacency list.
# -square is the returned matrix a square matrix?
#
# == details
# Convert adjacency list to adjacency matrix.
#
adjacencyList2Matrix = function(lt, square = FALSE) {
	lt = as.data.frame(lt)
	if(ncol(lt) == 2) {
		lt = cbind(lt, rep(1, nrow(lt)))
	}
	if(ncol(lt) < 3) {
		stop_wrap("`lt` should be a data frame with three columns")
	}

	if(!is.numeric(lt[[3]])) {
		stop_wrap("Third column in `lt` should be numeric.")
	}

	lt[[1]] = as.vector(lt[[1]])
	lt[[2]] = as.vector(lt[[2]])

	rn = unique(lt[[1]])
	cn = unique(lt[[2]])

	if(square) {
		nm = union(rn, cn)
		mat = matrix(0, ncol = length(nm), nrow = length(nm))
		rownames(mat) = nm
		colnames(mat) = nm
	} else {
		mat = matrix(0, ncol = length(cn), nrow = length(rn))
		rownames(mat) = rn
		colnames(mat) = cn
	}

	for(i in seq_len(nrow(lt))) {
		mat[lt[i, 1], lt[i, 2]] = lt[i, 3]
	}

	return(mat)
}



# == title
# Transform back from colors to values
#
# == param
# -r red channel in `colorspace::sRGB` color space, value should be between 0 and 1. 
#    The ``r``, ``g`` and ``b`` argumentc can be wrapped into one variable which is either a three-column matrix
#    or a vector of colors.
# -g green channel in `colorspace::sRGB` color space, value should be between 0 and 1.
# -b blue channel in `colorspace::sRGB` color space, value should be between 0 and 1.
# -col_fun the color mapping function generated by `colorRamp2`.
#
# == details
# `colorRamp2` transforms values to colors and this function does the reversed job.
# Note for some color spaces, it cannot transform back to the original value perfectly.
#
# == value
# A vector of original numeric values.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# x = seq(0, 1, length = 11)
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# col = col_fun(x)
# col2value(col, col_fun = col_fun)
# col2value("red", col_fun = col_fun)
#
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"), space = "sRGB")
# col = col_fun(x)
# col2value(col, col_fun = col_fun)
col2value = function(r, g, b, col_fun) {
    if(missing(g) && missing(b)) {
        if(is.character(r)) {
            rgb = col2rgb(r)/255
            r = rgb[1, ]
            g = rgb[2, ]
            b = rgb[3, ]
        } else {
            if(is.list(r)) {
                g = r[[2]]
                b = r[[3]]
                r = r[[1]]
            } else if(is.data.frame(r) || is.matrix(r)) {
                g = r[, 2]
                b = r[, 3]
                r = r[, 1]
            }
        }
    }

    breaks = attr(col_fun, "breaks")
    colors = attr(col_fun, "colors")
    space = attr(col_fun, "space")

    n = length(r)

    ## convert all colors to the specified space
    m = coords(as(sRGB(r, g, b), space))
    breaks_m = coords(as(sRGB(colors), space))
    n_breaks = length(breaks)

    # interpolation for the three channels seperatedly
    v = numeric()
    for(i in seq_len(nrow(m))) {
        l1 = (breaks_m[-n_breaks, 1] <= m[i, 1] & breaks_m[-1, 1] >= m[i, 1]) | (breaks_m[-n_breaks, 1] >= m[i, 1] & breaks_m[-1, 1] <= m[i, 1])
        l2 = (breaks_m[-n_breaks, 2] <= m[i, 2] & breaks_m[-1, 2] >= m[i, 2]) | (breaks_m[-n_breaks, 2] >= m[i, 2] & breaks_m[-1, 2] <= m[i, 2])
        l3 = (breaks_m[-n_breaks, 3] <= m[i, 3] & breaks_m[-1, 3] >= m[i, 3]) | (breaks_m[-n_breaks, 3] >= m[i, 3] & breaks_m[-1, 3] <= m[i, 3])
        k = which(l1 & l2 & l3)[1]
        v1 = (breaks[k] - breaks[k+1])/(breaks_m[k, 1] - breaks_m[k+1, 1]) * (m[i, 1] - breaks_m[k, 1]) + breaks[k]
        v2 = (breaks[k] - breaks[k+1])/(breaks_m[k, 2] - breaks_m[k+1, 2]) * (m[i, 2] - breaks_m[k, 2]) + breaks[k]
        v3 = (breaks[k] - breaks[k+1])/(breaks_m[k, 3] - breaks_m[k+1, 3]) * (m[i, 3] - breaks_m[k, 3]) + breaks[k]
        vv = c(v1, v2, v3)
        v[i] = mean(vv, na.rm = TRUE)
    }
    return(v)
}

# == title
# Convert units
#
# == param
# -x a numeric vector
# -unit supported units, only "mm", "cm", "inches".
#
# == details
# This function coverts mm/cm/inches units to units measured in the canvas coordinate,
# e.g. how much is it in the canvas coordinate for 1 mm/cm/inches.
#
# Since in the circular plot, the aspect ratio is always 1, it does not matter this conversion
# is applied on x direction or y direction.
#
# == seealso
# `convert_x` and `convert_y` convert absolute units into a data coordinate in a specified cell.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# fa = letters[1:10]
# circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
# circos.initialize(fa, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
# circos.track(ylim = c(0, 1), track.height = convert_length(5, "mm"))
# circos.par(track.margin = c(0, convert_length(2, "mm")))
# circos.track(ylim = c(0, 1), track.height = convert_length(1, "cm"))
# circos.par(track.margin = c(0, convert_length(5, "mm")))
# circos.track(ylim = c(0, 1), track.height = convert_length(1, "inches"))
# circos.clear()
convert_length = function(x, unit = c("mm", "cm", "inches")) {

	pin = par("pin")
	usr = par("usr")

	unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
	pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_length()` only works when aspect of the coordinate is 1.")
    }

	inche_per_mm = 0.0393700787401575
    # length in the data coordinate
	if(unit == "inches") {
		len = x * pt_per_inche1
	} else if(unit == "mm") {
		len = x * pt_per_inche1 * inche_per_mm
	} else if(unit == "cm") {
		len = x * pt_per_inche1 * inche_per_mm * 10
	}

    return(len)
}

# == title
# Convert units
#
# == param
# -... pass to `convert_length`
#
# == details
# This function is same as `convert_length`. The reason for naming this function
# is `convert_length` is mostely used for defining the height of tracks and track margins.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example in `convert_length` page
# NULL
convert_height = function(...) {
    convert_length(...)
}

# == title
# Convert units
#
# == param
# -... pass to `convert_length`
#
# == details
# This function is same as `convert_length`.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example in `convert_length` page
# NULL
uh = function(...) {
    convert_length(...)
}

convert_unit_in_data_coordinate = function(x, unit = c("mm", "cm", "inches", "canvas"),
    direction = c("x", "y"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    h = get.cell.meta.data("ycenter", sector.index = sector.index, 
        track.index = track.index)) {

    pin = par("pin")
    usr = par("usr")

    unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
    pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_unit_in_data_coordinate()` only works when aspect of the coordinate is 1.")
    }

    direction = match.arg(direction)

    inche_per_mm = 0.0393700787401575
    # length in the data coordinate
    if(unit == "inches") {
        len = x * pt_per_inche1
    } else if(unit == "mm") {
        len = x * pt_per_inche1 * inche_per_mm
    } else if(unit == "cm") {
        len = x * pt_per_inche1 * inche_per_mm * 10
    } else if(unit == "canvas") {
        len = x
    }

    xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index)
    ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)

    
    if(direction == "x") {
        if(length(h) == 1) {
            h = rep(h, length(x))
        }
        w_in_data = xlim[2] - xlim[1]
        w_in_canvas = numeric(length(x))
        for(i in seq_along(x)) {
            coor_polar = circlize(xlim, c(h[i], h[i]), sector.index = sector.index, track.index = track.index)
            w_in_canvas[i] = abs(coor_polar[1, 1] - coor_polar[2, 1])/180*pi* coor_polar[1, 2]
        }
    } else {
        coor_polar = circlize(c(xlim[1], xlim[1]), ylim, sector.index = sector.index, track.index = track.index)
        w_in_data = ylim[2] - ylim[1]
        w_in_canvas = abs(coor_polar[2, 2] - coor_polar[1, 2])
    }

    len = len * abs(w_in_data/w_in_canvas)
    return(len)
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -x a numeric vector
# -unit supported units, only "mm", "cm", "inches"
# -sector.index index for the sector where the conversion is applied
# -track.index index for the track where the conversion is applied
# -h since the width of the cell is not identical from the top to the bottom in the cell, the position on
#   y direction needs to be specified. By default it is at the middle point on y-axis
#
# == value
# A vector of numeric values which are measured in the specified data coordinate
#
# == seealso
# `convert_y` converts on y direction.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# fa = letters[1:10]
# circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
# circos.initialize(fa, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
# circos.track(ylim = c(0, 1), track.height = convert_height(5, "mm"),
#     panel.fun = function(x, y) {
#         circos.lines(c(0, 0 + convert_x(5, "mm")), c(0.5, 0.5), col = "blue")
#     })
# circos.par(track.margin = c(0, convert_height(2, "mm")))
# circos.track(ylim = c(0, 1), track.height = convert_height(1, "cm"),
#     panel.fun = function(x, y) {
#         xcenter = get.cell.meta.data("xcenter")
#         circos.lines(c(xcenter, xcenter), c(0, convert_y(1, "cm")), col = "red")
#     })
# circos.par(track.margin = c(0, convert_height(5, "mm")))
# circos.track(ylim = c(0, 1), track.height = convert_height(1, "inches"),
#     panel.fun = function(x, y) {
#         line_length_on_x = convert_x(1*sqrt(2)/2, "cm")
#         line_length_on_y = convert_y(1*sqrt(2)/2, "cm")
#         circos.lines(c(0, line_length_on_x), c(0, line_length_on_y), col = "orange")
#     })
# circos.clear()
convert_x = function(x, unit = c("mm", "cm", "inches"),
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    h = get.cell.meta.data("ycenter", sector.index = sector.index, 
        track.index = track.index)) {
    convert_unit_in_data_coordinate(x, unit = unit, sector.index = sector.index, 
        track.index = track.index, h = h, direction = "x")
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -... pass to `convert_x`
#
# == details
# This function is same as `convert_x`.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example in `convert_x` page
# NULL
ux = function(...) {
    convert_x(...)
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -x a numeric vector
# -unit supported units, only "mm", "cm", "inches"
# -sector.index index for the sector where the conversion is applied
# -track.index index for the track where the conversion is applied
#
# == value
# A vector of numeric values which are measured in the specified data coordinate
#
# == seealso
# `convert_x` converts on x direction.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example on `convert_x` page 
# NULL
convert_y = function(x, unit = c("mm", "cm", "inches"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
    convert_unit_in_data_coordinate(x, unit = unit, sector.index = sector.index, 
        track.index = track.index, direction = "y")
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -... pass to `convert_y`
#
# == details
# This function is same as `convert_y`.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example in `convert_y` page
# NULL
uy = function(...) {
    convert_y(...)
}

convert_unit_in_canvas_coordinate = function(x, unit = c("mm", "cm", "inches")) {

    pin = par("pin")
    usr = par("usr")

    unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
    pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_unit_in_data_coordinate()` only works when aspect of the coordinate is 1.")
    }

    inche_per_mm = 0.0393700787401575
    # length in the data coordinate
    if(unit == "inches") {
        len = x * pt_per_inche1
    } else if(unit == "mm") {
        len = x * pt_per_inche1 * inche_per_mm
    } else if(unit == "cm") {
        len = x * pt_per_inche1 * inche_per_mm * 10
    }
    return(len)
}

stop_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x, call. = FALSE)
}

message_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}

validate_data_frame = function(x) {
    if(inherits(x, "data.frame")) {
        return(x)
    } else if(inherits(x, "GRanges")) {
        x = as.data.frame(x)
        return(x[, -(4:5), drop = FALSE])
    } else {
        oe = try(x <- as.data.frame(x))
        if(inherits(oe, "try-error")) {
            stop_wrap("The input should be a data frame or an object that can be converted to a data frame.")
        }
        return(x)
    }
}
