# == title
# Return the coordinate in polar coordinate system
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
#
# == details
# This is the core function in the package. It transform data points from data coordinate system to polar coordinate system.
#
# == values
# A matrix with two columns (``theta`` and ``rou``)
circlize = function(x, y, sector.index = get.current.sector.index(),
	track.index = get.current.track.index()) {
    
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
# Return the coordinate in data coordinate system
#
# == param
# -theta        measured by degree
# -rou          distance to the circle center (radius)
# -sector.index Index for the sector
# -track.index  Index for the track
#
# == details
# This is the reverse function of `circlize`. It transform data points from polar coordinate system to data coordinate system.
#
# == values
# A matrix with two columns (``x`` and ``y``)
reverse.circlize = function(theta, rou, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
	sector.data = get.sector.data(sector.index)
    cell.data = get.cell.data(sector.index, track.index)
	cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
	
	x = (sector.data["start.degree"] - theta) / abs(sector.data["end.degree"] - sector.data["start.degree"]) *
	    (sector.data["max.value"] - sector.data["min.value"]) + sector.data["min.value"]
	y = (cell.data$track.height - (cell.data$track.start - rou)) / cell.data$track.height * (cell.ylim[2] - cell.ylim[1]) + cell.ylim[1]
	
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
        stop(paste("not enough space for cells at track index '", track.index, "'.\n", sep = ""))
    }
    if(track.start - track.margin[1] - track.height - track.margin[2] < 0) {
        stop(paste("not enough space for bottom margin of cells at track index '", track.index, "'.\n", sep = ""))
    }
    
    if(track.index > 1) {
        
        if(track.start > get.cell.meta.data("cell.bottom.radius", track.index = track.index - 1)) {
            stop("Track overlaps with previous track.\n")
        }
    }
}

check.points.position = function(x, y, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")) {
        
    cell.xlim = get.cell.meta.data("cell.xlim", sector.index, track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
    
    l1 = x < cell.xlim[1] | x > cell.xlim[2]
    l2 = y < cell.ylim[1] | y > cell.ylim[2]
    l = l1 | l2
    if(sum(l) && circos.par("points.overflow.warning")) {
        cat(paste("Note: ", sum(l), " point", ifelse(sum(l) == 1, " is", "s are"), " out of plotting region in sector '", sector.index, "', track '", track.index, "'.\n", sep = ""))
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
# -transparency A single value in [0, 1]. 0 refers to no transparency and 1 refers to full transparency
#
# == details
# Colors are interpolated according to break values and corresponding colors. Values exceeding breaks will be assigned with maximum or minimum colors.
#
# == values
# It returns a function which accepts a vector of numbers and returns interpolated colors.
colorRamp2 = function(breaks, colors, transparency = 0) {
    if(length(breaks) != length(colors)) {
        stop("Length of `breaks` should be equal to `colors`.\n")
    }
	
	if(length(unique(breaks)) != length(breaks)) {
		stop("Duplicated values are not allowed in `breaks`\n")
	}

    colors = colors[order(breaks)]
	colors = col2rgb(colors)
    breaks = sort(breaks)
	
    transparency = ifelse(transparency > 1, 1, ifelse(transparency < 0, 0, transparency))

    function(x) {
		att = attributes(x)
        x = ifelse(x < breaks[1], breaks[1],
                  ifelse(x > breaks[length(breaks)], breaks[length(breaks)],
                        x
                    ))
		ibin = .bincode(x, breaks, right = TRUE, include.lowest = TRUE)
		res_col = character(length(x))
		for(i in unique(ibin)) {
			l = ibin == i
			res_col[l] = .get_color(x[l], breaks[i], breaks[i+1], colors[, i], colors[, i+1], transparency)
		}
		attributes(res_col) = att
		return(res_col)
    }
}

# x: vector
# break1 single value
# break2 single value
# rgb1 vector with 3 elements
# rgb2 vector with 3 elements
.get_color = function(x, break1, break2, rgb1, rgb2, transparency) {
	res_rgb = matrix(nrow = 3, ncol = length(x))
	for(i in seq_along(x)) {
		res_rgb[, i] = (x[i] - break2)*(rgb2 - rgb1) / (break2 - break1) + rgb2
	}
	return(rgb(t(res_rgb)/255, alpha = 1-transparency))
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
# generate random colors
#
# == param
# -n number of colors
# -transparency transparency, numeric value between 0 and 1
#
# == value
# a vector of colors
rand_color = function(n = 1, transparency = 0) {
    return(rgb(runif(n), runif(n), runif(n), 1 - transparency))
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
