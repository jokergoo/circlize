# return the coordinate in polar coordinate system in a specified cell
circlize = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
    
    sector.data = get.sector.data(sector.index)
    cell.data = get.cell.data(sector.index, track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
        
    theta = sector.data["start.degree"] - (x - sector.data["min.value"]) / (sector.data["max.value"] - sector.data["min.value"]) *
            abs(sector.data["start.degree"] - sector.data["end.degree"])
        
    y.range = cell.ylim[2] - cell.ylim[1]
        
    rou = cell.data$track.start - (cell.ylim[2] - y) / y.range * cell.data$track.height
    
    m = cbind(theta, rou)
    colnames(m) = c("theta", "rou")
    rownames(m) = NULL
    
    return(m)
}

# reverse function of circlize
reverse.circlize = function(theta, rou, sector.index, track.index) {
	sector.data = get.sector.data(sector.index)
    cell.data = get.cell.data(sector.index, track.index)
	cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
	
	x = (sector.data["start.degree"] - theta) / abs(sector.data["end.degree"] - sector.data["start.degree"]) *
	    (sector.data["max.value"] - sector.data["min.value"]) + sector.data["min.value"]
	y = (cell.data$track.height - (cell.data$track.start - rou)) / cell.data$track.height * (cell.ylim[2] - cell.ylim[1]) + cell.ylim[1]
	
	m = cbind(x, y)
	colnames(m) = c("x", "y")
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

check.track.position = function(trace.index, track.start, track.height) {

    track.margin = circos.par("track.margin")
    if(track.start - track.height - track.margin[2] < 0 ||
       track.start - track.height < 0 ||
       track.start < 0) {
        stop(paste("not enough space for plotting region of track index '", trace.index, "'.\n", sep = ""))
    }
    if(track.start - track.margin[1] - track.height - track.margin[2] < 0) {
        stop(paste("not enough space for bottom margin of track index '", trace.index, "'.\n", sep = ""))
    }
    
    if(trace.index > 1) {
        
        if(track.start > get.track.end.position(trace.index - 1)) {
            stop("Plotting region overlaps with previous track.\n")
        }
    }
}

check.points.position = function(x, y, sector.index = NULL, track.index = NULL) {
    
    if(is.null(sector.index)) {
        sector.index = get.current.sector.index()   
    }

    if(is.null(track.index)) {
        track.index = get.current.track.index()
    }
    
    cell.xlim = get.cell.meta.data("cell.xlim", sector.index, track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
    
    l1 = x < cell.xlim[1] | x > cell.xlim[2]
    l2 = y < cell.ylim[1] | y > cell.ylim[2]
    l = l1 | l2
    if(sum(l) && circos.par("points.overflow.warning")) {
        warning(paste(sum(l), " point", ifelse(sum(l) == 1, " is", "s are"), " out of plotting region in sector '", sector.index, "', track '", track.index, "'.\n", sep = ""))
    }

    return(invisible(NULL))
}

as.radian = function(degree) {
	return(degree/180*pi)
}

as.degree = function(radian) {
	return(radian/pi*180)
}