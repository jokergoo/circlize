# return the coordinate in polar coordinate system in a specified cell
circlize = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
    
    sector.data = get.sector.data(sector.index)
    cell.data = get.cell.data(sector.index, track.index)
    ylim = cell.data$ylim
        
    theta = sector.data["end.degree"] - (x - sector.data["start.value"]) / (sector.data["end.value"] - sector.data["start.value"]) *
            (sector.data["end.degree"] - sector.data["start.degree"])
        
    y.range = ylim[2] - ylim[1]
        
    rou = cell.data$track.start - (ylim[2] - y) / y.range * cell.data$track.height
    
    m = cbind(theta, rou)
    colnames(m) = c("theta", "rou")
    rownames(m) = NULL
    
    return(m)
}

reverse.circlize = function(theta, rou, sector.index, track.index) {
	sector.data = get.sector.data(sector.index)
    cell.data = get.cell.data(sector.index, track.index)
	ylim = cell.data$ylim
	
	x = (sector.data["end.degree"] - theta) / (sector.data["end.degree"] - sector.data["start.degree"]) *
	    (sector.data["end.value"] - sector.data["start.value"]) + sector.data["start.value"]
	y = (cell.data$track.height - (cell.data$track.start - rou)) / cell.data$track.height * (ylim[2] - ylim[1]) + ylim[1]
	
	m = cbind(x, y)
	colnames(m) = c("x", "y")
	return(m)
}

degree.add = function(theta1, theta2) {
    return((theta1 + theta2) %% 360)
}

# reverse clockwise
# should only deal with start.degree and end.degree
degree.minus = function(to, from, min.zero = TRUE) {
	if(min.zero) {
		return((to - from) %% 360)
	} else {
		if((to - from) %% 360 == 0) {
			return(360)
		} else {
			return((to - from) %% 360)
		}
	}
}

degree.seq = function(from, to, length.out = 2) {
    per = degree.minus(to, from) / (length.out - 1)
    s = numeric(length.out)
    s[1] = from
    for(i in seq_along(s)) {
        if(i == 1) {
            next
        }
        s[i] = degree.add(s[i - 1], per)
    }
    return(s)
}

polar2Cartesian = function(d) {
    theta = d[, 1]/360 * 2 *pi
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
		td2 = circlize(td[, 1], td[, 2], sector.index = sector.index, track.index = track.index)
		td3 = polar2Cartesian(td2)
		ncut = sqrt((td3[2, 1] - td3[1, 1])^2 + (td3[2, 2] - td3[2, 1])^2)/ (2*pi/circos.par("unit.circle.segments"))
        ncut = floor(ncut)

		if(ncut) {
				j = seq_len(ncut) / (ncut + 1)
				
				nx = c(nx, x[i-1] + (x[i] - x[i-1])*j, x[i])
				ny = c(ny, y[i-1] + (y[i] - y[i-1])*j, y[i])
		}
    }
    
    return(cbind(nx, ny))
    
}

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
    
    cell.data = get.cell.data(sector.index, track.index)
    xlim = cell.data$xlim
    ylim = cell.data$ylim
    
    l1 = x < xlim[1] | x > xlim[2]
    l2 = y < ylim[1] | y > ylim[2]
    l = l1 | l2
    if(sum(l) && circos.par("points.overflow.warning")) {
        warning(paste(sum(l), " point", ifelse(sum(l) == 1, " is", "s are"), " out of plotting region in sector '", sector.index, "', track '", track.index, "'.\n", sep = ""))
    }

    return(invisible(NULL))
}

# parabola intersects with the UNIT circle
# theta1 is the start point and theta2 is the end point
rotate.parabola = function(theta1, theta2, rou1, rou2 = rou1, theta = (theta1+theta2)/2, 
    rou = rou1 * abs(cos(degree.minus(theta1, theta2)/2/180*pi))*rou.ratio, rou.ratio = 0.5,
    n = 1001) {
    
    while(theta2 < theta1) {
        theta2 = theta2 + 360
    }
    
    delta_theta = degree.minus(theta2, theta1)
    
    flag = 0
    if(delta_theta > 180) {
        theta = theta + 180
        flag = 1
    }
    
    # y^2 = kx, y = +-sqrt(kx)
    b = rou1 * abs(sin(degree.minus(theta2, theta1)/2/180*pi))
    a = rou1 * abs(cos(degree.minus(theta2, theta1)/2/180*pi)) - rou
    k = b^2/a
    
    if(n %% 2 == 0) {
        n = n + 1
    }
    n.half = (n - 1) / 2
    x = numeric(n)
    y = numeric(n)
    x = c(n.half:1/n.half, 0, 1:n.half/n.half)*a
    y[1:n.half] = sqrt(k*x[1:n.half])
    y[n.half + 1] = 0
    y[1:n.half + n.half + 1] = -sqrt(k*x[1:n.half + n.half + 1])
    
    alpha = numeric(n)
    
    alpha[1:n.half] = atan(y[1:n.half]/x[1:n.half])*180/pi
    alpha[1:n.half + n.half + 1] = atan(y[1:n.half + n.half + 1]/x[1:n.half + n.half + 1])*180/pi
    alpha[n.half + 1] = 90
    
    d = sqrt(x^2 + y^2)
    x = d*cos((alpha + theta)/180*pi)
    y = d*sin((alpha + theta)/180*pi)
    
    center.x = rou*cos(theta/180*pi)
    center.y = rou*sin(theta/180*pi)
    
    x = x + center.x
    y = y + center.y
    
    if(!flag) {
        x = rev(x)
        y = rev(y)
    }

    return(cbind(x, y))
}

# this is not a perfect function because it assumes all theta are different
# but it is ok in this package since the former step can ensure the values
# are different
is.points.ordered.on.circle = function(theta, clock.wise = FALSE) {
    if(clock.wise) {
        theta = rev(theta)
    }
    theta = theta %% 360
    theta = theta - min(theta)
    min_index = which(theta == 0)
    if(min_index > 2) {
        theta2 = c(theta[min_index:length(theta)], c(theta[1:(min_index - 1)]))
    } else {
        theta2 = theta
    }
    
    return(identical(order(theta2), 1:length(theta2)))
}

arc.points = function(theta1, theta2, rou, clock.wise = FALSE) {
    n = 100
        if(clock.wise) {
            theta = degree.seq(theta2, theta1, length = n)
    } else {
            theta = degree.seq(theta1, theta2, length = n)
     }
    x = rou * cos(theta*pi/180)
    y = rou * sin(theta*pi/180)
    if(clock.wise) {
            x = rev(x)
            y = rev(y)
    }
    return(cbind(x, y))
}
