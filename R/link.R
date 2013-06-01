# functions to draw links are not so nice,
# a little inconsistant to the other functions and a little hard to understand
# it may be improved later.


# == title
# Draw links between points or intervals
#
# == param
# -sector.index1 Sector index for one sector
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt.
# -sector.index2 Sector index for the other sector
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt.
# -rou           The position of the 'root' of the link. It is the percentage of the radius of the unit circle.
#                By default it is the end (bottom) of the most recent track.
# -top.ratio     Set the height of the quadratic curve.
# -col           Color of the link. If the link is a belt, then it is the filled color for the belt.
# -lwd           Line (or border) width
# -lty           Line (or border) style
# -border        If the link is a belt, then it is the color for the belt border.
# -n             Number of points to represent a quadratic curve. Because currently I don't know how to 
#                calculate the length of a quadratic curve, the number of segmentation of the quadratic curve
#                cannot be calculated now. It should be an odd value because we need the point for the vertex.
#
# == details
# The link is in fact a quadratic curve.
#
# Drawing links does not create any track. So you can think it is independent of the tracks
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# link would look nice. However you can also set the position and the height of links by specifying
# ``rou`` and ``top.ratio``. See vignette for detailed explaination.
circos.link = function(sector.index1, point1, sector.index2, point2,
    rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, n = 501) {
    
    sector.data1 = get.sector.data(sector.index1)
    sector.data2 = get.sector.data(sector.index2)
    
    if(length(point1) == 1 && length(point2) == 1) {
        theta1 = sector.data1["start.degree"] - (point1 - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
                 abs(sector.data1["start.degree"] - sector.data1["end.degree"])
        
        theta2 = sector.data2["start.degree"] - (point2 - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
                 abs(sector.data2["start.degree"] - sector.data2["end.degree"])
        
        d = rotate.parabola(theta1, theta2, rou1 = rou, rou.ratio = top.ratio, n = n)
        lines(d, col = col, lwd = lwd, lty = lty)
    } else {
        if(length(point1) == 1) {
			current.cell.xrange = get.cell.meta.data("xrange", sector.index1, 1)
            point1 = c(point1, point1 + current.cell.xrange/100)   
        }
        if(length(point2) == 1) {
			current.cell.xrange = get.cell.meta.data("xrange", sector.index2, 1)
            point2 = c(point2, point2 + current.cell.xrange/100)  
        }
		
		if(sector.index1 == sector.index2) {
			if(max(c(point1, point2)) - min(c(point1, point2)) < max(point2) - min(point2) + max(point1) - min(point1)) {
				stop("Two intervals in a same sector, but they should not be intersected.\n")
			}
		}
		
		point1 = sort(point1)
		point2 = sort(point2)
        
        theta11 = sector.data1["start.degree"] - (point1[1] - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
            abs(sector.data1["start.degree"] - sector.data1["end.degree"])
        theta12 = sector.data1["start.degree"] - (point1[2] - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
            abs(sector.data1["start.degree"] - sector.data1["end.degree"])
        
        theta21 = sector.data2["start.degree"] - (point2[1] - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
            abs(sector.data2["start.degree"] - sector.data2["end.degree"])
        theta22 = sector.data2["start.degree"] - (point2[2] - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
            abs(sector.data2["start.degree"] - sector.data2["end.degree"])
        
        # line from theta11, theta21 and line from theta12, theta22
        # uint circle
        k1 = (sin(as.radian(theta11)) - sin(as.radian(theta21)))/(cos(as.radian(theta11)) - cos(as.radian(theta21)))
        b1 = sin(as.radian(theta11)) - k1*cos(as.radian(theta11))
        k2 = (sin(as.radian(theta12)) - sin(as.radian(theta22)))/(cos(as.radian(theta12)) - cos(as.radian(theta22)))
        b2 = sin(as.radian(theta12)) - k2*cos(as.radian(theta12))
        
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
        
        d1 = rotate.parabola(theta1 = theta11, theta2 = theta21, rou1 = rou, rou.ratio = top.ratio, n = n)
        d2 = rotate.parabola(theta1 = theta12, theta2 = theta22, rou1 = rou, rou.ratio = top.ratio, n = n)

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


# parabola intersects with the UNIT circle
# theta1 is the start point and theta2 is the end point
rotate.parabola = function(theta1, theta2, rou1, rou2 = rou1, theta = (theta1+theta2)/2, 
    rou = rou1 * abs(cos(degree.minus(theta1, theta2)/2/180*pi))*rou.ratio, rou.ratio = 0.5,
    n = 501) {
    
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
    
    if(clock.wise) {
		l = as.radian(theta1 - theta2)*rou
		ncut = l/ (2*pi/circos.par("unit.circle.segments"))
		ncut = floor(ncut)
		ncut = ifelse(ncut < 2, 2, ncut)
        theta = degree.seq(from = theta2, to = theta1, length.out = ncut)
    } else {
		l = as.radian(theta2 - theta1)*rou
		ncut = l/ (2*pi/circos.par("unit.circle.segments"))
		ncut = floor(ncut)
		ncut = ifelse(ncut < 2, 2, ncut)
        theta = degree.seq(from = theta1, to = theta2, length.out = ncut)
     }
    x = rou * cos(as.radian(theta))
    y = rou * sin(as.radian(theta))
    if(clock.wise) {
        x = rev(x)
        y = rev(y)
    }
    return(cbind(x, y))
}


# if restrict is TRUE, then value should belong to [0, 360)
degree.add = function(theta1, theta2, restrict = FALSE) {
	if(restrict) {
		return((theta1 + theta2) %% 360)
	} else {
		return(theta1 + theta2)
	}
}

degree.minus = function(to, from, restrict = FALSE) {
	if(restrict) {
		return((to - from) %% 360)
	} else {
		return(to - from)
	}
}

degree.seq = function(from, to, length.out = 2, restrict = FALSE) {
	if(length.out == 2) {
		return(c(from, to))
	} else if(length.out > 2) {
		per = degree.minus(to, from, restrict = FALSE) / (length.out - 1)
		s = numeric(length.out)
		s[1] = degree.add(from, 0, restrict)
		for(i in seq_along(s)) {
			if(i == 1) {
				next
			}
			s[i] = degree.add(s[i - 1], per, restrict)
		}
		return(s)
	}
}
