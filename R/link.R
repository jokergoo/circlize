
# == title
# Draw links between points or intervals
#
# == param
# -sector.index1 Index for sector one
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -sector.index2 Index for the other sector
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -rou           The position of the 'root' of the link. It is the percentage of the radius of the unit circle.
#                By default its value is the position of bottom margin of the most inner track.
# -rou1          The position of root 1 of the link. 
# -rou2          The position of root 2 of the link.
# -h             Height of the link. 
# -w             Since the link is a Bezier curve, it controls the shape of Bezier curve.
# -h2            Height of the bottom edge of the link if it is a ribbon.
# -w2            Shape of the bottom edge of the link if it is a ribbon.
# -col           Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.
# -lwd           Line (or border) width
# -lty           Line (or border) style
# -border        If the link is a ribbon, then it is the color for the ribbon border.
#
# == details
# Links are implemented as quadratic Bezier curves.
#
# Drawing links does not create any track. So you can think it is independent of the tracks.
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# links would look nice. 
#
# See vignette for detailed explanation.
circos.link = function(sector.index1, point1, sector.index2, point2,
    rou = get_most_inside_radius(),
    rou1 = rou, rou2 = rou, h = NULL, w = 1, h2 = h, w2 = w,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA) {
    
    sector.data1 = get.sector.data(sector.index1)
    sector.data2 = get.sector.data(sector.index2)
	
	point1 = sort(point1)
	point2 = sort(point2)

    if(length(point1) == 1 && length(point2) == 1) {
        theta1 = circlize(point1, 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta2 = circlize(point2, 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        
		d = getQuadraticPoints(theta1, theta2, rou1, rou2, h = h, w = w)
        lines(d, col = col, lwd = lwd, lty = lty)
    } else if(length(point1) == 1) {
		theta1 = circlize(point1, 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
		theta21 = circlize(point2[1], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        theta22 = circlize(point2[2], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        
        if(degreeDiff(theta1, theta21) > degreeDiff(theta1, theta22)) {
        	d1 = getQuadraticPoints(theta1, theta21, rou1, rou2, h = h, w = w)
        	d2 = getQuadraticPoints(theta1, theta22, rou1, rou2, h = h2, w = w2)
        } else {
	        d1 = getQuadraticPoints(theta1, theta21, rou1, rou2, h = h2, w = w2)
	        d2 = getQuadraticPoints(theta1, theta22, rou1, rou2, h = h, w = w)
	    }
		r2 = arc.points(theta21, theta22, rou2)
		d = rbind(d1, r2)
		d = rbind(d, revMat(d2))
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)
	} else if(length(point2) == 1) {
		theta2 = circlize(point2, 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
		theta11 = circlize(point1[1], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta12 = circlize(point1[2], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        
        if(degreeDiff(theta2, theta11) > degreeDiff(theta2, theta12)) {
	        d1 = getQuadraticPoints(theta11, theta2, rou1, rou2, h = h, w = w)
	        d2 = getQuadraticPoints(theta12, theta2, rou1, rou2, h = h2, w = w2)
	    } else {
	    	d1 = getQuadraticPoints(theta11, theta2, rou1, rou2, h = h2, w = w2)
	        d2 = getQuadraticPoints(theta12, theta2, rou1, rou2, h = h, w = w)
	    }
		r1 = arc.points(theta11, theta12, rou1)
		d = rbind(revMat(d1), r1)
		d = rbind(d, d2)
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)
	} else {
		
		theta11 = circlize(point1[1], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta12 = circlize(point1[2], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
		theta21 = circlize(point2[1], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        theta22 = circlize(point2[2], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
		
		if(degreeDiff(theta11, theta22) > degreeDiff(theta12, theta21)) {
			d1 = getQuadraticPoints(theta11, theta22, rou1, rou2, h = h, w = w)
	        d2 = getQuadraticPoints(theta12, theta21, rou1, rou2, h = h2, w = w2)
	    } else {
	    	d1 = getQuadraticPoints(theta11, theta22, rou1, rou2, h = h2, w = w2)
	        d2 = getQuadraticPoints(theta12, theta21, rou1, rou2, h = h, w = w)
	    }
		r2 = arc.points(theta21, theta22, rou2)
		r1 = arc.points(theta11, theta12, rou1)

        d = rbind(d1, revMat(r2))
        d = rbind(d, revMat(d2))
        d = rbind(d, revMat(r1))
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)
    }
	
    return(invisible(NULL))
}

# points from theta1 to theta2
arc.points = function(theta1, theta2, rou) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	theta_diff = (theta1 - theta2) %% 360
	l = as.radian(theta_diff)*rou
	ncut = l/ (2*pi/circos.par("unit.circle.segments"))
	ncut = floor(ncut)
	ncut = ifelse(ncut < 2, 2, ncut)
    
	x = numeric(ncut)
	y = numeric(ncut)
	for(i in seq_len(ncut)) {
		t = (theta1 - (i-1)*theta_diff/(ncut-1)) %% 360
		x[i] = rou * cos(as.radian(t))
		y[i] = rou * sin(as.radian(t))
	}
	d = cbind(x, y)
	#linesWithArrows(d)
	return(d)
}

# points from theta1 to theta2
# first calcualte bezier curve of which two end points are located at (-d, 0), (d, 0)
# and the summit is located at (0, 2h)
getQuadraticPoints = function(theta1, theta2, rou1, rou2, h = NULL, w = 1) {

	# enforce theta1 is always less than theta 2 (reverse-clockwise)
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360

    if (abs(theta2 - theta1) > 180) {
        theta_mid = (theta2 + theta1)/2 - 180
    } else {
        theta_mid = (theta2 + theta1)/2
    }

    theta_mid  = theta_mid %% 360


	rou_min = min(rou1, rou2)
	x1 = rou_min*cos(as.radian(theta1))
	y1 = rou_min*sin(as.radian(theta1))
	
	x2 = rou_min*cos(as.radian(theta2))
	y2 = rou_min*sin(as.radian(theta2))
	
	# determin h
	beta = (theta1 - theta2) %% 360
	if(beta > 180) beta = 360 - beta
	h_auto = rou_min*(1-0.5*cos(as.radian(beta/2)))
	
	if(is.null(h)) {
		h = h_auto
	}
	if(h > rou_min) {
		h = h_auto
	}
	
	h2 = h - rou_min*(1-cos(as.radian(beta/2)))
	
	if(w < 0) h2 = -h2
	
	dis = 1/2 * sqrt((x1 - x2)^2 + (y1 - y2)^2)
	p0 = c(-dis, 0)
	p2 = c(dis, 0)
	if(w == 0) {
		p1 = c(0, 0)
	} else {
		p1 = c(0, (1+w)/w*h2)
	}
	
	d = quadratic.bezier(p0, p1, p2, w = w)

	# shit, I don't know why!
	col = "red"
	alpha = - 90 + (360 - (theta1 + theta2)/2 %% 360)
	if((theta_mid > 270 || theta_mid < 90) &&
	   ( (theta1 - 180)*(theta2 - 180) < 0 )) {
		alpha = alpha + 180
		col = "green"
	}
	
	# rotate coordinate
	mat = matrix(c(cos(as.radian(alpha)), sin(as.radian(alpha)), -sin(as.radian(alpha)), cos(as.radian(alpha))), nrow = 2)
	d = d %*% mat
	
	x0 = (x1+x2)/2
	y0 = (y1+y2)/2
	
	d[, 1] = d[, 1] + x0
	d[, 2] = d[, 2] + y0

	if((theta1 - theta2) %% 360 < 180) {
		d = revMat(d)
	}

	#points(rou_min*cos(as.radian(theta1)), rou_min*sin(as.radian(theta1)), pch = 16, col = "blue")
	
	if(rou1 > rou2) {
		d = rbind(c(rou1*cos(as.radian(theta1)), rou1*sin(as.radian(theta1))), d)
	} else if(rou1 < rou2) {
		d = rbind(d, c(rou2*cos(as.radian(theta2)), rou2*sin(as.radian(theta2))))
	}
	#linesWithArrows(d, col = col)
	return(d)
}

# 'Rational bezier curve', from wikipedia
# w: w1
quadratic.bezier = function(p0, p1, p2, n = 100, w = 1) {
	
	t = seq(0, 1, length.out = n)
	x = ((1-t)^2 * p0[1] + 2*t*(1-t)*p1[1]*w + t^2*p2[1]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	y = ((1-t)^2 * p0[2] + 2*t*(1-t)*p1[2]*w + t^2*p2[2]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	return(cbind(x, y))

}

are.lines.intersected = function(x1, y1, x2, y2) {
	n1 = length(x1)
	n2 = length(x2)
	
	for(i in seq_len(n1)) {
		if(i == 1) next
		
		a_1x = x1[i-1]
		a_1y = y1[i-1]
		a_2x = x1[i]
		a_2y = y1[i]
			
		k1 = (a_1y - a_2y)/(a_1x - a_2x)
		b1 = a_1y - k1*a_1x	
						
		for(j in seq_len(n2)) {
			if(j == 1) next

			b_1x = x2[j-1]
			b_1y = y2[j-1]
			b_2x = x2[j]
			b_2y = y2[j]
			
			
			k2 = (b_1y - b_2y)/(b_1x - b_2x)
			b2 = b_1y - k2*b_1x
				
			i_x = - (b2 - b1)/(k2 - k1)
			i_y = k1*i_x + b1
			
			a_minx = min(c(a_1x, a_2x))
			a_maxx = max(c(a_1x, a_2x))
			b_minx = min(c(b_1x, b_2x))
			b_maxx = max(c(b_1x, b_2x))
			if(i_x >= a_minx && i_x <= a_maxx && i_x >= b_minx && i_x <= b_maxx) {
				return(TRUE)
			}
		}
	}
	return(FALSE)
}

revMat = function(mat) {
	return(mat[rev(seq_len(nrow(mat))), , drop = FALSE])
}


linesWithArrows = function(d, sep = 6, col = "red") {
	lines(d, col = col)
	if(nrow(d) > sep) {
		for(i in seq(sep, nrow(d)-sep, by = sep)) {
			arrows(d[i-sep/2, 1], d[i-sep/2, 2], d[i+sep/2, 1], d[i+sep/2, 2], length = 0.05, col = col)
		}
	} else {
		arrows(d[1, 1], d[1, 2], d[2, 1], d[2, 2], length = 0.05, col = col)
	}
}

degreeDiff = function (theta1, theta2) {
    theta1 = theta1 %% 360
    theta2 = theta2 %% 360
    if (abs(theta2 - theta1) > 180) {
        return(abs(theta2 - theta1) - 180)
    }
    else {
        return(abs(theta2 - theta1))
    }
}
