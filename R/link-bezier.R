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
circos.link2 = function(sector.index1, point1, sector.index2, point2,
    rou1 = get.track.end.position(get.current.track.index()), 
	rou2 = get.track.end.position(get.current.track.index()), height = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, n = 101) {
    
    sector.data1 = get.sector.data(sector.index1)
    sector.data2 = get.sector.data(sector.index2)
    
    if(length(point1) == 1 && length(point2) == 1) {
        theta1 = sector.data1["start.degree"] - (point1 - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
                 abs(sector.data1["start.degree"] - sector.data1["end.degree"])
        
        theta2 = sector.data2["start.degree"] - (point2 - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
                 abs(sector.data2["start.degree"] - sector.data2["end.degree"])
        
        dq = polar2Cartesian(cbind(c(theta1, quadratic.mean.degree(theta1, theta2), theta2), c(rou1, quadratic.height(theta1, theta2, max(c(rou1, rou2))), rou2)))
		d = quadratic.bezier(dq[1, ], dq[2, ], dq[3, ])
        lines(d, col = col, lwd = lwd, lty = lty)
    } else if(length(point1) == 1 && length(point2) == 2) {
		theta1 = sector.data1["start.degree"] - (point1 - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
                 abs(sector.data1["start.degree"] - sector.data1["end.degree"])
		theta21 = sector.data2["start.degree"] - (point2[1] - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
            abs(sector.data2["start.degree"] - sector.data2["end.degree"])
        theta22 = sector.data2["start.degree"] - (point2[2] - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
            abs(sector.data2["start.degree"] - sector.data2["end.degree"])
		
		dq1 = polar2Cartesian(cbind(c(theta1, quadratic.mean.degree(theta1, theta21), theta21), c(rou1, quadratic.height(theta1, theta21, max(c(rou1, rou2))), rou2)))
		d1 = quadratic.bezier(dq1[1, ], dq1[2, ], dq1[3, ])
		dq2 = polar2Cartesian(cbind(c(theta1, quadratic.mean.degree(theta1, theta22), theta22), c(rou1, quadratic.height(theta1, theta22, max(c(rou1, rou2))), rou2)))
		d2 = quadratic.bezier(dq2[1, ], dq2[2, ], dq2[3, ])
		r = arc.points(theta21, theta22, rou2, clock.wise = TRUE)

		d = rbind(d1, r)
		d = rbind(d, d2[rev(seq_len(nrow(d2))), ])
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)
		
	} else if(length(point1) == 2 && length(point2) == 1) {
		theta11 = sector.data1["start.degree"] - (point1[1] - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
            abs(sector.data1["start.degree"] - sector.data1["end.degree"])
        theta12 = sector.data1["start.degree"] - (point1[2] - sector.data1["min.value"]) / (sector.data1["max.value"] - sector.data1["min.value"]) *
            abs(sector.data1["start.degree"] - sector.data1["end.degree"])
		theta2 = sector.data2["start.degree"] - (point2 - sector.data2["min.value"]) / (sector.data2["max.value"] - sector.data2["min.value"]) *
                 abs(sector.data2["start.degree"] - sector.data2["end.degree"])
		
		dq1 = polar2Cartesian(cbind(c(theta11, quadratic.mean.degree(theta11, theta2), theta2), c(rou1, quadratic.height(theta11, theta2, max(c(rou1, rou2))), rou2)))
		d1 = quadratic.bezier(dq1[1, ], dq1[2, ], dq1[3, ])
		dq2 = polar2Cartesian(cbind(c(theta12, quadratic.mean.degree(theta12, theta2), theta2), c(rou1, quadratic.height(theta12, theta2, max(c(rou1, rou2))), rou2)))
		d2 = quadratic.bezier(dq2[1, ], dq2[2, ], dq2[3, ])
		r = arc.points(theta11, theta12, rou2, clock.wise = TRUE)

		d = rbind(r, d2)
		d = rbind(d, d1[rev(seq_len(nrow(d1))), ])
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)

	} else if(length(point1) == 2 && length(point2) == 2) {
	
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
			
		dq1 = polar2Cartesian(cbind(c(theta11, quadratic.mean.degree(theta11, theta22), theta22), c(rou1, quadratic.height(theta11, theta22, max(c(rou1, rou2))), rou2)))
		d1 = quadratic.bezier(dq1[1, ], dq1[2, ], dq1[3, ])
		dq2 = polar2Cartesian(cbind(c(theta12, quadratic.mean.degree(theta12, theta21), theta21), c(rou1, quadratic.height(theta12, theta21, max(c(rou1, rou2))), rou2)))
		d2 = quadratic.bezier(dq2[1, ], dq2[2, ], dq2[3, ])
		r1 = arc.points(theta11, theta12, rou1, clock.wise = TRUE)
		r2 = arc.points(theta21, theta22, rou2, clock.wise = TRUE)
		
		d = rbind(d1, r2[rev(seq_len(nrow(r2))), ])
		d = rbind(d, d2[rev(seq_len(nrow(d2))), ])
		d = rbind(d, r1)
		
		polygon(d, col = col, lty = lty, lwd = lwd, border = border)
	}
    
	# link is the last track in the current version
	#set.track.end.position(0)
    return(invisible(NULL))
}

quadratic.bezier = function(p0, p1, p2, n = 100) {
	
	col = sample(1:10, 1)
	
	points(p0[1], p0[2], pch = 16, col = col)
	points(p1[1], p1[2], pch = 16, col = col)
	points(p2[1], p2[2], pch = 16, col = col)
	
	t = seq(0, 1, length.out = n)
	x = (1-t)^2 * p0[1] + 2*t*(1-t)*p1[1] + t^2*p2[1]
	y = (1-t)^2 * p0[2] + 2*t*(1-t)*p1[2] + t^2*p2[2]
	return(cbind(x, y))

}

quadratic.mean.degree = function(theta1, theta2) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	
	if(abs(theta2 - theta1) > 180) {
		return((theta2 + theta1)/2 - 180)
	} else {
		return((theta2 + theta1)/2)
	}

}

quadratic.height = function(theta1, theta2, rou) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	
	if(abs(theta2 - theta1) > 180) {
		theta = (abs(theta2 - theta1) - 180)/2
	} else {
		theta = (abs(theta2 - theta1))/2
	}
	
	#return(cos(as.radian(theta))*rou)
	return(0)
}