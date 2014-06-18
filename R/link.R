# functions to draw links are not so nice,
# a little inconsistant to the other functions and a little hard to understand
# it may be improved later.


# == title
# Draw links between points or intervals
#
# == param
# -sector.index1 Sector index for one sector
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -sector.index2 Sector index for the other sector
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -rou           The position of the 'root' of the link. It is the percentage of the radius of the unit circle.
#                By default it is the end (bottom) of the most recent track.
# -top.ratio     Set the height of the quadratic curve. For the exact definition, please refer to the main vignette.
# -col           Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.
# -lwd           Line (or border) width
# -lty           Line (or border) style
# -border        If the link is a ribbon, then it is the color for the ribbon border.
# -n             Number of points to represent a quadratic curve. Because currently I don't know how to 
#                calculate the length of a quadratic curve, the number of segmentation of the quadratic curve
#                cannot be calculated now. It should be an odd value because we need the point for the vertex.
# -top.ratio.low Adjust the height of the lower border of a link (if it is like a ribbon)
#
# == details
# The link is in fact a quadratic curve.
#
# Drawing links does not create any track. So you can think it is independent of the tracks.
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# link would look nice. However you can also set the position and the height of links by specifying
# ``rou`` and ``top.ratio``. See vignette for detailed explaination.
circos.link = function(sector.index1, point1, sector.index2, point2,
    rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, n = 101,
	top.ratio.low = NULL) {
    
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
			if(sector.index1 == sector.index2 && point1 > max(point2)) {
				point1 = c(point1, point1 + current.cell.xrange/100)
			} else if(sector.index1 == sector.index2 && point1 < min(point2)) {
				point1 = c(point1, point1 - current.cell.xrange/100)
			} else {
				point1 = c(point1, point1 + current.cell.xrange/100)
			}
        }
        if(length(point2) == 1) {
			current.cell.xrange = get.cell.meta.data("xrange", sector.index2, 1)
			if(sector.index1 == sector.index2 && point2 > max(point1)) {
				point2 = c(point2, point2 + current.cell.xrange/100)
			} else if(sector.index1 == sector.index2 && point2 < min(point1)) {
				point2 = c(point2, point2 - current.cell.xrange/100)
			} else {
				point2 = c(point2, point2 + current.cell.xrange/100)
			}
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
		
		if(!is.null(top.ratio.low)) {
			if(top.ratio.low < top.ratio) {
				stop("'top.ratio.low' should be larger than 'top.ratio'.\n")
			}
			if(quadratic.minus.degree(theta11, theta21) > quadratic.minus.degree(theta12, theta22)) {
				d2 = rotate.parabola(theta1 = theta12, theta2 = theta22, rou1 = rou, rou.ratio = top.ratio.low, n = n)
			} else {
				d1 = rotate.parabola(theta1 = theta11, theta2 = theta21, rou1 = rou, rou.ratio = top.ratio.low, n = n)
			}
		} else {
			al = min(quadratic.minus.degree(theta11, theta12), quadratic.minus.degree(theta21, theta22))
			th2 = max(quadratic.minus.degree(theta11, theta21), quadratic.minus.degree(theta12, theta22))
			th1 = min(quadratic.minus.degree(theta11, theta21), quadratic.minus.degree(theta12, theta22))
			if(quadratic.minus.degree(th2, th1) < 90 &&
			   al < 5 &&  #alpha
			   th2 < 45 && th2 < 180 - th1 &&
			   top.ratio <= 1- 0.05) {
				
				if(quadratic.minus.degree(theta11, theta21) > quadratic.minus.degree(theta12, theta22)) {
					d2 = rotate.parabola(theta1 = theta12, theta2 = theta22, rou1 = rou, rou.ratio = top.ratio+0.05, n = n)
				} else {
					d1 = rotate.parabola(theta1 = theta11, theta2 = theta21, rou1 = rou, rou.ratio = top.ratio+0.05, n = n)
				}		
			}
		}
		
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
    n = 101) {
    
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
    # x points should be more thick near the vertex
    x = c((n.half:1)^2/n.half^2, 0, (1:n.half)^2/n.half^2)*a
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


quadratic.mean.degree = function(theta1, theta2) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	
	if(abs(theta2 - theta1) > 180) {
		return((theta2 + theta1)/2 - 180)
	} else {
		return((theta2 + theta1)/2)
	}

}



quadratic.minus.degree = function(theta1, theta2) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	
	if(abs(theta2 - theta1) > 180) {
		return(abs(theta2 - theta1) - 180)
	} else {
		return(abs(theta2 - theta1))
	}

}


chordDiagram = function(mat, grid.col = NULL, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, directional = FALSE,
	symmetric = FALSE, order = NULL) {
	
	if(symmetric) {
		mat[lower.tri(mat, diag = TRUE)] = 0
	}
	
	if(!is.null(order)) {
		if(is.null(rownames(mat)) || is.null(colnames(mat))) {
			stop("Since you specified `order`, your matrix should have rowname and colname.\n")
		}
		if(length(intersect(order, c(rownames(mat), colnames(mat))))) {
			stop("Elements in `order` should be same as in `mat` rownames/colnames.\n")
		}
	}
	
	ri = apply(mat, 1, function(x) all(abs(x) < 1e-6))
	ci = apply(mat, 2, function(x) all(abs(x) < 1e-6))
	
	mat = mat[ri, ci]
	if(is.matrix(col)) {
		col = col[ri, ci]
	}
	if(length(row.col) > 1) {
		row.col = row.col[ri]
	}
	if(length(column.col) > 1) {
		column.col = column.col[ci]
	}
	
	if(is.null(rownames(mat))) {
		rownames(mat) = paste0("R", seq_len(nrow(mat)))
	}
	if(is.null(colnames(mat))) {
		colnames(mat) = paste0("C", seq_len(ncol(mat)))
	}
	
	if(!is.null(order)) {
		order = order[order %in% c(rownames(mat), colnames(mat))]
	}

	rs = rowSums(abs(mat))
	cs = colSums(abs(mat))

	nn = union(names(rs), names(cs))
	xlim = numeric(length(nn))
	names(xlim) = nn
	
	if(!is.null(order)) {
		xlim = xlim[order]
	}
	
	xlim[names(rs)] = xlim[names(rs)] + rs
	xlim[names(cs)] = xlim[names(cs)] + cs
	
	factors = names(xlim)
	factors = factor(factors, levels = factors)
	xlim = cbind(rep(0, length(factors)), xlim)
	
	n = length(factors)
	if(is.null(grid.col)) {
		grid.col = rgb(cbind(runif(n), runif(n), runif(n)))
		names(grid.col) = factors
	}
	
	## make a color matrix based on settings
	if(!is.null(col)) {
		if(is.function(col)) {
			col = col(mat)
		} else if(is.matrix(col)) {
		
		} else if(length(col) == 1) {
			col = rep(col, length(mat))
		}
	} else if(!is.null(row.col)) {
		col = rep(row.col, ncol(mat))
	} else if(!is.null(column.col)) {
		col = rep(column.col, each = nrow(mat))
	} else {
		col = rep(grid.col[rownames(mat)], ncol(mat))
	}
	
	col = rgb(t(col2rgb(col)), maxColorValue = 255, alpha = (1 - transparency)*255)
	
	dim(col) = dim(mat)
	colnames(col) = colnames(col)
	rownames(col) = rownames(col)

	circos.par(cell.padding = c(0, 0, 0, 0))
    circos.initialize(factors = factors, xlim = xlim)
	circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
		panel.fun = function(x, y) {
			xlim = get.cell.meta.data("xlim")
			current.sector.index = get.cell.meta.data("sector.index")
			i = get.cell.meta.data("sector.numeric.index")
			if(is.null(text.direction)) {
				theta = mean(get.cell.meta.data("xplot")) %% 360
				if(theta < 90 || theta > 270) {
					text.direction = "vertical_right"
					text.adj = c(0, 0.5)
				} else {
					text.direction = "vertical_left"
					text.adj = c(1, 0.5)
				}
			}
			circos.text(mean(xlim), 0.5, labels = current.sector.index,
				direction = text.direction, adj = text.adj)
		}, track.height = 0.05)
    circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
		track.height = 0.05, panel.fun = function(x, y) {
			xlim = get.cell.meta.data("xlim")
			current.sector.index = get.cell.meta.data("sector.index")
			circos.rect(xlim[1], 0, xlim[2], 1, col = grid.col[current.sector.index], border = grid.col[current.sector.index])
		})
    # links
    rn = rownames(mat)
	cn = colnames(mat)
    sector.sum.row = numeric(length(factors))
    sector.sum.col = numeric(length(factors))
	names(sector.sum.row) = factors
	names(sector.sum.col) = factors
	sector.sum.col[ names(rs) ] = rs
    for(i in seq_along(rn)) {
		for(j in rev(seq_along(cn))) {
			if(abs(mat[i, j]) < 1e-6) {
				next
			}
			rou = circlize:::get.track.end.position(circlize:::get.current.track.index())
            sector.index1 = rn[i]
            sector.index2 = cn[j]
            circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[i, j])),
                        sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[i, j])),
                        col = col[i, j], rou = ifelse(directional, rou - 0.02, rou), border = NA)
			if(directional) {
				d1 = circlize(c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[i, j])), c(0, 0), sector.index = sector.index1)
				draw.sector(start.degree = d1[1, 1], end.degree = d1[2, 1], rou1 = rou, rou2 = rou - 0.02, col = col[i, j])
			}
            sector.sum.row[ rn[i] ] = sector.sum.row[ rn[i] ] + abs(mat[i, j])
			sector.sum.col[ cn[j] ] = sector.sum.col[ cn[j] ] + abs(mat[i, j])
        }
    }
}
	