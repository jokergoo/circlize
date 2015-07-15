
# == title
# Plot Chord Diagram
#
# == param
# -mat A table which represents as a numeric matrix.
# -grid.col Grid colors which correspond to matrix rows/columns (or sectors). The length of the vector should be either 1 or ``length(union(rownames(mat), colnames(mat)))``.
#           It's preferred that ``grid.col`` is a named vector of which names correspond to sectors. 
#           If it is not a named vector, the order of ``grid.col`` corresponds to order of sectors.
# -transparency Transparency of link colors, 0 means no transparency and 1 means full transparency.
#               If transparency is already set in ``col`` or ``row.col`` or ``column.col``, this argument will be ignored.
# -col Colors for links. It can be a matrix which corresponds to ``mat``, or a function which generate colors 
#      according to values in ``mat``, or a single value which means colors for all links are the same, or a three-column
#      data frame in which the first two columns correspond to row names and columns and the third column is colors. You
#      may use `colorRamp2` to generate a function which maps values to colors.
# -row.col Colors for links. Links from the same row in ``mat`` will have the same color.
#          Length should be same as number of rows in ``mat``. This argument only works when ``col`` is set to ``NULL``.
# -column.col Colors for links. Links from the same column in ``mat`` will have the same color.
#             Length should be same as number of columns in ``mat``. This argument only works when ``col`` and ``row.col`` is set to ``NULL``.
# -fromRows deprecated, use ``directional`` instead
# -directional Whether links have directions. 1 means from rows to columns. -1 means from columns to rows
# -direction.type type for representing directions. Can be one or two values in "diffHeight" and "arrows".
# -symmetric Whether the matrix is symmetric. If the value is set to ``TRUE``, only
#            lower triangular matrix without the diagonal will be used.
# -keep.diagonal If the matrix is specified as symmetric, whether keep diagonal for visualization.
# -order Order of sectors. Default order is ``union(rownames(mat), colnames(mat))``.
# -preAllocateTracks Pre-allocate empty tracks before drawing chord diagram. It can be a single number indicating
#                    how many empty tracks needed to be created or a list containing settings for empty
#                    tracks. Please refer to vignette for details.
# -annotationTrack Which annotation track should be plotted? By default, a track containing sector names and a track
#                  containing grid will be created.
# -annotationTrackHeight Track height corresponding to values in ``annotationTrack``.
# -link.border border for links, single scalar or a matrix with names or a data frame with three columns
# -link.lwd width for link borders, single scalar or a matrix with names or a data frame with three columns
# -link.lty style for link borders, single scalar or a matrix with names or a data frame with three columns
# -grid.border border for grids. If it is ``NULL``, the border color is same as grid color
# -diffHeight The difference of height between two 'roots' if ``directional`` is set to ``TRUE``. If the value is set to
#             a positive value, start root is shorter than end root and if it is set to a negative value, start root is longer
#             than the end root.
# -reduce if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.
#         Set it to value less than zero if you want to keep all tiny grid.
# -link.sort whether sort links on every sector based on the width of the links on it.
# -link.decreasing for ``link.sort``
# -link.arr.length pass to `circos.link`, same settings as ``link.lwd``.
# -link.arr.width pass to `shape::Arrowhead`, same settings as ``link.lwd``.
# -link.arr.type pass to `circos.link`, same settings as ``link.lwd``. Default value is ``triangle``.
# -link.arr.col color or the single line link which is put in the center of the belt, same settings as ``link.lwd``.
# -link.arr.lwd line width ofthe single line link which is put in the center of the belt, same settings as ``link.lwd``.
# -link.arr.lty line type of the single line link which is put in the center of the belt, same settings as ``link.lwd``.
# -... pass to `circos.link`
#
# == details
# Chord diagram is a way to visualize numeric tables ( http://circos.ca/intro/tabular_visualization/ ), especially useful
# when the table represents information of directional relations. This function
# visualize tables in a circular way.
#
# Sectors of the circos plot is ``union(rownames(mat), colnames(mat))``. If there is no rowname or colname, the function will
# assign names for it ("R1", "R2", ... for row names, "C1", "C2", ... for column names).
#
# This function is flexible and contains some settings that may be a little difficult to understand. 
# Please refer to vignette for better explanation.
chordDiagram = function(mat, grid.col = NULL, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, directional = 0, fromRow,
	direction.type = "diffHeight",
	symmetric = FALSE, keep.diagonal = FALSE, order = NULL, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), grid.border = NA, 
	diffHeight = 0.04, reduce = 1e-5, link.sort = FALSE, link.decreasing = FALSE,
	link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4), 
	link.arr.width = link.arr.length/2, 
	link.arr.type = "triangle", link.arr.lty = par("lty"), 
	link.arr.lwd = par("lwd"), link.arr.col = par("col"), ...) {
	
	if(!is.matrix(mat)) {
		stop("`mat` can only be a matrix.\n")
	}

	if(directional) {
		if(length(setdiff(direction.type, c("diffHeight", "arrows"))) > 0) {
			stop("`direction.type` can only be in 'diffHeight' and 'arrows'.")
		}
		if(!missing(fromRow)) {
			if(fromRow) warning("`fromRow` is deprated, use `directional = 1` instread.")
			else warning("`fromRow` is deprated, use `directional = -1` instread.")
		}
	}
	
	transparency = ifelse(transparency < 0, 0, ifelse(transparency > 1, 1, transparency))

	if(symmetric) {
		if(nrow(mat) != ncol(mat)) {
			stop("`mat` should be a square matrix.\n")
		}

		for(i in 1:10) {
			n = sample(nrow(mat), 2)
			ir = n[1]
			ic = n[2]
			if(abs(mat[ir, ic] - mat[ic, ir]) > 1e-8) {
				stop("Is `mat` really a symmetric matrix?\n")
			}
		}

		if(is.null(rownames(mat)) && is.null(colnames(mat))) {
			rownames(mat) = paste0("S", seq_len(nrow(mat)))
			colnames(mat) = paste0("S", seq_len(ncol(mat)))
		}

		if(!setequal(rownames(mat), colnames(mat))) {
			stop("Since you specified a symmetric matrix, rownames and colnames should be the same.\n")
		}

		mat[upper.tri(mat, diag = !keep.diagonal)] = 0
		
	}

	if(!is.null(order)) {
		if(is.null(rownames(mat)) || is.null(colnames(mat))) {
			stop("Since you specified `order`, your matrix should have rowname and colname.\n")
		}
		if(!setequal(order, union(rownames(mat), colnames(mat)))) {
			stop("Elements in `order` should be same as in `union(rownames(mat), colnames(mat))`.\n")
		}
	}
	
	if(is.null(rownames(mat))) {
		rownames(mat) = paste0("R", seq_len(nrow(mat)))
	}
	if(is.null(colnames(mat))) {
		colnames(mat) = paste0("C", seq_len(ncol(mat)))
	}
	

	# width of the category is almost 0, it means this category has no link to the others
	rs = rowSums(abs(mat))
	cs = colSums(abs(mat))

	nn = union(names(rs), names(cs))
	xlim = numeric(length(nn))
	names(xlim) = nn

	xlim[names(rs)] = xlim[names(rs)] + rs
	xlim[names(cs)] = xlim[names(cs)] + cs

	# self-link, values are added twice
	self_link = intersect(rownames(mat), colnames(mat))
	if(length(self_link)) {
		xlim[self_link] = xlim[self_link] - abs(diag(mat[self_link, self_link, drop = FALSE]))
	}

	keep_index = names(xlim)[xlim / sum(xlim) >= reduce]
	ri = which(rownames(mat) %in% keep_index)
	ci = which(colnames(mat) %in% keep_index)
	
	# if the matrix is reduced
	if(sum(length(ri) + length(ci)) < sum(ncol(mat) + nrow(mat))) {
		
		un = union(rownames(mat), colnames(mat))
		nn = union(rownames(mat)[ri], colnames(mat)[ci])
		if(length(circos.par("gap.degree")) == length(un)) {
			old.gap.degree = circos.par("gap.degree")
			circos.par("gap.degree" = old.gap.degree[un %in% nn])
		}
		
		if(!is.null(grid.col)) {
			if(is.null(names(grid.col)) && length(grid.col) == length(un)) {
				grid.col = grid.col[un %in% nn]
			}
		}
		
	}
	
	mat = mat[ri, ci, drop = FALSE]
	if(is.matrix(col)) {
		col = col[ri, ci, drop = FALSE]
	}
	if(length(row.col) > 1) {
		row.col = row.col[ri]
	}
	if(length(column.col) > 1) {
		column.col = column.col[ci]
	}

	# re-calculate xlim based on reduced mat
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

	self_link = intersect(rownames(mat), colnames(mat))
	if(length(self_link)) {
		xlim[self_link] = xlim[self_link] - abs(diag(mat[self_link, self_link, drop = FALSE]))
	}

	factors = names(xlim)
	factors = factor(factors, levels = factors)
	xlim = cbind(rep(0, length(factors)), xlim)

	n = length(factors)
	if(is.null(grid.col)) {
		grid.col = rand_color(n)
		names(grid.col) = factors
	} else {
		if(length(grid.col) == 1) {
			grid.col = rep(grid.col, length(factors))
			names(grid.col) = factors
		} else if(!is.null(names(grid.col))) {
			if(length(setdiff(factors, names(grid.col))) > 0) {
				stop("Since your ``grid.col`` is a named vector, all sectors should have corresponding colors.\n")
			}
			
			grid.col = grid.col[as.vector(factors)]
		} else if(length(grid.col) == length(factors)) {
			names(grid.col) = factors
		} else {
			stop("Since you set ``grid.col``, the length should be either 1 or number of sectors,\nor set your ``grid.col`` as vector with names.\n")
		}
	}

	rn = rownames(mat)
	cn = colnames(mat)

	## make a color matrix based on settings
	if(!is.null(col)) {
		if(is.function(col)) {
			col = col(mat)
		} else {
			col = .normalize_to_mat(col, rn, cn, "#FFFFFF00")
		}
	} else if(!is.null(row.col)) {
		if(length(row.col) == 1) {
			row.col = rep(row.col, nrow(mat))
		}
		col = rep(row.col, ncol(mat))
	} else if(!is.null(column.col)) {
		if(length(column.col) == 1) {
			column.col = rep(column.col, ncol(mat))
		}
		col = rep(column.col, each = nrow(mat))
	} else {
		col = rep(grid.col[rownames(mat)], ncol(mat))
	}

	rgb_mat = t(col2rgb(col, alpha = TRUE))
	if(all(rgb_mat[, 4] == 255)) {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = (1-transparency)*255)
	} else {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
	}
	

	dim(col) = dim(mat)
	colnames(col) = colnames(mat)
	rownames(col) = rownames(mat)
	
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(0, 0, 0, 0))
    circos.initialize(factors = factors, xlim = xlim)

	# pre allocate track
	if(!is.null(preAllocateTracks)) {
		pa = parsePreAllocateTracksValue(preAllocateTracks)
		for(i in seq_along(pa)) {
			va = pa[[i]]
			circos.trackPlotRegion(ylim = va$ylim, track.height = va$track.height,
				bg.col = va$bg.col, bg.border = va$bg.border, bg.lty = va$bg.lty, bg.lwd = va$bg.lwd)
		}
	}
	if(any(annotationTrack %in% "name")) {
		circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
			panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				i = get.cell.meta.data("sector.numeric.index")
				circos.text(mean(xlim), 0.5, labels = current.sector.index,
					facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
			}, track.height = annotationTrackHeight[which(annotationTrack %in% "name")])
    }
	if(any(annotationTrack %in% "grid")) {
		circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
			panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				if(is.null(grid.border)) {
					border.col = grid.col[current.sector.index]
				} else {
					border.col = grid.border
				}
				circos.rect(xlim[1], 0, xlim[2], 1, col = grid.col[current.sector.index], border = border.col)
			}, track.height = annotationTrackHeight[which(annotationTrack %in% "grid")])
	}
    # links
	rn = rownames(mat)
	cn = colnames(mat)
	
	
	link.border = .normalize_to_mat(link.border, rn, cn, default = NA)
	link.lwd = .normalize_to_mat(link.lwd, rn, cn, default = 1)
	link.lty = .normalize_to_mat(link.lty, rn, cn, default = 1)
	link.arr.length = .normalize_to_mat(link.arr.length, rn, cn, default = 0.4)
	link.arr.width = .normalize_to_mat(link.arr.width, rn, cn, default = 0.2)
	link.arr.type = .normalize_to_mat(link.arr.type, rn, cn, default = "triangle")
	link.arr.lty = .normalize_to_mat(link.arr.lty, rn, cn, default = 1)
	link.arr.lwd = .normalize_to_mat(link.arr.lwd, rn, cn, default = 1)
	link.arr.col = .normalize_to_mat(link.arr.col, rn, cn, default = NA)

	nr = length(rn)
	nc = length(cn)

	df = mat2df

	chordDiagram(df, ...)
	df$o1 = rep(0, nrow(df))
	df$or = rep(0, nrow(df))
	df$x1 = rep(0, nrow(df))
	df$x2 = rep(0, nrow(df))
	# row first
	od = tapply(abs(df$value), df$rn, order)
	for(nm in names(od)) {
		df$o1[df$rn == nm] = od[[nm]]
		df$x1[df$rn == nm] = cumsum(abs(df$value)[od[[nm]]])
	}
	max_o1 = sapply(od, max)
	sum_1 = tapply(df$x1, df$rn, sum)
	od = tapply(abs(df$value), df$cn, order)
	for(nm in names(od)) {
		if(!is.null(max_o1[[nm]])) {
			df$o2[df$cn == nm] = od[[nm]] + max_o1[nm]
			df$x2[df$cn == nm] = cumsum(abs(df$value)[od[nm]]) + sum_1[nm]
		} else {
			df$o2[df$cn == nm] = od[[nm]]
			df$x2[df$cn == nm] = cumsum(abs(df$value)[od[nm]])
		}
	}
	

	
	# if row names and column names overlaps, seperate them and sort separately
	.get_pos = function(mat, ri, ci, on = "row", sort = FALSE, decreasing = FALSE) {
		if(on == "row") {
			x1 = mat[ri, ]
			x2 = numeric(0)
			if(rownames(mat)[ri] %in% colnames(mat)) {
				x2 = mat[-ri, ri]
			}
			if(sort) {
				x = c(0, cumsum(c(sort(abs(x1), decreasing = link.decreasing), sort(abs(x2), decreasing = link.decreasing))))
			} else {
				x = c(0, cumsum(abs(c(x1, x2))))
			}
			x = rev(x)
			return(x[ci + 0:1])
		} else if(on == "column") {
			if(colnames(mat)[ci] %in% rownames(mat)) {
				rri = which(rownames(mat), colnames(mat)[ci])
				x1 = mat[rri, ]
				x2 = numeric(0)
				if(rownames(mat)[rri] %in% colnames(mat)) {
					x2 = mat[-rri, rri]
				}
				if(sort) {
					x = c(0, cumsum(c(sort(abs(x1), decreasing = link.decreasing), sort(abs(x2), decreasing = link.decreasing))))
				} else {
					x = c(0, cumsum(abs(c(x1, x2))))
				}

			} else {
				x1 = mat[, ci]
				if(sort) {
					x = c(0, cumsum(sort(abs(x1), decreasing = link.decreasing)))
				} else {
					x = c(0, cumsum(abs(x1)))
				}
				x = rev(x)
				return(x[ri + 0:1])
			}
		}
	}

	pos = list(sector1 = character(0), sector2 = character(0),
		       x1 = numeric(0), x2 = numeric(0), y1 = numeric(0), y2 = numeric(0),
		       ri = numeric(0), ci = numeric(0))
	for(i in seq_along(rn)) {
		for(j in seq_along(cn)) {
			pos$sector1 = c(pos$sector1, rn[i])
			pos$sector2 = c(pos$sector2, cn[j])
			pos$x1 = c(pos$x1, .get_pos(mat, ri = i, ci = j, on = "row", sort = TRUE)[1])
			pos$x2 = c(pos$x2, .get_pos(mat, ri = i, ci = j, on = "row", sort = TRUE)[2])
			pos$y1 = c(pos$y1, .get_pos(mat, ri = i, ci = j, on = "column", sort = TRUE)[1])
			pos$y2 = c(pos$y2, .get_pos(mat, ri = i, ci = j, on = "column", sort = TRUE)[2])
			pos$ri = c(pos$ri, i)
			pos$ci = c(pos$ci, j)
		}
	}

	rou = get_most_inside_radius()
	if(directional) {
		if("diffHeight" %in% direction.type) {
			if(directional > 0) {
				if(diffHeight > 0) {
					rou1 = rou - diffHeight
					rou2 = rou
				} else {
					rou1 = rou
					rou2 = rou + diffHeight
				}
			} else {
				if(diffHeight > 0) {
					rou1 = rou
					rou2 = rou - diffHeight
				} else {
					rou1 = rou + diffHeight
					rou2 = rou
				}
			}
		} else {
			rou1 = rou
			rou2 = rou
		}
	} else {
		rou1 = rou
		rou2 = rou
	}

	for(k in seq_along(pos$sector1)) {
		
		sector.index1 = pos$sector1[k]
		sector.index2 = pos$sector2[k]
		i = pos$ri[k]
		j = pos$ci[k]

		if(abs(mat[i, j]) < 1e-8) {
			next
		}
		
		circos.link(sector.index1, c(pos$x1[k], pos$x2[k]),
					sector.index2, c(pos$y1[k], pos$y2[k]),
					directional = directional, col = col[i, j], rou1 = rou1, rou2 = rou2, border = link.border[i, j], 
					lwd = link.lwd[i, j], lty = link.lty[i, j], 
					arr.length = link.arr.length[i, j], arr.width = link.arr.width[i, j],
					arr.type = link.arr.type[i, j], arr.col = link.arr.col[i, j],
					arr.lty = link.arr.lty[i, j], arr.lwd = link.arr.lwd[i, j],
					...)
    }
	
	circos.par("cell.padding" = o.cell.padding)
}

# returns a list, each list containing settings for each new track
parsePreAllocateTracksValue = function(preAllocateTracks) {
	lt = list(ylim = c(0, 1),
		      track.height = circos.par("track.height"),
			  bg.col = NA,
			  bg.border = NA,
			  bg.lty = par("lty"),
			  bg.lwd = par("lwd"))
	if(length(preAllocateTracks) && is.numeric(preAllocateTracks)) {
		res = vector("list", length = preAllocateTracks)
		for(i in seq_len(preAllocateTracks)) {
			res[[i]] = lt
		}
		return(res)
	} else if(is.list(preAllocateTracks)) {
		# list of list
		if(all(sapply(preAllocateTracks, is.list))) {
			res = vector("list", length = length(preAllocateTracks))
			for(i in seq_along(preAllocateTracks)) {
				lt2 = lt
				for(nm in intersect(names(lt), names(preAllocateTracks[[i]]))) {
					lt2[[nm]] = preAllocateTracks[[i]][[nm]]
				}
				res[[i]] = lt2
			}
			return(res)
		} else {
			lt2 = lt
			for(nm in intersect(names(lt), names(preAllocateTracks))) {
				lt2[[nm]] = preAllocateTracks[[nm]]
			}
			return(list(lt2))
		}
	} else {
		stop("Wrong `preAllocateTracks` value.\n")
	}
}

# values can be:
# - a scalar
# - a matrix
# - a three column data frame
.normalize_to_mat = function(value, rn, cn, default) {
	var_name = deparse(substitute(value))
	mat = matrix(default, nrow = length(rn), ncol = length(cn))
	rownames(mat) = rn
	colnames(mat) = cn
	if(inherits(value, "data.frame")) {
		if(ncol(value) == 3) {
			value[[1]] = as.vector(value[[1]])
			value[[2]] = as.vector(value[[2]])
			value[[3]] = as.vector(value[[3]])

			l = value[, 1] %in% rn & value[, 2] %in% cn
			value = value[l, , drop = FALSE]
			for(i in seq_len(nrow(value))) {
				mat[ value[i, 1], value[i, 2] ] = value[i, 3]
			}
		} else {
			stop(paste0("If ", var_name, " is set as a data frame, it should have three columns."))
		}
	} else if(is.atomic(value) && length(value) == 1) {
		mat[,] = value
	} else {
		if(!is.null(rownames(value)) && !is.null(colnames(value))) {
			common_rn = intersect(rownames(value), rn)
			common_cn = intersect(colnames(value), cn)
			mat[common_rn, common_cn] = value[common_rn, common_cn]
		} else {
			if(nrow(value) == length(rn) && ncol(value) == length(cn)) {
				mat = value
				rownames(mat) = rn
				colnames(mat) = cn
			} else {
				stop(paste0("If ", var_name, " is a matrix, it should have both rownames and colnames.\n"))
			}
		}
	}
	return(mat)
}

# == title
# Adjust gaps to make chord diagrams comparable
#
# == param
# -mat1 matrix that has the largest sum of absolute
# -gap.degree gap.degree for the Chord Diagram which corresponds to ``mat1``
# -mat2 matrix to be compared
#
# == details
# Normally, in Chord Diagram, values in mat are normalized to the summation and each value is put 
# to the circle according to its percentage, which means the width for each link only represents 
# kind of relative value. However, when comparing two Chord Diagrams, it is necessary that unit 
# width of links in the two plots should be represented in a same scale. This problem can be solved by 
# adding more blank gaps to the Chord Diagram which has smaller values.
#
# == value
# Sum of gaps for ``mat2``.
#
normalizeChordDiagramGap = function(mat1, gap.degree = circos.par("gap.degree"), mat2) {
	percent = sum(abs(mat2)) / sum(abs(mat1))

	if(length(gap.degree) == 1) {
		gap.degree = rep(gap.degree, length(unique(rownames(mat1), colnames(mat1))))
	}
	blank.degree = (360 - sum(gap.degree)) * (1 - percent)
	return(blank.degree)
}

mat2df = function(mat) {
	nr = dim(mat)[1]
	nc = dim(mat)[2]
	rn = rep(rownames(mat), times = nc)
	ri = rep(seq_len(nr), times = nc)
	cn = rep(colnames(mat), each = nr)
	ci = rep(seq_len(nc), each = nr)
	v = as.vector(mat)
	df = data.frame(rn = rn, cn = cn, ri = ri, ci = ci, value = v, stringsAsFactors = FALSE)
	l = df$value > 0
	return(df[l, , drop = FALSE])
}

chordDiagram.matrix = function(x) {

}

chordDiagram.data.frame = function(x, grid.col = NULL, transparency = 0.5,
	col = NULL, from.col = NULL, to.col = NULL, directional = 0,
	direction.type = "diffHeight",
	order = NULL, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), grid.border = NA, 
	diffHeight = 0.04, reduce = 1e-5, link.sort = FALSE, link.decreasing = FALSE,
	link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4), 
	link.arr.width = link.arr.length/2, 
	link.arr.type = "triangle", link.arr.lty = par("lty"), 
	link.arr.lwd = par("lwd"), link.arr.col = par("col"), ...) {

}

chordDiagram = function(x, ...) {
	UseMethod("chordDiagram")
}
