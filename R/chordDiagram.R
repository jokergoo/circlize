
# == title
# Plot Chord Diagram
#
# == param
# -x a matrix or a data frame. The function will pass all argument to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame` depending on the type of ``x``,
#    also format of other arguments depends of the type of ``x``. If it is in the form of a matrix,
#    it should be an adjacency matrix. If it is in the form of a data frame, it should be an adjacency list.
# -grid.col pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -grid.border  pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -transparency pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -col pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -row.col pass to `chordDiagramFromMatrix`
# -column.col pass to `chordDiagramFromMatrix`
# -order  pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -directional pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -xmax maximum value on x-axes, the value should be a named vector.
# -symmetric pass to `chordDiagramFromMatrix`
# -keep.diagonal  pass to `chordDiagramFromMatrix`
# -direction.type pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -diffHeight pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -reduce pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -self.link pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -preAllocateTracks pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -annotationTrack pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -annotationTrackHeight pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.border pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.lwd pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.lty pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.sort pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.decreasing pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.length pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.width  pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.type  pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.lty pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.lwd pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.arr.col pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.largest.ontop  pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.visible pass to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame`
# -link.rank order to add links to the circle, a large value means to add it later.
# -scale scale each sector to same width
# -big.gap Gap between the two sets of sectors. If the input is a matrix, the two sets
#      are row sectors and column sectors. If the input is a data frame, the two sets
#      correspond to the first column and the second column. It only works when there
#      is no intersection between the two sets.
# -small.gap Small gap between sectors.
# -... pass to `circos.link`.
#
# == details
# Chord diagram is a way to visualize numeric tables ( http://circos.ca/intro/tabular_visualization/ ), especially useful
# when the table represents information of directional relations. This function
# visualize tables in a circular way.
#
# This function is flexible and contains some settings that may be a little difficult to understand. 
# Please refer to vignette for better explanation.
#
# == seealso
# http://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
#
# == value
# A data frame which contains positions of links, columns are:
#
# -``rn`` sector name corresponding to rows in the adjacency matrix or the first column in the adjacency list
# -``cn`` sector name corresponding to columns in the adjacency matrix or the second column in the adjacency list
# -``value`` value for the interaction or relation
# -``o1`` order of the link on the "from" sector
# -``o2`` order of the link on the "to" sector
# -``x1`` and position of the link on the "from" sector, the interval for the link on the "from" sector is ``c(x1-abs(value), x1)``
# -``x2`` and position of the link on the "to" sector, the interval for the link on the "from" sector is ``c(x2-abs(value), x2)``
# 
chordDiagram = function(x, grid.col = NULL, grid.border = NA, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, 
	order = NULL, directional = 0, xmax = NULL,
	symmetric = FALSE, keep.diagonal = FALSE, 
	direction.type = "diffHeight", diffHeight = convert_height(2, "mm"), 
	reduce = 1e-5, self.link = 2, 
	preAllocateTracks = NULL,
	annotationTrack = c("name", "grid", "axis"), 
	annotationTrackHeight = convert_height(c(3, 2), "mm"),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), 
	link.sort = FALSE, link.decreasing = TRUE,
	link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4), 
	link.arr.width = link.arr.length/2, 
	link.arr.type = "triangle", link.arr.lty = par("lty"), 
	link.arr.lwd = par("lwd"), link.arr.col = par("col"), 
	link.largest.ontop = FALSE, link.visible = TRUE, 
	link.rank = NULL, scale = FALSE, big.gap = 10, small.gap = 1, ...) {
		
	if(inherits(x, "table"))  {
		if(length(dim(x)) == 2) {
			class(x) = "matrix"
		}
	}
	if(inherits(x, "matrix")) {
		if(ncol(x) == 2 && nrow(x) > 10) {
			x = as.data.frame(x)
		}
	}

	if(inherits(x, "matrix")) {
		chordDiagramFromMatrix(x, grid.col = grid.col, grid.border = grid.border, transparency = transparency,
			col = col, row.col = row.col, column.col = column.col, order = order, directional = directional,
			symmetric = symmetric, keep.diagonal = keep.diagonal, direction.type = direction.type,
			diffHeight = diffHeight, reduce = reduce, self.link = self.link, xmax = xmax,
			preAllocateTracks = preAllocateTracks, annotationTrack = annotationTrack, annotationTrackHeight = annotationTrackHeight,
			link.border = link.border, link.lwd = link.lwd, link.lty = link.lty, link.sort = link.sort, link.decreasing = link.decreasing,
			link.arr.length = link.arr.length, link.arr.width = link.arr.width, link.arr.type = link.arr.type, link.arr.lty = link.arr.lty,
			link.arr.lwd = link.arr.lwd, link.arr.col = link.arr.col, link.largest.ontop = link.largest.ontop, 
			link.visible = link.visible, link.rank = link.rank, scale = scale, big.gap = big.gap, small.gap = small.gap, ...)
	} else {
		x = validate_data_frame(x)
		if(ncol(x) > 3) {
			if(all(sapply(x, inherits, c("numeric", "integer")))) {
				warning("It seems your input data is an adjacency matrix, maybe you need to convert it to 'matrix' explicitely.")
				return(chordDiagramFromMatrix(as.matrix(x, grid.col = grid.col, grid.border = grid.border, transparency = transparency,
					col = col, row.col = row.col, column.col = column.col, order = order, directional = directional,
					symmetric = symmetric, keep.diagonal = keep.diagonal, direction.type = direction.type,
					diffHeight = diffHeight, reduce = reduce, self.link = self.link, xmax = xmax,
					preAllocateTracks = preAllocateTracks, annotationTrack = annotationTrack, annotationTrackHeight = annotationTrackHeight,
					link.border = link.border, link.lwd = link.lwd, link.lty = link.lty, link.sort = link.sort, link.decreasing = link.decreasing,
					link.arr.length = link.arr.length, link.arr.width = link.arr.width, link.arr.type = link.arr.type, link.arr.lty = link.arr.lty,
					link.arr.lwd = link.arr.lwd, link.arr.col = link.arr.col, link.largest.ontop = link.largest.ontop, 
					link.visible = link.visible, link.rank = link.rank, scale = scale, big.gap = big.gap, small.gap = small.gap, ...)))
			} else {
				chordDiagramFromDataFrame(x, grid.col = grid.col, grid.border = grid.border, transparency = transparency,
					col = col, order = order, directional = directional, direction.type = direction.type,
					diffHeight = diffHeight, reduce = reduce, self.link = self.link, xmax = xmax,
					preAllocateTracks = preAllocateTracks, annotationTrack = annotationTrack, annotationTrackHeight = annotationTrackHeight,
					link.border = link.border, link.lwd = link.lwd, link.lty = link.lty, link.sort = link.sort, link.decreasing = link.decreasing,
					link.arr.length = link.arr.length, link.arr.width = link.arr.width, link.arr.type = link.arr.type, link.arr.lty = link.arr.lty,
					link.arr.lwd = link.arr.lwd, link.arr.col = link.arr.col, link.largest.ontop = link.largest.ontop, 
					link.visible = link.visible, link.rank = link.rank, scale = scale, big.gap = big.gap, small.gap = small.gap, ...)
			}
		} else {
			chordDiagramFromDataFrame(x, grid.col = grid.col, grid.border = grid.border, transparency = transparency,
				col = col, order = order, directional = directional, direction.type = direction.type,
				diffHeight = diffHeight, reduce = reduce, self.link = self.link, xmax = xmax,
				preAllocateTracks = preAllocateTracks, annotationTrack = annotationTrack, annotationTrackHeight = annotationTrackHeight,
				link.border = link.border, link.lwd = link.lwd, link.lty = link.lty, link.sort = link.sort, link.decreasing = link.decreasing,
				link.arr.length = link.arr.length, link.arr.width = link.arr.width, link.arr.type = link.arr.type, link.arr.lty = link.arr.lty,
				link.arr.lwd = link.arr.lwd, link.arr.col = link.arr.col, link.largest.ontop = link.largest.ontop, 
				link.visible = link.visible, link.rank = link.rank, scale = scale, big.gap = big.gap, small.gap = small.gap, ...)
		}
	}	
}

# returns a list, each list containing settings for each new track
parsePreAllocateTracksValue = function(preAllocateTracks) {
	lt = list(ylim = c(0, 1),
		      track.height = circos.par("track.height"),
			  bg.col = NA,
			  bg.border = NA,
			  bg.lty = par("lty"),
			  bg.lwd = par("lwd"),
			  track.margin = circos.par("track.margin"))
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
		stop_wrap("Wrong `preAllocateTracks` value.")
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
			stop_wrap("If ", var_name, " is set as a data frame, it should have three columns.")
		}
	} else if(is.atomic(value) && length(value) == 1) {
		mat[,] = value
	} else if(length(value) == length(rn) * length(cn)) {
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
				stop_wrap("If ", var_name, " is a matrix, it should have both rownames and colnames.")
			}
		}
	}
	return(mat)
}

# title
# Adjust gaps to make chord diagrams comparable
#
# == param
# -mat1 matrix that has the largest sum of absolute
# -gap.degree gap.degree for the Chord diagram which corresponds to ``mat1``
# -start.degree start.degree for the first Chord diagram
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
normalizeChordDiagramGap = function(mat1, gap.degree = circos.par("gap.degree"), 
	start.degree = circos.par("start.degree"), mat2) {
	percent = sum(abs(mat2)) / sum(abs(mat1))

	if(length(gap.degree) == 1) {
		gap.degree = rep(gap.degree, length(unique(rownames(mat1), colnames(mat1))))
	}
	blank.degree = (360 - sum(gap.degree)) * (1 - percent)
	big.gap = (blank.degree - sum(rep()))/2
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
	return(df)
}

# == title
# Plot Chord Diagram from an adjacency matrix
#
# == param
# -mat A table which represents as a numeric matrix.
# -grid.col Grid colors which correspond to matrix rows/columns (or sectors). The length of the vector should be either 1 or ``length(union(rownames(mat), colnames(mat)))``.
#           It's preferred that ``grid.col`` is a named vector of which names correspond to sectors. 
#           If it is not a named vector, the order of ``grid.col`` corresponds to order of sectors.
# -grid.border border for grids. If it is ``NULL``, the border color is same as grid color
# -transparency Transparency of link colors, 0 means no transparency and 1 means full transparency.
#               If transparency is already set in ``col`` or ``row.col`` or ``column.col``, this argument will be ignored.
#               ``NA``also ignores this argument.
# -col Colors for links. It can be a matrix which corresponds to ``mat``, or a function which generate colors 
#      according to values in ``mat``, or a single value which means colors for all links are the same, or a three-column
#      data frame in which the first two columns correspond to row names and columns and the third column is colors. You
#      may use `colorRamp2` to generate a function which maps values to colors.
# -row.col Colors for links. Links from the same row in ``mat`` will have the same color.
#          Length should be same as number of rows in ``mat``. This argument only works when ``col`` is set to ``NULL``.
# -column.col Colors for links. Links from the same column in ``mat`` will have the same color.
#             Length should be same as number of columns in ``mat``. This argument only works when ``col`` and ``row.col`` is set to ``NULL``.
# -order Order of sectors. Default order is ``union(df[[1]], df[[2]])``.
# -directional Whether links have directions. 1 means the direction is from the first column in ``df`` to the second column, -1
#              is the reverse, 0 is no direction, and 2 for two directional. Same setting as ``link.border``.
# -xmax maximum value on x-axes, the value should be a named vector.
# -direction.type type for representing directions. Can be one or two values in "diffHeight" and "arrows". If the value contains "diffHeight",
#            different heights of the links are used to represent the directions for which starting root has long height to give people feeling
#            that something is comming out. If the value contains "arrows", users can customize arrows with following arguments.
#            Same setting as ``link.border``. Note if you want to set both ``diffHeight``
#             and ``arrows`` for certain links, you need to embed these two options into one string such as ``"diffHeight+arrows"``.
# -diffHeight The difference of height between two 'roots' if ``directional`` is set to ``TRUE``. If the value is set to
#             a positive value, start root is shorter than end root and if it is set to a negative value, start root is longer
#             than the end root.
# -reduce if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.
#         Set it to value less than zero if you want to keep all tiny grid.
# -self.link if there is a self link in one sector, 1 means the link will be degenerated as a 'mountain' and the width corresponds to the value for this connection.
#            2 means the width of the starting root and the ending root all have the width that corresponds to the value for the connection.
# -symmetric Whether the matrix is symmetric. If the value is set to ``TRUE``, only
#            lower triangular matrix without the diagonal will be used.
# -keep.diagonal If the matrix is specified as symmetric, whether keep diagonal for visualization.
# -preAllocateTracks Pre-allocate empty tracks before drawing Chord diagram. It can be a single number indicating
#                    how many empty tracks needed to be created or a list containing settings for empty
#                    tracks. Please refer to vignette for details.
# -annotationTrack Which annotation track should be plotted? By default, a track containing sector names and a track
#                  containing grid will be created.
# -annotationTrackHeight Track height corresponding to values in ``annotationTrack``.
# -link.border border for links, single scalar or a matrix with names or a data frame with three columns
# -link.lwd width for link borders, single scalar or a matrix with names or a data frame with three columns
# -link.lty style for link borders, single scalar or a matrix with names or a data frame with three columns
# -link.sort whether sort links on every sector based on the width of the links on it. If it is set to "overall", all links are sorted regardless
#            whether they are from rows or columns.
# -link.decreasing for ``link.sort``
# -link.arr.length pass to `circos.link`. The format of this argument is same as ``link.lwd``.
# -link.arr.width pass to `shape::Arrowhead`. The format of this argument is same as ``link.lwd``.
# -link.arr.type pass to `circos.link`, same format as ``link.lwd``. Default value is ``triangle``.
# -link.arr.col color or the single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.arr.lwd line width ofthe single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.arr.lty line type of the single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.largest.ontop controls the order of adding links, whether based on the absolute value?
# -link.visible whether plot the link. The value is logical, if it is set to ``FALSE``, the corresponding link will not 
#            plotted, but the space is still ocuppied. The format of this argument is same as ``link.lwd``
# -link.rank order to add links to the circle, a large value means to add it later.
# -scale scale each sector to same width
# -big.gap Gap between row sectors and column sectors.
# -small.gap Small gap between sectors.
# -... pass to `circos.link`
#
# == details
# Internally, the matrix is transformed to a data frame and sent to `chordDiagramFromDataFrame`.
#
# == value
# A data frame which contains positions of links, see explanation in `chordDiagram`.
#
chordDiagramFromMatrix = function(mat, grid.col = NULL, grid.border = NA, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, order = NULL, directional = 0,
	direction.type = "diffHeight", diffHeight = convert_height(2, "mm"), 
	reduce = 1e-5, xmax = NULL, self.link = 2,
	symmetric = FALSE, keep.diagonal = FALSE, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid", "axis"), 
	annotationTrackHeight = convert_height(c(3, 2), "mm"),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), 
	link.sort = FALSE, link.decreasing = TRUE,
	link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4), 
	link.arr.width = link.arr.length/2, 
	link.arr.type = "triangle", link.arr.lty = par("lty"), 
	link.arr.lwd = par("lwd"), link.arr.col = par("col"), 
	link.largest.ontop = FALSE, link.visible = TRUE, 
	link.rank = NULL, scale = FALSE, big.gap = 10, small.gap = 1, ...) {
	
	if(!is.matrix(mat)) {
		stop_wrap("`mat` can only be a matrix.")
	}

	if(length(mat) != 2) {
		if(identical(direction.type, c("diffHeight", "arrows")) || identical(direction.type, c("arrows", "diffHeight"))) {
			direction.type = "diffHeight+arrows"
		}
	}

	if(is.null(link.rank)) {
		link.rank = 1
	}

	transparency = ifelse(transparency < 0, 0, ifelse(transparency > 1, 1, transparency))

	if(symmetric) {
		if(nrow(mat) != ncol(mat)) {
			stop_wrap("`mat` should be a square matrix.")
		}

		for(i in 1:10) {
			n = sample(nrow(mat), 2)
			ir = n[1]
			ic = n[2]
			if(abs(mat[ir, ic] - mat[ic, ir]) > 1e-8) {
				stop("Is `mat` really a symmetric matrix?")
			}
		}

		if(is.null(rownames(mat)) && is.null(colnames(mat))) {
			rownames(mat) = paste0("S", seq_len(nrow(mat)))
			colnames(mat) = paste0("S", seq_len(ncol(mat)))
		}

		if(!setequal(rownames(mat), colnames(mat))) {
			stop_wrap("Since you specified a symmetric matrix, rownames and colnames should be the same.")
		}

		mat[upper.tri(mat, diag = !keep.diagonal)] = 0
		
	}

	mat[is.na(mat)] = 0

	if(!is.null(order)) {
		if(is.null(rownames(mat)) || is.null(colnames(mat))) {
			stop_wrap("Since you specified `order`, your matrix should have rowname and colname.")
		}
		if(!setequal(order, union(rownames(mat), colnames(mat)))) {
			stop_wrap("Elements in `order` should be same as in `union(rownames(mat), colnames(mat))`.")
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

	ri_zero_sum = ri[ rowSums(mat[ri, ci, drop = FALSE]) == 0]
	ci_zero_sum = ci[ colSums(mat[ri, ci, drop = FALSE]) == 0]

	while(length(ri_zero_sum) || length(ci_zero_sum)) {
		ri = setdiff(ri, ri_zero_sum)
		ci = setdiff(ci, ci_zero_sum)

		ri_zero_sum = ri[ rowSums(mat[ri, ci, drop = FALSE]) == 0]
		ci_zero_sum = ci[ colSums(mat[ri, ci, drop = FALSE]) == 0]
	}

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
		if(!is.null(names(grid.col))) {
			unnamed_grid = setdiff(factors, names(grid.col))
			if(length(unnamed_grid) > 0) {
				grid.col = c(grid.col, structure(rand_color(length(unnamed_grid)), names = unnamed_grid))
				# stop("Since your ``grid.col`` is a named vector, all sectors should have corresponding colors.\n")
			}
			grid.col = grid.col[as.vector(factors)]
		} else if(length(grid.col) == 1) {
			grid.col = rep(grid.col, n)
			names(grid.col) = factors
		} else if(length(grid.col) == length(factors)) {
			names(grid.col) = factors
		} else {
			stop_wrap("Since you set ``grid.col``, the length should be either 1 or number of sectors, or set your ``grid.col`` as vector with names.")
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
	if(is.na(transparency)) {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
	} else if(all(rgb_mat[, 4] == 255)) {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = (1-transparency)*255)
	} else {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
	}

	dim(col) = dim(mat)
	colnames(col) = colnames(mat)
	rownames(col) = rownames(mat)

	link.border = .normalize_to_mat(link.border, rn, cn, default = NA)
	link.lwd = .normalize_to_mat(link.lwd, rn, cn, default = 1)
	link.lty = .normalize_to_mat(link.lty, rn, cn, default = 1)
	link.arr.length = .normalize_to_mat(link.arr.length, rn, cn, default = 0.4)
	link.arr.width = .normalize_to_mat(link.arr.width, rn, cn, default = 0.2)
	link.arr.type = .normalize_to_mat(link.arr.type, rn, cn, default = "triangle")
	link.arr.lty = .normalize_to_mat(link.arr.lty, rn, cn, default = 1)
	link.arr.lwd = .normalize_to_mat(link.arr.lwd, rn, cn, default = 1)
	link.arr.col = .normalize_to_mat(link.arr.col, rn, cn, default = NA)
	link.visible = .normalize_to_mat(link.visible, rn, cn, default = NA)
	link.rank = .normalize_to_mat(link.rank, rn, cn, default = NA)

	directional = .normalize_to_mat(directional, rn, cn, default = 0)
	direction.type = .normalize_to_mat(direction.type, rn, cn, default = "diffHeight")

	df = mat2df(mat)

	chordDiagramFromDataFrame(df[c(1, 2, 5)], grid.col = grid.col, grid.border = grid.border, transparency = NA,
		col = psubset(col, df$ri, df$ci), order = order, xmax = xmax,
		directional = psubset(directional, df$ri, df$ci),
		direction.type = psubset(direction.type, df$ri, df$ci), 
		diffHeight = diffHeight, 
		reduce = 0, self.link = self.link,
		preAllocateTracks = preAllocateTracks,
		annotationTrack = annotationTrack, annotationTrackHeight = annotationTrackHeight,
		link.sort = link.sort, link.decreasing = link.decreasing,
		link.border = psubset(link.border, df$ri, df$ci), 
		link.lwd = psubset(link.lwd, df$ri, df$ci),
		link.lty = psubset(link.lty, df$ri, df$ci), 
		link.arr.length = psubset(link.arr.length, df$ri, df$ci),
		link.arr.width = psubset(link.arr.width, df$ri, df$ci),
		link.arr.type = psubset(link.arr.type, df$ri, df$ci),
		link.arr.lwd = psubset(link.arr.lwd, df$ri, df$ci),
		link.arr.lty = psubset(link.arr.lty, df$ri, df$ci),
		link.arr.col = psubset(link.arr.col, df$ri, df$ci),
		link.largest.ontop = link.largest.ontop,
		link.visible = link.visible,
		link.rank = link.rank,
		scale = scale,
		big.gap = big.gap,
		small.gap = small.gap,
		...)
	
}


# == title
# Plot Chord Diagram from a data frame
#
# == param
# -df A data frame with at least two columns. The first two columns specify the connections and the third column (optional)
#     contains numeric values which are mapped to the width of links as well as the colors if ``col`` is specified as a color mapping function.
#     The sectors in the plot will be ``union(df[[1]], df[[2]])``.
# -grid.col Grid colors which correspond to sectors. The length of the vector should be either 1 or the number of sectors.
#           It's preferred that ``grid.col`` is a named vector of which names correspond to sectors. 
#           If it is not a named vector, the order of ``grid.col`` corresponds to order of sectors.
# -grid.border border for grids. If it is ``NULL``, the border color is same as grid color
# -transparency Transparency of link colors, 0 means no transparency and 1 means full transparency.
#               If transparency is already set in ``col`` or ``row.col`` or ``column.col``, this argument will be ignored.
#               ``NA``also ignores this argument.
# -col Colors for links. It can be a vector which corresponds to connections in ``df``, or a function which generate colors 
#      according to values (the third column) in ``df``, or a single value which means colors for all links are the same. You
#      may use `colorRamp2` to generate a function which maps values to colors.
# -order Order of sectors. Default order is ``union(df[[1]], df[[2]])``.
# -directional Whether links have directions. 1 means the direction is from the first column in ``df`` to the second column, -1
#              is the reverse, 0 is no direction, and 2 for two directional. The value can be a vector which has same length as number of rows in ``df``.
# -xmax maximum value on x-axes, the value should be a named vector.
# -direction.type type for representing directions. Can be one or two values in "diffHeight" and "arrows". If the value contains "diffHeight",
#            different heights of the links are used to represent the directions for which starting root has long height to give people feeling
#            that something is comming out. If the value contains "arrows", users can customize arrows with following arguments. 
#             The value can be a vector which has same length as number of rows in ``df``. Note if you want to set both ``diffHeight``
#             and ``arrows`` for certain links, you need to embed these two options into one string such as ``"diffHeight+arrows"``.
# -diffHeight The difference of height between two 'roots' if ``directional`` is set to ``TRUE``. If the value is set to
#             a positive value, start root is shorter than end root and if it is set to a negative value, start root is longer
#             than the end root. The value can be a vector which has same length as number of rows in ``df``.
# -reduce if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.
#         Set it to value less than zero if you want to keep all tiny grid.
# -self.link if there is a self link in one sector, 1 means the link will be degenerated as a 'mountain' and the width corresponds to the value for this connection.
#            2 means the width of the starting root and the ending root all have the same width that corresponds to the value for the connection.
# -preAllocateTracks Pre-allocate empty tracks before drawing Chord diagram. It can be a single number indicating
#                    how many empty tracks needed to be created or a list containing settings for empty
#                    tracks. Please refer to vignette for details.
# -annotationTrack Which annotation track should be plotted? By default, a track containing sector names and a track
#                  containing grid will be created.
# -annotationTrackHeight Track height corresponding to values in ``annotationTrack``.
# -link.border border for links, single scalar or a vector which has the same length as nrows of ``df`` or a data frame
# -link.lwd width for link borders, single scalar or a vector which has the same length as nrows of ``df`` or a data frame
# -link.lty style for link borders, single scalar or a vector which has the same length as nrows of ``df`` or a data frame
# -link.sort whether sort links on every sector based on the width of the links on it. If it is set to "overall", all links are sorted regardless
#            whether they are from the first column or the second column.
# -link.decreasing for ``link.sort``
# -link.arr.length pass to `circos.link`. The format of this argument is same as ``link.lwd``.
# -link.arr.width pass to `shape::Arrowhead`. The format of this argument is same as ``link.lwd``.
# -link.arr.type pass to `circos.link`, same settings as ``link.lwd``. Default value is ``triangle``.
# -link.arr.col color or the single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.arr.lwd line width ofthe single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.arr.lty line type of the single line link which is put in the center of the belt. The format of this argument is same as ``link.lwd``.
# -link.largest.ontop controls the order of adding links, whether based on the absolute value?
# -link.visible whether plot the link. The value is logical, if it is set to ``FALSE``, the corresponding link will not 
#            plotted, but the space is still ocuppied. The format of this argument is same as ``link.lwd``
# -link.rank order to add links to the circle, a large value means to add it later.
# -scale scale each sector to same width
# -big.gap Gaps between the sectors in the first column of ``df`` and sectors in the second column in ``df``.
# -small.gap Small gap between sectors.
# -... pass to `circos.link`
#
# == details
# The data frame can have a column named "rank" which is used to control the order of adding links to the diagram.
#
# == value
# A data frame which contains positions of links, see explanation in `chordDiagram`.
#
chordDiagramFromDataFrame = function(df, grid.col = NULL, grid.border = NA, transparency = 0.5,
	col = NULL, order = NULL, directional = 0, xmax = NULL,
	direction.type = "diffHeight", diffHeight = convert_height(2, "mm"), 
	reduce = 1e-5, self.link = 2, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid", "axis"), 
	annotationTrackHeight = convert_height(c(3, 2), "mm"),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), 
	link.sort = FALSE, link.decreasing = TRUE,
	link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4), 
	link.arr.width = link.arr.length/2, 
	link.arr.type = "triangle", link.arr.lty = par("lty"), 
	link.arr.lwd = par("lwd"), link.arr.col = par("col"), 
	link.largest.ontop = FALSE, link.visible = TRUE, 
	link.rank = seq_len(nrow(df)), 
	scale = FALSE, 
	big.gap = 10, small.gap = 1,
	...) {

	if(nrow(df) != 2) {
		if(identical(direction.type, c("diffHeight", "arrows")) || identical(direction.type, c("arrows", "diffHeight"))) {
			direction.type = "diffHeight+arrows"
		}
	}

	# check the format of the data frame
	if(!inherits(df, "data.frame")) {
		stop_wrap("`df` must be a data frame.")
	}
	df = as.data.frame(df)
	if(ncol(df) < 2) {
		stop_wrap("`df` should have at least have two columns.")
	}
	if(ncol(df) == 2) {
		df[, 3] = rep(1, nrow(df))
		df[, 4] = df[, 3]
	} else if(ncol(df) == 3) {
		df[, 4] = df[, 3]
	}
	if(scale) {
		for(nm in unique(df[, 1])) df[ df[, 1] == nm, 4] = df[ df[, 1] == nm, 3]/sum(df[ df[, 1] == nm, 3])
		for(nm in unique(df[, 2])) df[ df[, 2] == nm, 5] = df[ df[, 2] == nm, 3]/sum(df[ df[, 2] == nm, 3])
		df = df[, -3]
	}
	df2 = df[1:4]
	df2[[1]] = as.character(df2[[1]])
	df2[[2]] = as.character(df2[[2]])
	colnames(df2) = c("rn", "cn", "value1", "value2")
	df = df2
	nr = nrow(df)

	if(is.null(link.rank)) {
		link.rank = seq_len(nrow(df))
	}

	transparency = ifelse(transparency < 0, 0, ifelse(transparency > 1, 1, transparency))

	cate = union(df[[1]], df[[2]])
	if(!is.null(order)) {
		order = intersect(order, cate)
		if(length(order) != length(cate)) {
			stop_wrap("`order` should contain names of all sectors.")
		}
		if(is.numeric(order)) {
			if(!setequal(order, seq_along(cate))) {
				stop_wrap("`order` needs to be integers ranging from 1 to", length(cate))
			}
			cate = cate[order]
		} else {
			if(!setequal(order, cate)) {
				stop_wrap("`order` should only be picked from sectors.")	
			}
			cate = order
		}
	}

	n = length(cate)
	if(is.null(grid.col)) {
		grid.col = rand_color(n)
		names(grid.col) = cate
	} else {
		if(!is.null(names(grid.col))) {
			unnamed_grid = setdiff(cate, names(grid.col))
			if(length(unnamed_grid) > 0) {
				grid.col = c(grid.col, structure(rand_color(length(unnamed_grid)), names = unnamed_grid))
				# stop("Since your ``grid.col`` is a named vector, all sectors should have corresponding colors.\n")
			}
			grid.col = grid.col[as.vector(cate)]
		} else if(length(grid.col) == 1) {
			grid.col = rep(grid.col, n)
			names(grid.col) = cate
		} else if(length(grid.col) == length(cate)) {
			names(grid.col) = cate
		} else {
			stop_wrap("Since you set ``grid.col``, the length should be either 1 or number of sectors, or set your ``grid.col`` as vector with names.")
		}
	}

	# colors
	if(is.null(col)) {
		col = grid.col[df[[1]]]
	} else {
		if(is.function(col)) {
			col = col(pmax(df$value1, df$value2))
		} else {
			col = rep(col, nr)[1:nr]
		}
	} 

	rgb_mat = t(col2rgb(col, alpha = TRUE))
	if(length(transparency) == nrow(rgb_mat)) {
		col = rgb(rgb_mat, maxColorValue = 255, alpha = (1-transparency)*255)
	} else {
		if(is.na(transparency)) {
			col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
		} else if(all(rgb_mat[, 4] == 255)) {
			col = rgb(rgb_mat, maxColorValue = 255, alpha = (1-transparency)*255)
		} else {
			col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
		}
	}

	.normalize_to_vector = function(x, link, default) {
		n = nrow(link)
		if(inherits(x, "data.frame")) {
			y = rep(default, n)
			xv = x[[3]]
			names(xv) = paste(x[[1]], x[[2]], sep = "$%^")
			lv = paste(link[[1]], link[[2]], sep = "$%^")
			names(y) = lv
			y[names(xv)] = xv
			y
		} else if(length(x) == 1) {
			rep(x, n)
		} else {
			x
		}
	}

	link.border = .normalize_to_vector(link.border, df[1:2], default = NA)
	link.lwd = .normalize_to_vector(link.lwd, df[1:2], default = 1)
	link.lty = .normalize_to_vector(link.lty, df[1:2], default = 1)
	link.arr.length = .normalize_to_vector(link.arr.length, df[1:2], default = 0.4)
	link.arr.width = .normalize_to_vector(link.arr.width, df[1:2], default = 0.2)
	link.arr.type = .normalize_to_vector(link.arr.type, df[1:2], default = "triangle")
	link.arr.lty = .normalize_to_vector(link.arr.lty, df[1:2], default = 1)
	link.arr.lwd = .normalize_to_vector(link.arr.lwd, df[1:2], default = 1)
	link.arr.col = .normalize_to_vector(link.arr.col, df[1:2], default = NA)
	link.visible = .normalize_to_vector(link.visible, df[1:2], default = NA)
	link.rank = .normalize_to_vector(link.rank, df[1:2], default = NA)
	directional = .normalize_to_vector(directional, df[1:2], default = 0)
	direction.type = .normalize_to_vector(direction.type, df[1:2], default = "diffHeight")


	#### reduce the data frame
	onr = nrow(df)
	while(1) {
		xsum = structure(rep(0, length(cate)), names = cate)
		for(i in seq_len(nr)) {
			if(df$rn[i] == df$cn[i]) {
				xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value1[i])
				if(self.link == 2) {
					xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value2[i])  # <<- self-link!!!!!
				}
			} else {
				xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value1[i])
				xsum[df$cn[i]] = xsum[df$cn[i]] + abs(df$value2[i])
			}
		}

		keep = names(xsum)[xsum / sum(xsum) >= reduce]
		l = df$rn %in% keep & df$cn %in% keep

		cate = intersect(cate, keep)
		df = df[l, , drop = FALSE]
		grid.col = grid.col[intersect(names(grid.col), keep)]
		col = col[l]
		link.border = link.border[l]
		link.lwd = link.lwd[l]
		link.lty = link.lty[l] 
		link.arr.length = link.arr.length[l]
		link.arr.width = link.arr.width[l]
		link.arr.type = link.arr.type[l]
		link.arr.lwd = link.arr.lwd[l]
		link.arr.lty = link.arr.lty[l]
		link.arr.col = link.arr.col[l]
		link.visible = link.visible[l]
		link.rank = link.rank[l]
		directional = directional[l]
		direction.type = direction.type[l]

		nr = nrow(df)
		reduce = 1e-10
		if(nr == onr) break
		onr = nr
	}

	# re-calcualte xsum
	xsum = structure(rep(0, length(cate)), names = cate)
	for(i in seq_len(nr)) {
		if(df$rn[i] == df$cn[i]) {
			xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value1[i])
			if(self.link == 2) {
				xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value2[i])  # <<- self-link!!!!!
			}
		} else {
			xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value1[i])
			xsum[df$cn[i]] = xsum[df$cn[i]] + abs(df$value2[i])
		}
	}

	# add additinal columns in df
	df$o1 = rep(0, nr)  # order of the link root in the sector
	df$o2 = rep(0, nr)  # order of the other link root in the sector
	df$x1 = rep(0, nr)  # end position of the link root in the sector
	df$x2 = rep(0, nr)  # end position on the other link root in the sector

	######## sort links on every sector
	# row first
	.order = function(x, sort = FALSE, reverse = FALSE) {
		if(sort) {
			od = order(x)
		} else {
			od = seq_along(x)
		}
		if(reverse) {
			od = rev(od)
		}
		return(od)
	}

	if(length(link.sort) == 1) link.sort = rep(link.sort, 2)
	if(length(link.decreasing) == 1) link.decreasing = rep(link.decreasing, 2)

	if(is.logical(link.sort)) {
		# position of root 1
		od = tapply(abs(df$value1), df$rn, .order, link.sort[1], link.decreasing[1])
		for(nm in names(od)) {  # for each sector
			l = df$rn == nm # rows in df that correspond to current sector
			df$o1[l] = od[[nm]] # adjust rows according to the order in current sector
			df$x1[l][od[[nm]]] = cumsum(abs(df$value1[l])[od[[nm]]]) # position

			l2 = df$rn == nm & df$cn == nm 
			if(sum(l2)) { # there is a self link
				if(self.link == 1) {
					df$x2[l2] = df$x1[l2]+abs(df$value1[l2])*0.000001
				}
			}
		}
		max_o1 = sapply(od, max)
		sum_1 = tapply(abs(df$value2), df$rn, sum)
		# position of root 2
		od = tapply(abs(df$value2), df$cn, .order, link.sort[2], link.decreasing[2])
		for(nm in names(od)) {
			if(!is.na(max_o1[nm])) { # if cn already in rn
				l = df$cn == nm
				if(self.link == 1) {
					l2 = ! df$rn[l] == nm # self link
					# od[[nm]][l2] = order(od[[nm]][l2])
					if(sum(l2)) {
						od[[nm]] = order(od[[nm]][l2])
					}
				} else {
					l2 = rep(TRUE, sum(l))
				}
				df$o2[l][l2] = od[[nm]] + max_o1[nm]
				df$x2[l][l2][ od[[nm]] ] = cumsum(abs(df$value2[l][l2])[ od[[nm]] ]) + sum_1[nm]
			} else {
				l = df$cn == nm
				df$o2[l] = od[[nm]]
				df$x2[l][od[[nm]]] = cumsum(abs(df$value2[l])[od[[nm]]])
			}
		}
		if(self.link == 1) {
			l = df$rn == df$cn
			df$x1[l] = pmin(df$x1[l], df$x2[l])
			df$x2[l] = pmin(df$x1[l], df$x2[l])
		}
	} else {
		for(nm in unique(c(df$rn, df$cn))) {
			l = df$rn == nm | df$cn == nm
			od = order(abs(df$value1[l]), decreasing = link.decreasing[1])
			cum_pos = cumsum(abs(df$value[l])[od])
			if(self.link == 2) {
				ii = which(df$rn[l] == nm & df$cn[l] == nm)
				if(length(ii)) {
					iii = which(od == ii)
					if(iii < length(cum_pos)) {
						cum_pos[(iii+1):length(cum_pos)] = cum_pos[(iii+1):length(cum_pos)] + df$value[l][ii]
					}
				}
			}
			xx = NULL
			xx[od] = cum_pos
			l1 = df$rn == nm
			l2 = l & !l1
			df$x1[l1] = xx[l1[l]]
			df$x2[l2] = xx[l2[l]]
		}
		l = df$rn == df$cn
		if(self.link == 1) {
			df$x2[l] = df$x1[l]
		} else if(self.link == 2) {
			df$x2[l] = df$x1[l] + abs(df$value[l])
		}
	}

	#######################################

	if(!is.null(xmax)) {
		overlap = intersect(names(xmax), names(xsum))
		xmax = xmax[overlap]
		xmax = xmax[xmax > xsum[overlap]]
		if(length(xmax)) {
			xsum[names(xmax)] = xmax
		}
	}


	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(0, 0, 0, 0))
    o.start.degree = circos.par("start.degree")
    o.gap.after = circos.par("gap.after")

	if((identical(circos.par("gap.after"), 1))) {  # gap.after is not set in circos.par()
		if(length(intersect(df[, 1], df[, 2])) == 0) {
			n_df1 = length(unique(df[, 1]))
			n_df2 = length(unique(df[, 2]))
			n_sector = n_df1 + n_df2
			s1 = sum(abs(df[, 3]))
			s2 = sum(abs(df[, 4]))
			d1 =  (360 - small.gap*(n_sector - 2) - big.gap*2) * (s1/(s1 + s2)) + small.gap*(n_df1-1)
			if(circos.par$start.degree == 1) circos.par$start.degree = 0
			if(circos.par$clock.wise) {
				start_degree = circos.par$start.degree - (180 - d1)/2
			} else {
				start_degree = circos.par$start.degree + (180 - d1)/2
			}
			suppressWarnings(circos.par(start.degree = start_degree, 
				gap.after = c(rep(small.gap, n_df1 - 1), big.gap, rep(small.gap, n_df2 - 1), big.gap)))
		} else {
			# warning("The two sets of sectors overlap, ignore `gap.degree`.")
		}
	} else {
		# warning("You have changed the default value of circos.par('gap.degree') or circos.par('gap.after').\nIgnore `gap.degree` argument.")
	}
    circos.initialize(factors = factor(cate, levels = cate), xlim = cbind(rep(0, length(xsum)), xsum))

	# pre allocate track
	if(!is.null(preAllocateTracks)) {
		pa = parsePreAllocateTracksValue(preAllocateTracks)
		for(i in seq_along(pa)) {
			va = pa[[i]]
			circos.trackPlotRegion(ylim = va$ylim, track.height = va$track.height,
				bg.col = va$bg.col, bg.border = va$bg.border, bg.lty = va$bg.lty, bg.lwd = va$bg.lwd,
				track.margin = va$track.margin)
		}
	}
	if("name" %in% annotationTrack) {
		circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA,
			panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				i = get.cell.meta.data("sector.numeric.index")
				circos.text(mean(xlim), 0.9, labels = current.sector.index, cex = 0.8,
					facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
			}, track.height = annotationTrackHeight[which(annotationTrack %in% "name")])
    }
	if("grid" %in% annotationTrack) {
		circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, 
			panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				if(is.null(grid.border)) {
					border.col = grid.col[current.sector.index]
				} else {
					border.col = grid.border
				}
				circos.rect(xlim[1], 0, xlim[2], 1, col = grid.col[current.sector.index], border = border.col)
				if("axis" %in% annotationTrack) {
					circos.axis("top", labels.cex = 0.5)
				}
			}, track.height = annotationTrackHeight[which(annotationTrack %in% "grid")])
	}
    
    rou = get_most_inside_radius()
    rou1 = numeric(nr)
    rou2 = numeric(nr)
    for(i in seq_len(nr)) {
		if(directional[i]) {
			if(grepl("diffHeight", direction.type[i])) {
				if(directional[i] == 1) {
					if(diffHeight > 0) {
						rou1[i] = rou - diffHeight
						rou2[i] = rou
					} else {
						rou1[i] = rou
						rou2[i] = rou + diffHeight
					}
				} else if(directional[i] == -1) {
					if(diffHeight > 0) {
						rou1[i] = rou
						rou2[i] = rou - diffHeight
					} else {
						rou1[i] = rou + diffHeight
						rou2[i] = rou
					}
				} else  if(directional[i] == 2) {
					if(diffHeight > 0) {
						rou1[i] = rou - diffHeight
						rou2[i] = rou - diffHeight
					} else {
						rou1[i] = rou + diffHeight
						rou2[i] = rou + diffHeight
					}
				}
			} else {
				rou1[i] = rou
				rou2[i] = rou
			}
		} else {
			rou1[i] = rou
			rou2[i] = rou
		}
	}

	if(link.largest.ontop) {
		link_order = order(pmax(abs(df$value1), abs(df$value2)), decreasing = FALSE)
	} else {
		link_order = order(link.rank)
	}

	for(k in link_order) {
		if(abs(df$value1[k])/sum(abs(df$value1)) < 1e-6 && abs(df$value2[k])/sum(abs(df$value2)) < 1e-6) next
		if(link.visible[k]) {
			if(setequal(direction.type, c("diffHeight"))) {
				circos.link(df$rn[k], c(df$x1[k] - abs(df$value1[k]), df$x1[k]),
						df$cn[k], c(df$x2[k] - abs(df$value2[k]), df$x2[k]),
						directional = 0, col = col[k], rou1 = rou1[k], rou2 = rou2[k], 
						border = link.border[k], lwd = link.lwd[k], lty = link.lty[k],
						...)	
			} else if(grepl("arrows", direction.type[k])) {
				circos.link(df$rn[k], c(df$x1[k] - abs(df$value1[k]), df$x1[k]),
							df$cn[k], c(df$x2[k] - abs(df$value2[k]), df$x2[k]),
							directional = directional[k], col = col[k], rou1 = rou1[k], rou2 = rou2[k], 
							border = link.border[k], 
							lwd = link.lwd[k], lty = link.lty[k], 
							arr.length = link.arr.length[k], arr.width = link.arr.width[k],
							arr.type = link.arr.type[k], arr.col = link.arr.col[k],
							arr.lty = link.arr.lty[k], arr.lwd = link.arr.lwd[k],
							...)
			}
		}
    }
	
	df$col = col

	suppressWarnings(circos.par("cell.padding" = o.cell.padding, "start.degree" = o.start.degree,
		"gap.after" = o.gap.after))
	return(invisible(df))
}

psubset = function(mat, ri, ci) {
	return(mat[ri + (ci - 1) * nrow(mat)])
}

# == title
# Calculate gap to make two Chord diagram with same scale
#
# == param
# -x1 The matrix or the data frame for the first Chord diagram.
# -x2 The matrix or the data frame for the second Chord diagram.
# -big.gap ``big.gap`` for the first Chord diagram.
# -small.gap ``small.gap`` for both Chord diagrams.
#
# == details
# There should be no overlap between the two sets of sectors.
#
# == value
# A numeric value which can be directly set to ``big.gap`` in the second Chord diagram.
#
calc_gap = function(x1, x2, big.gap = 10, small.gap = 1) {
	if(is.matrix(x1)) {
		sum1 = sum(abs(x1))
		n1 = nrow(x1)
		n2 = ncol(x1)
	} else {
		sum1 = sum(abs(x1[, 3]))
		n1 = length(unique(x1[, 1]))
		n2 = length(unique(x1[, 2]))
	}
	sum_gap1 = sum(c(rep(small.gap, n1 - 1), big.gap, rep(small.gap, n2 - 1), big.gap))
		
	if(is.matrix(x2)) {
		sum2 = sum(abs(x2))
		n1 = nrow(x2)
		n2 = ncol(x2)
	} else {
		sum2 = sum(abs(x2[, 3]))
		n1 = length(unique(x2[, 1]))
		n2 = length(unique(x2[, 2]))
	}
	sum_gap2 = sum(rep(small.gap, n1 + n2 - 2))

	percent = sum2 / sum1
	blank.degree = (360 - sum_gap1) * (1 - percent)

	(blank.degree - sum_gap2)/2
}

