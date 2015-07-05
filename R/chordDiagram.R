
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
# -fromRows Unequal height of link root is used to represent the link direction.
#           If links are directional, whether they start from Rows. The starting root is always
#           more inside to circle centre than the ending root.
# -directional Whether links have directions. The directions are always from rows to columns. If you
#              want the direction from columns to rows, set ``fromRow`` to ``FALSE``.
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
# -link.order order of links in single sector. The value is a length-two vector which 
#         controls order of sectors which correspond to rows and columns respectively.
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
	col = NULL, row.col = NULL, column.col = NULL, directional = FALSE, 
	direction.type = "diffHeight", fromRows = TRUE,
	symmetric = FALSE, keep.diagonal = FALSE, order = NULL, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), grid.border = NA, 
	diffHeight = 0.04, reduce = 1e-5, link.order = -1,
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
	
    sector.sum.row = numeric(length(factors))
    sector.sum.col = numeric(length(factors))
	names(sector.sum.row) = factors
	names(sector.sum.col) = factors
	sector.sum.col[ names(rs) ] = rs

	if(length(link.order) == 1) {
		link.order = rep(link.order, 2)
	}
	if(length(link.order) != 2) {
		stop("`link.order` should be of length 1 or 2.")
	}
	if(!all(link.order %in% c(-1, 1))) {
		stop("`link.order` can only be in c(-1, 1).")
	}

	if(link.order[1] == 1) {
		row_index = seq_along(rn)
	} else {
		row_index = rev(seq_along(rn))	
	}
    for(i in row_index) {
		# if one name exists in both rows and columns, put it 
		if(link.order[2] == 1) {
			cn_index = seq_along(cn)
		} else {
			cn_index = rev(seq_along(cn))
		}
		if(rn[i] %in% cn) {
			is = which(cn == rn[i])
			cn_index = c(is, cn_index[cn_index != is])  # self link is always put in the first?
		}
		
		for(j in cn_index) {
			if(abs(mat[rn[i], cn[j]]) < 1e-8) {
				next
			}
			rou = get_most_inside_radius()
            sector.index1 = rn[i]
            sector.index2 = cn[j]
			
			if(sector.index1 == sector.index2) {
				circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]]))*0.50000001,
							sector.index2, c(sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])/2, sector.sum.row[ cn[j] ] + abs(mat[rn[i], cn[j]])),
							col = col[rn[i], cn[j]], rou1 = rou, border = link.border[rn[i], cn[j]], lwd = link.lwd[rn[i], cn[j]], 
							lty = link.lty[rn[i], cn[j]], ...)
	            sector.sum.row[ rn[i] ] = sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])
			} else {
				if(directional) {
					if(setequal(direction.type, c("diffHeight", "arrows"))) {
						if(fromRows) {
							if(diffHeight > 0) {
								rou1 = rou - diffHeight
								rou2 = rou
							} else {
								rou1 = rou
								rou2 = rou + diffHeight
							}
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
										sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
										directional = 1, col = col[rn[i], cn[j]], rou1 = rou1, rou2 = rou2, border = link.border[rn[i], cn[j]], 
										lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], 
										arr.length = link.arr.length[rn[i], cn[j]], arr.width = link.arr.width[rn[i], cn[j]],
										arr.type = link.arr.type[rn[i], cn[j]], arr.col = link.arr.col[rn[i], cn[j]],
										arr.lty = link.arr.lty[rn[i], cn[j]], arr.lwd = link.arr.lwd[rn[i], cn[j]],
										...)
						} else {
							if(diffHeight > 0) {
								rou1 = rou
								rou2 = rou - diffHeight
							} else {
								rou1 = rou + diffHeight
								rou2 = rou
							}
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
										sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
										directional = -1, col = col[rn[i], cn[j]], rou1 = rou1, rou2 = rou2, border = link.border[rn[i], cn[j]],
										lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]],
										arr.length = link.arr.length[rn[i], cn[j]], arr.width = link.arr.width[rn[i], cn[j]],
										arr.type = link.arr.type[rn[i], cn[j]], arr.col = link.arr.col[rn[i], cn[j]],
										arr.lty = link.arr.lty[rn[i], cn[j]], arr.lwd = link.arr.lwd[rn[i], cn[j]], ...)
						}
					} else if(setequal(direction.type, "diffHeight")) {
						if(fromRows) {
							if(diffHeight > 0) {
								rou1 = rou - diffHeight
								rou2 = rou
							} else {
								rou1 = rou
								rou2 = rou + diffHeight
							}
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
										sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
										col = col[rn[i], cn[j]], rou1 = rou1, rou2 = rou2, border = link.border[rn[i], cn[j]], 
										lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], ...)
						} else {
							if(diffHeight > 0) {
								rou1 = rou
								rou2 = rou - diffHeight
							} else {
								rou1 = rou + diffHeight
								rou2 = rou
							}
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
									sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
									col = col[rn[i], cn[j]], rou1 = rou1, rou2 = rou2, border = link.border[rn[i], cn[j]],
									lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], ...)
						}
					} else if(setequal(direction.type, "arrows")) {
						if(fromRows) {
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
										sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
										directional = 1, col = col[rn[i], cn[j]], rou1 = rou, border = link.border[rn[i], cn[j]], 
										lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], 
										arr.length = link.arr.length[rn[i], cn[j]], arr.width = link.arr.width[rn[i], cn[j]],
										arr.type = link.arr.type[rn[i], cn[j]], arr.col = link.arr.col[rn[i], cn[j]],
										arr.lty = link.arr.lty[rn[i], cn[j]], arr.lwd = link.arr.lwd[rn[i], cn[j]],
										...)
						} else {
							circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
										sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
										directional = -1, col = col[rn[i], cn[j]], rou1 = rou, border = link.border[rn[i], cn[j]],
										lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]],
										arr.length = link.arr.length[rn[i], cn[j]], arr.width = link.arr.width[rn[i], cn[j]],
										arr.type = link.arr.type[rn[i], cn[j]], arr.col = link.arr.col[rn[i], cn[j]],
										arr.lty = link.arr.lty[rn[i], cn[j]], arr.lwd = link.arr.lwd[rn[i], cn[j]], ...)
						}
					}
				} else {
					circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
								sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
								col = col[rn[i], cn[j]], rou1 = rou, border = link.border[rn[i], cn[j]], lwd = link.lwd[rn[i], cn[j]], 
								lty = link.lty[rn[i], cn[j]], ...)
				}
				
	            sector.sum.row[ rn[i] ] = sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])
				sector.sum.col[ cn[j] ] = sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])
			}
        }
		
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
