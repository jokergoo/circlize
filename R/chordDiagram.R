
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
#      according to values in ``mat``, or a single value which means colors for all links are the same. You
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
# -symmetric Whether the matrix is symmetric. If the value is set to ``TRUE``, only
#            lower triangular matrix without the diagonal will be used.
# -order Order of sectors. Default order is ``union(rownames(mat), colnames(mat))``.
# -preAllocateTracks Pre-allocate empty tracks before drawing chord diagram. It can be a single number indicating
#                    how many empty tracks needed to be created or a list containing settings for empty
#                    tracks. Please refer to vignette for details.
# -annotationTrack Which annotation track should be plotted? By default, a track containing sector names and a track
#                  containing grid will be created.
# -annotationTrackHeight Track height corresponding to values in ``annotationTrack``.
# -link.border border for links, single scalar or a matrix with names
# -link.lwd width for link borders, single scalar or a matrix with names
# -link.lty style for link borders, single scalar or a matrix with names
# -grid.border border for grids. If it is ``NULL``, the border color is same as grid color
# -diffHeight The difference of height between two 'roots' if ``directional`` is set to ``TRUE``. 
# -reduce if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.
#         Set it to value less than zero if you want to keep all tiny grid.
# -... pass to `circos.link`
#
# == details
# Chord diagram is a way to visualize numeric tables ( http://circos.ca/intro/tabular_visualization/ ), especially useful
# when the table represent information of directional relation. This function
# visualize tables in a circular way.
#
# Sectors of the circos plot is ``union(rownames(mat), colnames(mat))``. If there is no rowname or colname, the function will
# assign names for it ("R1", "R2", ... for row names, "C1", "C2", ... for column names).
#
# This function is flexible and contains some settings that may be a little difficult to understand. 
# Please refer to vignette for better explanation.
chordDiagram = function(mat, grid.col = NULL, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, directional = FALSE, fromRows = TRUE,
	symmetric = FALSE, order = NULL, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
	link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), grid.border = NA, 
	diffHeight = 0.04, reduce = 1e-5, ...) {
	
	if(!is.matrix(mat)) {
		stop("`mat` can only be a matrix.\n")
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
		mat[upper.tri(mat, diag = TRUE)] = 0
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
			if(is.null(names(grid.col)) && length(grid.col) == sum(ncol(mat) + nrow(mat))) {
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

	## make a color matrix based on settings
	if(!is.null(col)) {
		if(is.function(col)) {
			col = col(mat)
		} else if(is.matrix(col)) {

		} else if(length(col) == 1) {
			col = rep(col, length(mat))
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
	
	.normalize_to_mat = function(value, rn, cn, default) {
		var_name = deparse(substitute(value))
		mat = matrix(default, nrow = length(rn), ncol = length(cn))
		rownames(mat) = rn
		colnames(mat) = cn
		if(length(value) == 1) {
			mat[,] = value
		} else {
			if(!is.null(rownames(value)) && !is.null(colnames(value))) {
				common_rn = intersect(rownames(value), rn)
				common_cn = intersect(colnames(value), cn)
				mat[common_rn, common_cn] = value[common_rn, common_cn]
			} else {
				stop(paste0(var_name, " should be a scalar or a matrix with both rownames and colnames.\n"))
			}
		}
		return(mat)
	}
	
	link.border = .normalize_to_mat(link.border, rn, cn, default = NA)
	link.lwd = .normalize_to_mat(link.lwd, rn, cn, default = 1)
	link.lty = .normalize_to_mat(link.lty, rn, cn, default = 1)
	
    sector.sum.row = numeric(length(factors))
    sector.sum.col = numeric(length(factors))
	names(sector.sum.row) = factors
	names(sector.sum.col) = factors
	sector.sum.col[ names(rs) ] = rs
    for(i in seq_along(rn)) {
		# if one name exists in both rows and columns, put it 
		cn_index = rev(seq_along(cn))
		if(rn[i] %in% cn) {
			is = which(cn == rn[i])
			cn_index = c(is, cn_index[cn_index != is])
		}
		
		for(j in cn_index) {
			if(abs(mat[rn[i], cn[j]]) < 1e-8) {
				next
			}
			rou = get_most_inside_radius()
            sector.index1 = rn[i]
            sector.index2 = cn[j]
			
			if(directional) {
				if(fromRows) {
					circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
								sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
								col = col[rn[i], cn[j]], rou1 = rou - diffHeight, rou2 = rou, border = link.border[rn[i], cn[j]], 
								lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], ...)
				} else {
					circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[rn[i], cn[j]])),
							sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[rn[i], cn[j]])),
							col = col[rn[i], cn[j]], rou1 = rou, rou2 = rou - diffHeight, border = link.border[rn[i], cn[j]],
							lwd = link.lwd[rn[i], cn[j]], lty = link.lty[rn[i], cn[j]], ...)
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