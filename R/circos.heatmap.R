
# returns a list of named data frames
circos.heatmap.format.input = function(mat, split) {
	if(is.atomic(mat) && !is.matrix(mat)) {
		if(length(mat) == 1) {
			if(!is.null(split)) {
				mat = matrix(rep(mat, length(split)), ncol = 1)
			}
		} else {
			mat = matrix(mat, ncol = 1)
		}
	}
	if(is.data.frame(mat)) mat = as.matrix(mat)

	if(!is.factor(split)) {
		if(is.numeric(split)) split = as.character(split)
		split = factor(split, levels = unique(split))
	}

	subset_list = NULL
	if(is.matrix(mat)) {
		if(is.null(split)) {
			if(is.circos.heatmap.cached()) {
				# qqcat("use the cached split\n")
				env = circos.par("__tempenv__")
				split = env$circos.heatmap.split
			}
		}
		if(is.null(split)) {  # this does not run because split is not NULL in upstream functions
			mat_list = list(mat = mat)
			subset_list = list(mat = 1:nrow(mat))
		} else {
			mat_list = lapply(split(seq_len(nrow(mat)), split), function(ind) mat[ind, , drop = FALSE])
			subset_list = split(seq_len(nrow(mat)), split)

			le = levels(split)
			mat_list = mat_list[le]
			subset_list = subset_list[le]
		}
	} else {
		stop_wrap("Input should be a vector or a matrix.")
	}

	n = length(mat_list)
	if(is.null(names(mat_list))) {
		names(mat_list) = paste0("mat", 1:n)
	}

	attr(mat_list, "subset_list") = subset_list
	mat_list
}

is.circos.heatmap.cached = function() {
	env = circos.par("__tempenv__")
	identical(env$circos.heatmap.initialized, TRUE)
}

# == title
# Initialize circular heatmaps
#
# == param
# -mat A matrix or a vector. The vector is transformed as a one-column matrix.
# -split A categorical variable. It splits the matrix into a list of matrices.
# -cluster whether to apply clustering on rows.
# -clustering.method Clustering method, pass to `stats::hclust`.
# -distance.method Distance method, pass to `stats::dist`.
# -dend.callback A callback function that is applied to the dendrogram in every sector.
# 
circos.heatmap.initialize = function(mat, split = NULL, cluster = TRUE, 
	clustering.method = "complete", distance.method = "euclidean",
	dend.callback = function(dend, m, si) reorder(dend, rowMeans(m))) {

	if(is.null(split)) {
		if(is.matrix(mat)) {
			split = rep("mat", nrow(mat))
		} else {
			split = rep("mat", length(mat))
		}
	}

	if(!is.matrix(mat)) {
		if(is.vector(mat)) {
			mat = matrix(mat, ncol = 1)
		}
	}

	mat_list = circos.heatmap.format.input(mat, split)
	n = length(mat_list)
	subset_list = attr(mat_list, "subset_list")

	cell.padding = c(0, 0, 0, 0)
	track.margin = c(0.02, 0)
	gap.degree = 2

	if(!identical(circos.par("cell.padding"), c(0.02, 1, 0.02, 1))) {
		cell.padding = circos.par("cell.padding")
	}
	if(!identical(circos.par("track.margin"), c(0.01, 0.01))) {
		track.margin = circos.par("track.margin")
	}
	if(!identical(circos.par("gap.degree"), 1)) {
		gap.degree = circos.par("gap.degree")
	}

	circos.par(cell.padding = cell.padding, track.margin = track.margin, gap.degree = gap.degree)

	# qqcat("initialize the circular plot with @{n} matrices.\n")
	circos.initialize(names(mat_list), xlim = cbind(rep(0, n), sapply(mat_list, nrow)))

	if(is.character(mat_list[[1]])) cluster = FALSE

	env = circos.par("__tempenv__")
	if(cluster) {
		# qqcat("perform clustering.\n")
		dend_list = list()
		for(nm in names(mat_list)) {
			m = mat_list[[nm]]
			dend = as.dendrogram(hclust(dist(m, method = distance.method), method = clustering.method))
			dend_list[[nm]] = dend.callback(dend, m, nm)
		}

		for(se in get.all.sector.index()) {
			add.sector.meta.data("row_dend", dend_list[[se]], sector.index = se)
			add.sector.meta.data("dend", dend_list[[se]], sector.index = se)
			add.sector.meta.data("row_order", order.dendrogram(dend_list[[se]]), sector.index = se)
			add.sector.meta.data("order", order.dendrogram(dend_list[[se]]), sector.index = se)
			if(!is.null(subset_list)) {
				add.sector.meta.data("subset", subset_list[[se]], sector.index = se)
			}
		}
		env$circos.heatmap.cluster = TRUE
	} else {
		# qqcat("use the natural order.\n")
		for(se in get.all.sector.index()) {
			add.sector.meta.data("row_order", 1:nrow(mat_list[[se]]), sector.index = se)
			add.sector.meta.data("order", 1:nrow(mat_list[[se]]), sector.index = se)
			if(!is.null(subset_list)) {
				add.sector.meta.data("subset", subset_list[[se]], sector.index = se)
			}
		}
		env$circos.heatmap.cluster = FALSE
	}
	env$circos.heatmap.split = split
	env$circos.heatmap.initialized = TRUE

}

# e.g. to check number of rows, split varaible, ...
circos.heatmap.validate = function(mat_list) {

	# assume the heatmap is already initialized
	env = circos.par("__tempenv__")
	order_list = lapply(env$sector.meta.data, function(x) {
		x$row_order
	})

	if(!identical(unname(sapply(mat_list, nrow)), unname(sapply(order_list, length)))) {
		stop_wrap("The numbers of total rows and in each group are different from the cached values. Maybe you should 1. provide the matrix with the same number of rows as the previous ones, 2. don't set `split` because only the cached one is used, or 3. apply `circos.clear()` if you are making a new plot.")
	}
}

# == title
# Make circular heatmaps
#
# == param
# -mat A matrix or a vector. The vector is transformed as a one-column matrix.
# -split A categorical variable. It splits the matrix into a list of matrices.
# -col If the values in the matrices are continuous, the color should be a color mapping generated by 
#      `colorRamp2`. If the values are characters, the color should be a named color vector.
# -na.col Color for ``NA`` values.
# -bg.border Color for background border.
# -bg.lty Line type of the background border.
# -bg.lwd Line width of the background border.
# -ignore.white Whether to draw the white color?
# -cluster whether to apply clustering on rows.
# -clustering.method Clustering method, pass to `stats::hclust`.
# -distance.method Distance method, pass to `stats::dist`.
# -dend.callback A callback function that is applied to the dendrogram in every sector.
# -dend.side Side of the dendrograms relative to the heatmap track.
# -dend.track.height Track height of the dendrograms.
# -rownames.side Side of the row names relative to the heatmap track.
# -rownames.cex Cex of row names.
# -rownames.font Font of row names.
# -rownames.col Color of row names.
# -show.sector.labels Whether to show sector labels.
# -... Pass to `circos.track` which draws the heatmap track.
# 
# == example
# \donttest{
# set.seed(123)
# mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50), 
#                    matrix(rnorm(50*5, mean = -1), nr = 50)),
#              cbind(matrix(rnorm(50*5, mean = -1), nr = 50), 
#                    matrix(rnorm(50*5, mean = 1), nr = 50))
#             )
# rownames(mat1) = paste0("R", 1:100)
# colnames(mat1) = paste0("C", 1:10)
# mat1 = mat1[sample(100, 100), ] # randomly permute rows
# split = sample(letters[1:5], 100, replace = TRUE)
# spilt = factor(split, levels = letters[1:5])
# col_fun1 = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
# circos.heatmap(mat1, split = split, col = col_fun1)
# circos.clear()
# }
circos.heatmap = function(mat, split = NULL, col, na.col = "grey", 
	bg.border = NA, bg.lty = par("lty"), bg.lwd = par("lwd"), ignore.white = TRUE, 
	cluster = TRUE, clustering.method = "complete", distance.method = "euclidean",
	dend.callback = function(dend, m, si) reorder(dend, rowMeans(m)),
	dend.side = c("none", "outside", "inside"), dend.track.height = 0.1,
	rownames.side = c("none", "outside", "inside"), rownames.cex = 0.5,
	rownames.font = par("font"), rownames.col = "black", 
	show.sector.labels = FALSE, ...) {

	if(!is.matrix(mat)) {
		if(is.vector(mat)) {
			mat = matrix(mat, ncol = 1)
		}
	}

	if(!is.circos.initialized()) {
		cell.padding = c(0, 0, 0, 0)
		track.margin = c(0.02, 0)
		gap.degree = 2

		if(!identical(circos.par("cell.padding"), c(0.02, 1, 0.02, 1))) {
			cell.padding = circos.par("cell.padding")
		}
		if(!identical(circos.par("track.margin"), c(0.01, 0.01))) {
			track.margin = circos.par("track.margin")
		}
		if(!identical(circos.par("gap.degree"), 1)) {
			gap.degree = circos.par("gap.degree")
		}

		circos.par(cell.padding = cell.padding, track.margin = track.margin, gap.degree = gap.degree)

		circos.heatmap.initialize(mat, split = split, cluster = cluster, 
			clustering.method = clustering.method, distance.method = distance.method,
			dend.callback = dend.callback)
	}

	env = circos.par("__tempenv__")
	
	split = env$circos.heatmap.split
	mat_list = circos.heatmap.format.input(mat, split)

	circos.heatmap.validate(mat_list)
	# qqcat("making the heatmap\n")

	dend.side = match.arg(dend.side)
	rownames.side = match.arg(rownames.side)
	if(dend.side == rownames.side && dend.side %in% c("inside", "outside")) {
		stop_wrap("dendrograms and row names cannot be on the same side.")
	}

	if(!env$circos.heatmap.cluster) {
		dend.side = "none"
	}

	if(dend.side == "outside") {
		dend_list = lapply(env$sector.meta.data, function(x) {
			x$row_dend
		})
		max_height = max(sapply(dend_list, function(x) attr(x, "height")))
		circos.track(ylim = c(0, max_height), bg.border = NA, track.height = dend.track.height,
		    panel.fun = function(x, y) {
		        sector.numeric.index = get.cell.meta.data("sector.numeric.index")
		        dend = dend_list[[sector.numeric.index]]
		        circos.dendrogram(dend, max_height = max_height, facing = "inside")
		})
	}

	nr = nrow(mat)
	if(length(rownames.cex) == 1) rownames.cex = rep(rownames.cex, nr)
	if(length(rownames.font) == 1) rownames.font = rep(rownames.font, nr)
	if(length(rownames.col) == 1) rownames.col = rep(rownames.col, nr)

	if(!is.null(rownames(mat))) {
		subset_list = lapply(env$sector.meta.data, function(x) {
			x$subset
		})
		rownames_track_height = max(sapply(subset_list, function(ind) {
			max(strwidth(rownames(mat)[ind], cex = rownames.cex[ind], font = rownames.font[ind]))
		}))
	}
	if(rownames.side == "outside") {
		if(!is.null(rownames(mat))) {
			circos.track(ylim = c(0, 1), bg.border = NA, track.height = rownames_track_height, 
				panel.fun = function(x, y) {
					sector.numeric.index = CELL_META$sector.numeric.index
				    m = mat_list[[sector.numeric.index]]
				    od = CELL_META$row_order
				    nr = nrow(m)
	    
				    if(!is.null(rownames(m))) {
				    	circos.text(1:nr - 0.5, rep(0, nr), rownames(m)[od], cex = rownames.cex[CELL_META$subset][od], 
				    		font = rownames.font[CELL_META$subset][od], col = rownames.col[CELL_META$subset][od],
				    		facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
				    }

				})
		}
	}

	nr = nrow(mat_list[[1]])
	nc = ncol(mat_list[[1]])

	if(missing(col)) {
		stop_wrap("You should provide user-defined colors. If the values are continuous in the matrix, please set a color mapping function generated by `colorRamp2()`. If the values are characters, please set a named color vector.")
	}
	
	if(is.function(col)) {
		col_fun = col
	} else {
		if(is.null(names(col))) {
			stop_wrap('`col` should be a named vector.')
		}
		col_fun = function(x) {
			attr = attributes(x)
			v = col[x]
			v[is.na(v)] = na.col
			attributes(v) = attr
			v
		}
	}

	circos.track(ylim = c(0, nc), bg.border = NA, panel.fun = function(x, y) {

	    sector.numeric.index = CELL_META$sector.numeric.index
	    m = mat_list[[sector.numeric.index]]
	    od = CELL_META$row_order

	    m2 = m[od, , drop = FALSE]
	    col_mat = col_fun(m2)
	    if(!grepl("#\\w\\w\\w\\w\\w\\w", col_mat[1])) {
	    	col_mat_attr = attributes(col_mat)
	    	col_mat = as.vector(col_mat)
	    	col_rgb = col2rgb(col_mat, alpha = TRUE)
	    	col_mat = rgb(col_rgb[1, ], col_rgb[2, ], col_rgb[3, ], alpha = col_rgb[4, ], maxColorValue = 255)
	    	attributes(col_mat) = col_mat_attr
	    }
	    nr = nrow(m2)
	    nc = ncol(m2)

	    for(i in 1:nc) {
	    	if(ignore.white) {
	    		l = grepl("#FFFFFF", col_mat[, i])  # white colors
	    		if(all(l)) {
	    			# no nothing
	    		} else {
	    			# qqcat("@{sum(l)} white rectangles are not drawn.\n")
	    			circos.rect(
	    				(1:nr - 1)[!l], 
	    				(rep(nc - i, nr))[!l], 
			            (1:nr)[!l], 
			            (rep(nc - i + 1, nr))[!l], 
			            border = col_mat[, i][!l], 
			            col = col_mat[, i][!l])
	    		}
	    	} else {
		        circos.rect(1:nr - 1, rep(nc - i, nr), 
		            1:nr, rep(nc - i + 1, nr), 
		            border = col_mat[, i], col = col_mat[, i])
		       }
	    }

	    if(!is.na(bg.border)) {
	    	circos.rect(CELL_META$cell.xlim[1], CELL_META$cell.ylim[1],
	    		        CELL_META$cell.xlim[2], CELL_META$cell.ylim[2],
	    		        border = bg.border, lwd = bg.lwd, lty = bg.lty, col = NA)
	    }

	}, ...)

	
	if(dend.side == "inside") {
		dend_list = lapply(env$sector.meta.data, function(x) {
			x$row_dend
		})
		max_height = max(sapply(dend_list, function(x) attr(x, "height")))
		circos.track(ylim = c(0, max_height), bg.border = NA, track.height = dend.track.height,
		    panel.fun = function(x, y) {
		        sector.numeric.index = get.cell.meta.data("sector.numeric.index")
		        dend = dend_list[[sector.numeric.index]]
		        circos.dendrogram(dend, max_height = max_height, facing = "outside")
		})
	}

	if(rownames.side == "inside") {
		if(!is.null(rownames(mat))) {
			circos.track(ylim = c(0, 1), bg.border = NA, track.height = rownames_track_height, 
				panel.fun = function(x, y) {
					sector.numeric.index = CELL_META$sector.numeric.index
				    m = mat_list[[sector.numeric.index]]
				    od = CELL_META$row_order
				    nr = nrow(m)
	    
				    if(!is.null(rownames(m))) {
				    	circos.text(1:nr - 0.5, rep(1, nr), rownames(m)[od], cex = rownames.cex[CELL_META$subset][od], 
				    		font = rownames.font[CELL_META$subset][od], col = rownames.col[CELL_META$subset][od],
				    		facing = "clockwise", niceFacing = TRUE, adj = c(1, 0.5))
				    }

				})
		}
	}

	if(show.sector.labels) {
		circos.track(track.index = 1, panel.fun = function(x, y) {
			circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + convert_y(2, "mm"), 
				CELL_META$sector.index, facing = "bending.inside", cex = 0.8,
				adj = c(0.5, 0), niceFacing = TRUE)
		}, bg.border = NA)
	}
}

# == title
# Draw a link between two matrix rows in the circular heatmap
#
# == param
# -row_from The row index where the link starts. The value should be length 1. If
#      you want to draw multiple links, put the function in a ``for`` loop.
# -row_to The row index where the link ends.
# -... Pass to `circos.link`.
#
# == example
# \donttest{
# set.seed(123)
# mat = matrix(rnorm(100*10), nrow = 100)
# rownames(mat) = paste0("R", 1:100)
# col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
# circos.heatmap(mat, col = col_fun, rownames.side = "outside")
# circos.heatmap.link(10, 60)
# circos.clear()
#
# split = sample(letters[1:5], 100, replace = TRUE)
# circos.heatmap(mat, col = col_fun, split = split, 
# 	rownames.side = "outside")
# circos.heatmap.link(10, 60)
# circos.clear()
# }
circos.heatmap.link = function(row_from, row_to, ...) {

	if(length(row_from) != 1) {
		stop_wrap("Length of `row_from` should be one.")
	}
	if(length(row_to) != 1) {
		stop_wrap("Length of `row_to` should be one.")
	}

	env = env = circos.par("__tempenv__")
	split = env$circos.heatmap.split
	group1 = split[ row_from ]
	group2 = split[ row_to ]

	if(is.na(group1)) {
		stop_wrap(paste0("Cannot find matrix row with index ", row_from))
	}
	if(is.na(group2)) {
		stop_wrap(paste0("Cannot find matrix row with index ", row_to))
	}

	subset = get.cell.meta.data("subset", sector.index = group1)
	row_order = get.cell.meta.data("row_order", sector.index = group1)
	x1 = which(subset[row_order] == row_from)

	subset = get.cell.meta.data("subset", sector.index = group2)
	row_order = get.cell.meta.data("row_order", sector.index = group2)
	x2 = which(subset[row_order] == row_to)

	circos.link(group1, x1 - 0.5, group2, x2 - 0.5, ...)

}
