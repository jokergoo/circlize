
# == title
# Add raster images
#
# == param
# -image a ``raster`` object, or an object that can be converted by `grDevices::as.raster`
# -x position of the center of the raster image, measued in the data coordinate in the cell
# -y position of the center of the raster image, measued in the data coordinate in the cell
# -width width of the raster image. When ``facing`` is one of "inside", "outside", "clockwise"
#        and "reverse.clockwise", the image should have absolute size where the value of ``width``
#        should be specified like ``20mm``, ``1cm`` or ``0.5inche``. When ``facing`` is one of
#        ``bending.inside`` and ``bending.outside``, the value of ``width`` is measured in the data
#         coordinate in the cell.
# -height height of the raster image. Same format as ``width``. If the value of ``height`` is omit, 
#         default height is calculated by taking the aspect ratio of the original image. But when
#         ``facing`` is one of ``bending.inside`` and ``bending.outside``, ``height`` is mandatory to set.
# -facing facing of the raster image
# -niceFacing facing of text. Please refer to vignette for different settings
# -sector.index index for the sector
# -track.index index for the track
# -scaling scaling factor to resize the raster image.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# require(png)
# image = system.file("extdata", "Rlogo.png", package = "circlize")
# image = as.raster(readPNG(image))
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.raster(image, CELL_META$xcenter, CELL_META$ycenter, width = "2cm", 
#         facing = "inside", niceFacing = TRUE)
# })
# circos.clear()
#
# \dontrun{
# # NOTE: following takes quite a long time to run
# load(system.file("extdata", "doodle.RData", package = "circlize"))
# circos.par("cell.padding" = c(0, 0, 0, 0))
# circos.initialize(letters[1:16], xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     img = img_list[[CELL_META$sector.numeric.index]]
#     circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, width = 1, 
#         height = 1, facing = "bending.inside")
# }, track.height = 0.25, bg.border = NA)
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     img = img_list[[CELL_META$sector.numeric.index + 16]]
#     circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, width = 1, 
#         height = 1, facing = "bending.inside")
# }, track.height = 0.25, bg.border = NA)
# circos.clear()
# }
circos.raster = function(image, x, y, width, height, 
	facing = c("inside", "outside", "reverse.clockwise", "clockwise",
        "downward", "bending.inside", "bending.outside"),
    niceFacing = FALSE, sector.index = get.cell.meta.data("sector.index"), 
    track.index = get.cell.meta.data("track.index"), 
    scaling = 1) {

	# convert image to raster class
	if(!inherits(image, "raster")) {
		image = as.raster(image)
	}
	asp = dim(image)[1]/dim(image)[2]

	facing = match.arg(facing)
	if(facing %in% c("bending.inside", "bending.outside")) {

		if(niceFacing) {
			degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
			degree = degree %% 360
			if(degree > 180 & degree < 360) {
				if(facing == "bending.inside") {
					facing = "bending.outside"
				} else {
					facing = "bending.inside"
				}
			}
		} 

		pin = par("pin")
		usr = par("usr")

	    pt_per_inche1 = (usr[2] - usr[1])/pin[1]

	    xplot = get.cell.meta.data("xplot", sector.index = sector.index, track.index = track.index)
	    cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
	    width_in_degree = width/(cell.xlim[2] - cell.xlim[1]) * ((xplot[1] - xplot[2]) %% 360)
	    yplot = get.cell.meta.data("yplot", sector.index = sector.index, track.index = track.index)
	    cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sector.index, track.index = track.index)
	    height_in_native = height/(cell.ylim[2] - cell.ylim[1]) * (yplot[2] - yplot[1])

	    r = yplot[1] + (y - cell.ylim[1])/(cell.ylim[2] - cell.ylim[1]) * (yplot[2] - yplot[1])
	    width_in_native = as.radian(width_in_degree)*r

	    width_in_px = round(width_in_native/pt_per_inche1*96)
	    height_in_px = round(height_in_native/pt_per_inche1*96)

		# resize according to the size on the image
		image = resize_image(image, w = ncol(image)*scaling, h = nrow(image)*scaling)

		# make circular rectangles
		xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index)
		ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)

		nr = nrow(image)
		nc = ncol(image)

		row_index = rep(1:nr, nc)
		col_index = rep(1:nc, each = nr)

		if(facing == "bending.inside") {
			yv = 1 - (row_index - 0.5)/nr
			xv = (col_index - 0.5)/nc
		} else {
			yv = (row_index - 0.5)/nr
			xv = 1 - (col_index - 0.5)/nc
		}
		xv = x - width/2 + xv*width
		yv = y - height/2 + yv*height
		l = !grepl("^#FFFFFF00", image)
		op = circos.par("points.overflow.warning")
		circos.par(points.overflow.warning = FALSE)
		circos.rect(xv[l] - width/nc/2, yv[l] - height/nr/2, xv[l] + width/nc/2, yv[l] + height/nr/2,
			col = image[l], border = image[l], sector.index = sector.index, track.index = track.index)
		circos.par(points.overflow.warning = op)
	} else {
		if(missing(width) && missing(height)) {
			stop_wrap("at least one of `width` and `height` should be specified")
		}
		if(!missing(width)) {
			width_lt = parse_unit(width)
			width = width_lt$value
			width_unit = width_lt$unit
			width_in_px = switch(width_unit, 
				mm = width*3.7795275591,
				cm = width*37.795275591,
				inche = width*96
			)
			width_in_px = round(width_in_px)
			width_in_native = convert_length(width, width_unit)
		}
		if(!missing(height)) {
			height_lt = parse_unit(height)
			height = height_lt$value
			height_unit = height_lt$unit
			height_in_px = switch(width_unit, 
				mm = height*3.7795275591,
				cm = height*37.795275591,
				inche = height*96
			)
			height_in_px = round(height_in_px)
			height_in_native = convert_length(height, height_unit)
		}

		if(missing(width)) {
			width_in_px = round(height_in_px*asp)
			width_in_native = width_in_native*asp
		}
		if(missing(height)) {
			height_in_px = round(width_in_px/asp)
			height_in_native = width_in_native*asp
		}
			
		df1 = circlize(x, y, sector.index = sector.index, track.index = track.index)
		df2 = polar2Cartesian(df1)
		x0 = df2[1, 1]
		y0 = df2[1, 2]

		# resize accoring to the width and height
		image = resize_image(image, w = ncol(image)*scaling, h = nrow(image)*scaling)

		coor_mat = cbind(c(x0 - width_in_native/2, y0 - height_in_native/2),
			             c(x0 - width_in_native/2, y0 + height_in_native/2),
			             c(x0 + width_in_native/2, y0 + height_in_native/2),
			             c(x0 + width_in_native/2, y0 - height_in_native/2))

		# polygon(coor_mat[1, ], coor_mat[2, ], border = "red")
		if(facing %in% c("downward")) {
			rasterImage(image, coor_mat[1, 1], coor_mat[2, 1], coor_mat[1, 3], coor_mat[2, 3])
		} else {

			if(niceFacing) {
				degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
				degree = degree %% 360
				if(facing %in% c("inside", "outside")) {
					if(degree > 180 & degree < 360) {
						if(facing == "inside") {
							facing = "outside"
						} else {
							facing = "inside"
						}
					}
				} else {
					if(facing == "clockwise") {
						if(degree > 90 & degree < 270) {
							facing = "reverse.clockwise"
						}
					} else  if(facing == "reverse.clockwise") { 
						if(degree < 90 | degree > 270) {
							facing = "clockwise"
						}
					}
				}
			} 

			# rotate the rectangle, and find the coordiante of the left bottom angle and top right angle
			if(facing == "inside") {
				theta = df1[1, 1] - 90
			} else if(facing == "outside") {
				theta = df1[1, 1] - 90 + 180
			} else if(facing == "clockwise") {
				theta = df1[1, 1]
			} else if(facing == "reverse.clockwise") {
				theta = df1[1, 1] + 180
			}
			offset_x = mean(coor_mat[1, ])
			offset_y = mean(coor_mat[2, ])
			coor_mat[1, ] = coor_mat[1, ] - offset_x
			coor_mat[2, ] = coor_mat[2, ] - offset_y
			# polygon(coor_mat[1, ], coor_mat[2, ], border = "blue")
			rot_mat = cbind(c( cos(as.radian(theta)),  sin(as.radian(theta))),
				            c(-sin(as.radian(theta)),  cos(as.radian(theta))))
			coor_mat = rot_mat %*% coor_mat
			coor_mat[1, ] = coor_mat[1, ] + offset_x
			coor_mat[2, ] = coor_mat[2, ] + offset_y
			# polygon(coor_mat[1, ], coor_mat[2, ], border = "green")

			rasterImage(image, coor_mat[1, 1], coor_mat[2, 1], coor_mat[1, 1] + width_in_native, coor_mat[2, 1] + height_in_native,
				angle = theta)
			# points(coor_mat[1, 1], coor_mat[2, 1], pch = 16, col = "red")
		}
	}
}


resize_image = function(m, w, h = round(w*(ncol(m)/nrow(m)))) {
	
	w0 = nrow(m)
	h0 = ncol(m)

	w_ratio = w0/w
	h_ratio = h0/h

 	# Do resizing -- select appropriate indices
 	if(length(dim(m)) == 2) {
 		out = m[ ceiling(w_ratio* 1:w), ceiling(h_ratio* 1:h)]
 	} else {
 		out = m[ ceiling(w_ratio* 1:w), ceiling(h_ratio* 1:h), , drop = FALSE]
 	}
 	return(out)
}

