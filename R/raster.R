
# if facing  %in% c("bending.inside", "bending.outside"), width and height
# can be numeric values
# else facing should be a string like "2cm"

image = system.file("img", "Rlogo.png", package = "png")

circos.initialize(letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
	circos.raster(image, CELL_META$xcenter, CELL_META$ycenter, width = 1, 
		height = 1, facing = "bending.inside")
})
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
	circos.raster(image, CELL_META$xcenter, CELL_META$ycenter, width = runif(1, 0.5, 1), 
		height = runif(1, 0.5, 1), facing = "bending.inside", niceFacing = TRUE)
})

circos.raster = function(image, x, y, width, height, 
	facing = c("inside", "outside", "reverse.clockwise", "clockwise",
        "downward", "bending", "bending.inside", "bending.outside"),
    niceFacing = FALSE, sector.index = get.cell.meta.data("sector.index"), 
    track.index = get.cell.meta.data("track.index"), 
    resolution = ifelse(grepl("bend", facing), 0.5, 1)) {

	# convert image to Image class (in EBImage package)
	if(inherits(image, "character")) {
		image = readImage(image)
	} else if(!inherits(image, "Image")) {
		image = as.Image(image)
	}
	asp = dim(img)[1]/dim(img)[2]

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
	    width_in_native = pi*as.radian(width_in_degree)*r

	    width_in_px = round(width_in_native/pt_per_inche1*96)
	    height_in_px = round(height_in_native/pt_per_inche1*96)

		# resize according to the size on the image
		image = resize(image, w = width_in_px*resolution, h = height_in_px*resolution)
		image = as.raster(image)

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
		l = !grepl("^#FFFFFF", image)
		circos.rect(xv[l] - width/nc/2, yv[l] - height/nr/2, xv[l] + width/nc/2, yv[l] + height/nr/2,
			col = image[l], border = image[l], sector.index = sector.index, track.index = track.index)

	} else {
		if(missing(width) && missing(height)) {
			stop("at least one of `width` and `height` should be specified")
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

		if(missig(width)) {
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
		image = resize(image, w = width_in_px, h = height_in_px)

		theta = df1[1, 1] - 90
		coor_mat = cbind(c(x0 - width_in_native/2, y0 - height_in_native/2),
			             c(x0 + width_in_native/2, y0 + height_in_native/2))

		points(corr_mat[, 1], corr_mat[, 2], pch = 16, col = "red")

		# since rasterImage() rotate image at the left bottom corner,
		# here we move the image to let it rotate at the center of the image
		rot_mat1 = cbind(c( cos(as.radian(theta)),  sin(as.radian(theta))),
			             c(-sin(as.radian(theta)),  cos(as.radian(theta))))
		rot_mat2 = cbind(c( cos(as.radian(-theta)), sin(as.radian(-theta))),
			             c(-sin(as.radian(-theta)), cos(as.radian(-theta))))
		coor_mat = coor_mat %*% rot_mat1
		corr_mat[, 1] = corr_mat[, 1] - width_in_native/2
		corr_mat[, 2] = corr_mat[, 2] - height_in_native/2
		corr_mat = corr_mat %*% rot_mat2
		rasterImage(image, corr_mat[1, 1], corr_mat[1, 2], corr_mat[2, 1], corr_mat[2, 2], angle = theta)
		points(corr_mat[, 1], corr_mat[, 2], pch = 16, col = "red")
	}
}


# a function convert unit in data coordinate to absolute unit