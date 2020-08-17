
# == title
# Draw arrow which is paralle to the circle
#
# == param
# -x1 Start position of the arrow on the x-axis.
# -x2 End position of the arrow on the x-axis. Note ``x2`` should be larger than ``x1``. The direction
#     of arrows can be controlled by ``arrow.position`` argument.
# -y Position of the arrow on the y-axis. Note this is the center of the arrow on y-axis.
# -width Width of the arrow body.
# -sector.index Index of the sector.
# -track.index Index of the track.
# -arrow.head.length Length of the arrow head. Note the value should be smaller than the length of the arrow itself (which is ``x2 - x1``).
# -arrow.head.width Width of the arrow head.
# -arrow.position Where is the arrow head on the arrow. If you want to the arrow in the reversed direction, set this value to ``"start"``.
# -tail The shape of the arrow tail (the opposite side of arrow head).
# -border Border color of the arrow.
# -col Filled color of the arrow.
# -lty Line style of the arrow.
# -... Pass to `graphics::polygon`.
#
# == details
# Note all position values are measured in the data coordinate (the coordinate in each cell). For the values of
# ``width``, ``arrow.head.Length``, ``arrow.head.width``, they can be set with `mm_y`/`cm_y`/`inches_y` in absolute units.
# 
# If you see points overflow warnings, you can set ``circos.par(points.overflow.warning = FALSE)`` to turn it off.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/graphics.html#circular-arrows
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# op = par(no.readonly = TRUE)
# par(mfrow = c(1, 2))
# circos.initialize(letters[1:4], xlim = c(0, 1))
# col = rand_color(4)
# tail = c("point", "normal", "point", "normal")
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.arrow(x1 = 0, x2 = 1, y = 0.5, width = 0.4, 
#         arrow.head.width = 0.6, arrow.head.length = cm_x(1), 
#         col = col[CELL_META$sector.numeric.index], 
#         tail = tail[CELL_META$sector.numeric.index])
# }, bg.border = NA, track.height = 0.4)
# circos.clear()
#
# circos.initialize(letters[1:4], xlim = c(0, 1))
# tail = c("point", "normal", "point", "normal")
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.arrow(x1 = 0, x2 = 1, y = 0.5, width = 0.4, 
#         arrow.head.width = 0.6, arrow.head.length = cm_x(1), 
#         col = col[CELL_META$sector.numeric.index], 
#         tail = tail[CELL_META$sector.numeric.index],
#         arrow.position = "start")
# }, bg.border = NA, track.height = 0.4)
# par(op)
#
# ########## cell cycle ###########
# cell_cycle = data.frame(phase = factor(c("G1", "S", "G2", "M"), 
#                                     levels = c("G1", "S", "G2", "M")),
#                         hour = c(11, 8, 4, 1))
# color = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")
# circos.par(start.degree = 90)
# circos.initialize(cell_cycle$phase, xlim = cbind(rep(0, 4), cell_cycle$hour))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#   circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2], 
#       arrow.head.width = CELL_META$yrange*0.8, arrow.head.length = cm_x(1),
#       col = color[CELL_META$sector.numeric.index])
#   circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, 
#       facing = "downward")
# }, bg.border = NA, track.height = 0.3)
# circos.clear()
#
circos.arrow = function(
	x1, 
	x2, 
	y = get.cell.meta.data("ycenter"), 
	width = get.cell.meta.data("yrange")/2, 
	sector.index = get.current.sector.index(), 
	track.index = get.current.track.index(),
	arrow.head.length = mm_x(5),
	arrow.head.width = width*2, 
	arrow.position = c("end", "start"),
	tail = c("normal", "point"), 
	border = "black", 
	col = "white", 
	lty = par("lty"), 
	...) {

	arrow.position = match.arg(arrow.position)[1]
	tail = match.arg(tail)[1]

	set.current.cell(sector.index, track.index)

	if(x2 <= x1) {
		# stop_wrap("`x2` should be larger than `x1`. Set `arrow.position = 'start'` to get reverse clockwise arrows.")
		x3 = x1
		x1 = x2
		x2 = x3
		arrow.position = setdiff(c("end", "start"), arrow.position)
	}
	
	if(abs(x2 - x1 - arrow.head.length) < 1e-6) {
		stop_wrap("Arrow head is too long that it is even longer than the arrow itself.")
	}

	if(arrow.position == "end") {
		arrow.head.coor = rbind(c(x2 - arrow.head.length, y + arrow.head.width/2),
			                    c(x2, y),
			                    c(x2 - arrow.head.length, y - arrow.head.width/2))
		if(tail == "normal") {
			arrow.body.coor = rbind(c(x2 - arrow.head.length, y - width/2),
				                    c(x1, y - width/2),
				                    c(x1, y + width/2),
				                    c(x2 - arrow.head.length, y + width/2))
			arrow.body.coor2 = rbind(lines.expand(arrow.body.coor[1:2, 1], arrow.body.coor[1:2, 2], sector.index, track.index),
				                    lines.expand(arrow.body.coor[3:4, 1], arrow.body.coor[3:4, 2], sector.index, track.index))
		} else {
			arrow.body.coor = rbind(c(x2 - arrow.head.length, y - width/2),
				                    c(x1, y),
				                    c(x2 - arrow.head.length, y + width/2))
			arrow.body.coor2 = rbind(lines.expand(arrow.body.coor[1:2, 1], arrow.body.coor[1:2, 2], sector.index, track.index),
				                    lines.expand(arrow.body.coor[2:3, 1], arrow.body.coor[2:3, 2], sector.index, track.index))
		}

		coor = rbind(arrow.body.coor2, arrow.head.coor)
	} else {
		
		arrow.head.coor = rbind(c(x1 + arrow.head.length, y + arrow.head.width/2),
			                    c(x1, y),
			                    c(x1 + arrow.head.length, y - arrow.head.width/2))
		if(tail == "normal") {
			arrow.body.coor = rbind(c(x1 + arrow.head.length, y - width/2),
				                    c(x2, y - width/2),
				                    c(x2, y + width/2),
				                    c(x1 + arrow.head.length, y + width/2))
			arrow.body.coor2 = rbind(lines.expand(arrow.body.coor[1:2, 1], arrow.body.coor[1:2, 2], sector.index, track.index),
				                    lines.expand(arrow.body.coor[3:4, 1], arrow.body.coor[3:4, 2], sector.index, track.index))
		} else {
			arrow.body.coor = rbind(c(x1 + arrow.head.length, y - width/2),
				                    c(x2, y),
				                    c(x1 + arrow.head.length, y + width/2))
			arrow.body.coor2 = rbind(lines.expand(arrow.body.coor[1:2, 1], arrow.body.coor[1:2, 2], sector.index, track.index),
				                    lines.expand(arrow.body.coor[2:3, 1], arrow.body.coor[2:3, 2], sector.index, track.index))
		}
		coor = rbind(arrow.body.coor2, arrow.head.coor)
	}
	coor = rbind(coor, coor[1, ])
	
	d2 = circlize(coor[, 1], coor[, 2], sector.index, track.index)
	polygon(polar2Cartesian(d2), border = border, col = col, lty = lty, ...)
}
