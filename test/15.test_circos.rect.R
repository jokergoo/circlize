

x1 = 1
y1 = 1
x2 = 2
y2 = 2

p1 = list(x = x1, y = y1)
p2 = list(x = x2, y = y1)
p3 = list(x = x2, y = y2)
p4 = list(x = x1, y = y2)

center = list(x = c(x1 + x2)/2, y = (y1 + y2)/2)


make_polygon = function(p1, p2, p3, p4) {
	polygon_x = c(p1$x, p2$x, p3$x, p4$x, p1$x)
	polygon_y = c(p1$y, p2$y, p3$y, p4$y, p1$y)

	plot(NULL, xlim = c(-2, 4), ylim = c(-2, 4))

	center = list(x = c(p1$x + p3$x)/2, y = (p1$y + p3$y)/2)

	points(center$x, center$y)
	polygon(polygon_x, polygon_y)
}

make_polygon(p1, p2, p3, p4)


q1 = .rotate(p1$x, p1$y, center$x, center$y, 45/180*pi)
q2 = .rotate(p2$x, p2$y, center$x, center$y, 45/180*pi)
q3 = .rotate(p3$x, p3$y, center$x, center$y, 45/180*pi)
q4 = .rotate(p4$x, p4$y, center$x, center$y, 45/180*pi)

make_polygon(q1, q2, q3, q4)


circos.initialize(fa = c("a", "b", "c", "d"), xlim = c(0, 10))
circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
    for(rot in seq(0, 360, by = 30)) {
        circos.rect(2, 2, 6, 6, rot = rot)
    }
}, track.height = 0.5)

circos.initialize(fa = c("a", "b", "c", "d"), xlim = c(0, 10))
circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
    circos.triangle(c(2, 2), c(2, 8),
                    c(8, 8), c(2, 8), 
                    c(5, 5), c(8, 2))
}, track.height = 0.5)

