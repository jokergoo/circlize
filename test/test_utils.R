
source("R/global.R")
source("R/plot.R")
source("R/utils.R")

# polar2Cartesian
d = polar2Cartesian(cbind(seq(0, 360, by = 30), rep(1, 13)))
plot(d)

# lines.expand


# recycle.with.factors
factors = factor(sample(letters[1:5], 20, replace = TRUE)) 
recycle.with.factors(1, factors)
recycle.with.factors(1:5, factors)
recycle.with.factors(1:10, factors)
recycle.with.factors(NULL, factors)

# recycle.with.levels
levels = letters[1:5]
recycle.with.levels(1, levels)
recycle.with.levels(1:5, levels)
recycle.with.levels(1:3, levels)
recycle.with.levels(NULL, levels)

# check.track.position

# check.points.position

# get.sector.numeric.index

# test rotate.parabola
plot(c(-1, 1), c(-1, 1))
d = rotate.parabola(theta1 = 30, theta2 = 90, rou1 = 0.8)
lines(d); text(d[1, 1], d[1, 2], "start_30")
d = rotate.parabola(theta1 = 50, theta2 = 70, rou1 = 0.8)
lines(d); text(d[1, 1], d[1, 2], "start_50")
d = rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 0.8, rou.ratio = 0.3)
lines(d); text(d[1, 1], d[1, 2], "start_270")
d = rotate.parabola(theta1 = 10, theta2 = 300, rou1 = 0.8, rou.ratio = 0.3, n = 5)
lines(d); text(d[1, 1], d[1, 2], "start_10")
d = rotate.parabola(theta1 = 240, theta2 = 360, rou1 = 0.8)
lines(d); text(d[1, 1], d[1, 2], "start_240")
d = rotate.parabola(theta1 = 290, theta2 = 40, rou1 = 0.8)
lines(d); text(d[1, 1], d[1, 2], "start_290")
d = rotate.parabola(theta1 = 180, theta2 = 150, rou1 = 0.8)
lines(d); text(d[1, 1], d[1, 2], "start_180")

is.points.ordered.on.circle(c(30, 50, 70, 90))    # TRUE
is.points.ordered.on.circle(c(90, 70, 50, 30))    # FALSE
is.points.ordered.on.circle(c(90, 70, 50, 30), clock.wise = TRUE)    # TRUE
is.points.ordered.on.circle(c(150, 50, 70, 180))  # FALSE 
is.points.ordered.on.circle(c(150, 30, 10, 330))  # FALSE
is.points.ordered.on.circle(c(270, 330, 10, 120)) # TRUE
is.points.ordered.on.circle(c(120, 10, 330, 270), clock.wise = TRUE) # TRUE
is.points.ordered.on.circle(c(120, 10, 330, 270)) # FALSE
is.points.ordered.on.circle(c(240, 280, 330, 360)) # TRUE
is.points.ordered.on.circle(c(360, 240, 280, 330)) # TRUE

degree.add(30, 60)     # 90
degree.add(60, 30)     # 90
degree.add(200, 300)   # 140
degree.add(300, 200)   # 140
degree.minus(50, 20)   # 30
degree.minus(20, 50)   # 330
degree.minus(30, 330)  # 60
degree.minus(330, 30)  # 300

