
# circlize


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
lines(rotate.parabola(theta1 = 30, theta2 = 90, rou1 = 0.8))
lines(rotate.parabola(theta1 = 50, theta2 = 70, rou1 = 0.8))
lines(rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 0.8, rou.ratio = 0.3))
lines(rotate.parabola(theta1 = 10, theta2 = 300, rou1 = 0.8, rou.ratio = 0.3, n = 5))
