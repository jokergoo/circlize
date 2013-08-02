library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))

circos.clear()


degree.minus = function(to, from, min.zero = TRUE) {
    if(min.zero) {
        return((to - from) %% 360)
    } else {
        if((to - from) %% 360 == 0) {
            return(360)
        } else {
            return((to - from) %% 360)
        }
    }
}

rotate.parabola = function(theta1, theta2, rou1, rou2 = rou1, theta = (theta1+theta2)/2, 
    rou = rou1 * abs(cos(degree.minus(theta1, theta2)/2/180*pi))*rou.ratio, rou.ratio = 0.5,
    n = 1001) {
    
    while(theta2 < theta1) {
        theta2 = theta2 + 360
    }
    
    delta_theta = degree.minus(theta2, theta1)
    
    flag = 0
    if(delta_theta > 180) {
        theta = theta + 180
        flag = 1
    }
    
    # y^2 = kx, y = +-sqrt(kx)
    b = rou1 * abs(sin(degree.minus(theta2, theta1)/2/180*pi))
    a = rou1 * abs(cos(degree.minus(theta2, theta1)/2/180*pi)) - rou
    k = b^2/a
    
    if(n %% 2 == 0) {
        n = n + 1
    }
    n.half = (n - 1) / 2
    x = numeric(n)
    y = numeric(n)
    x = c(n.half:1/n.half, 0, 1:n.half/n.half)*a
    y[1:n.half] = sqrt(k*x[1:n.half])
    y[n.half + 1] = 0
    y[1:n.half + n.half + 1] = -sqrt(k*x[1:n.half + n.half + 1])
    
    alpha = numeric(n)
    
    alpha[1:n.half] = atan(y[1:n.half]/x[1:n.half])*180/pi
    alpha[1:n.half + n.half + 1] = atan(y[1:n.half + n.half + 1]/x[1:n.half + n.half + 1])*180/pi
    alpha[n.half + 1] = 90
    
    d = sqrt(x^2 + y^2)
    x = d*cos((alpha + theta)/180*pi)
    y = d*sin((alpha + theta)/180*pi)
    
    center.x = rou*cos(theta/180*pi)
    center.y = rou*sin(theta/180*pi)
    
    x = x + center.x
    y = y + center.y
    
    if(!flag) {
        x = rev(x)
        y = rev(y)
    }

    return(cbind(x, y))
}

polar2Cartesian = function(d) {
    theta = d[, 1]/360 * 2 *pi
    rou = d[, 2]
    x = rou * cos(theta)
    y = rou * sin(theta)
    return(cbind(x, y))
}
par(mar = c(1, 1, 1, 1))
plot(c(-1, 1), c(-1, 1), axes = FALSE, ann = FALSE ,type = "n")
draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360, rou1 = 1, col = "white", border = "black")
d= rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 1, rou.ratio = 0.5)
lines(rbind(d, d[1, ]))
lines(c(cos(300/180*pi), cos(120/180*pi)), c(sin(300/180*pi), sin(120/180*pi)))
points(0, 0, pch = 16)
lines(c(0, sqrt(3)/4)+0.01, c(0, -3/4)+0.01, lwd = 4, col = "red")
lines(c(0, sqrt(3)/4/2)-0.01, c(0, -3/4/2)-0.01, lwd = 4, col = "blue")


par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0.1, 0))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", c(2, 3), "f", c(4, 6), border = "black")

# the following codes calculate the position for the 'little rectangle'
cell.ylim = get.cell.meta.data("cell.ylim", "a")
d1 = circlize(2, cell.ylim[1], "a")
theta1 = d1[1, 1]
rou1 = d1[1, 2]

d2 = circlize(3, cell.ylim[1], "a")
theta2 = d2[1, 1]
rou2 = d2[1, 2] - circos.par("track.margin")[1]

# draw the 'little rectangle'
draw.sector(start.degree = theta1, end.degree = theta2, rou1 = rou1, rou2 = rou2, col = "black", border = "black")
circos.clear()

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0.1, 0))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", c(2, 3), "f", c(4, 6), border = "black")

# the following codes calculate the position for the 'little rectangle'
cell.ylim = get.cell.meta.data("cell.ylim", "a")
d1 = circlize(2, cell.ylim[1], "a")
theta1 = d1[1, 1]
rou1 = d1[1, 2]

d2 = circlize(3, cell.ylim[1], "a")
theta2 = d2[1, 1]
rou2 = d2[1, 2] - circos.par("track.margin")[1]

# draw the 'little rectangle'
draw.sector(start.degree = theta1, end.degree = theta2, rou1 = rou1, rou2 = rou2, col = "red", border = "red")
circos.clear()
