
###############################
#  test draw.sector
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5, col = "red", border = "black")
draw.sector(c(0, 0), start = 70, end = 180, rou1 = 0.8, col = "blue", border = NA)
draw.sector(c(0, 0), start = 220, end = 270, rou1 = 0.4, rou2 = 0.6, col = "yellow", border = "black")

draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3, col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2, col = "green", border = "black")
