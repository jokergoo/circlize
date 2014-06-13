source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")
source("R/link-bezier.R")




par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link2("a", 5, "c", 5, col = "#00000040")
circos.link2("a", 5, "g", 5, col = "#00000040")
circos.link2("c", 10, "d", c(1, 4), col = "#00000040")
circos.link2("a", c(2, 8), "g", c(4, 4.5), height = 0.9, rou1 = 0.9, rou2 = 0.8, col = "#00000040")
circos.link2("b", c(1, 10), "f", c(1, 10), col = "#00000040")

circos.clear()


par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
#circos.par(track.margin = c(0.2, 0))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

#circos.link("a", 5, "c", 5)
circos.link("a", 1, "a", c(2, 8), top.ratio = 0.5, top.ratio.low = 0.5)
circos.link("b", 1, "b", c(2, 8), top.ratio = 0.5)
circos.link("c", 1, "c", c(2, 8), top.ratio = 0.5, top.ratio.low = 0.8)
#circos.link("a", c(2, 3), "e", c(4, 6))

#circos.link("a", c(-10, 10), "f", c(-10, 10))

circos.clear()



par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par(track.margin = c(0, 0.2))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("c", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "e", c(4, 6))

circos.link("a", c(-10, 10), "f", c(-10, 10))

circos.clear()

f_num = function(i) {
	i = as.character(i)
	if(nchar(i) == 1) {
		return(paste("00", i, sep = ""))
	} else if (nchar(i) == 2) {
		return(paste("0", i, sep = ""))
	} else {
		return(i)
	}
}

##################################
for(delta_rou in seq(0, 0.3, by = 0.05)) {
	for(alpha in c(0, 0.01, seq(2, 10, 2))) {
		dir.create(paste("alpha", alpha, "delta_rou", delta_rou, sep = ""))
		i = 1
		cat(paste("alpha:", alpha, ", rou1:", 0.5, ", rou2:", 0.5+delta_rou, sep = ""), "\n")
		d = NULL
		for(theta1 in seq(round(alpha)+2, 180, 1)) {
		cat("  - theta1: ", theta1, "\n")
			for(theta2 in seq(round(alpha)+1, theta1-1, 1)) {
				if(theta1 - theta2 < round(alpha)) next
				d1 = rotate.parabola(0, theta1, rou1 = 1, rou.ratio = 0.5, n = 51)
				d2 = rotate.parabola(alpha, theta2, rou1 = 1, rou.ratio = 0.5+delta_rou, n = 51)
				s = is.lines.intersected2(d1[2:50, 1], d1[2:50, 2], d2[2:50, 1], d2[2:50, 2])
				if(theta1 %% 5 == 0 && theta2 %% 5 == 0) {
					png(file = paste("alpha", alpha, "delta_rou", delta_rou, "/links_", f_num(i), ".png", sep = ""))
					i = i + 1
					par(mar = c(1, 1, 1, 1))
					plot(c(-1.1, 1.1), c(-1.1, 1.1),type = "n", ann = FALSE, axes = FALSE)
					draw.sector()
					
					r1 = arc.points(theta2, theta1, 1, clock.wise = FALSE)
					r2 = arc.points(0, alpha, 1, clock.wise = FALSE)
					
					dx = rbind(d1, r1[rev(seq_len(nrow(r1))), ])
					dx = rbind(dx, d2[rev(seq_len(nrow(d2))), ])
					dx = rbind(dx, r2[rev(seq_len(nrow(r2))), ])
					polygon(dx, col = ifelse(s, "red", "black"), border = ifelse(s, "red", "black"))
					#text(0, 0, paste("theta1=", theta1, "theta2=", theta2))
					text(-1.1, -1.1, paste("delta_rou = ", delta_rou, "\nalpha= ", alpha, "\n", sep = ""), adj = c(0, 0))
					dev.off()
				}
				d = rbind(d, c(theta1, theta2, s))
			}
		}
		png(file = paste("alpha", alpha, "delta_rou", delta_rou, ".png", sep = ""))
		plot(d[,1:2], pch = 15, col = d[, 3], cex = 0.5, xlab="theta1", ylab="theta2", xlim = c(0, 180), ylim = c(0,180))
		lines(c(0, 180), c(0, 180), col = "red")
		title(paste("alpha:", alpha, ", rou1:", 0.5, ", rou2:", 0.5+delta_rou, sep = ""))
		dev.off()
	}
}


mat = matrix(rnorm(100), 10)
mat = cor(mat)
chordDiagram.symmetric(mat)
chordDiagram.symmetric(mat, colFun = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))
chordDiagram.symmetric(mat, grid.col = "grey")
chordDiagram.symmetric(mat, text.direction = "vertical_right", text.adj = c(0, 0.5))

mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
chordDiagram.bicategorical(mat)

