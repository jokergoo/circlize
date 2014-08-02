
op = par(no.readonly = TRUE)

par(xpd = NA, mar = c(1, 1, 1, 1))
plot(c(0,0.9), c(0.3,1.1), type = "n", axes = FALSE, ann = FALSE)
text(0.4, 1, "circos.trackPlotRegion", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.9, "circos.points", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.8, "circos.lines", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.7, "circos.text", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.6, "circos.rect", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.5, "circos.polygon", family = "mono", adj = c(1, 0.5), font = 1)
text(0.4, 0.4, "circos.axis", family = "mono", adj = c(1, 0.5), font = 1)

text(0.6, 1, "plot.default", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.9, "points", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.8, "lines", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.7, "text", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.6, "rect", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.5, "polygon", family = "mono", adj = c(0, 0.5), font = 1)
text(0.6, 0.4, "axis", family = "mono", adj = c(0, 0.5), font = 1)

arrows(rep(0.45, 7), 10:4/10, rep(0.55, 7), 10:4/10, length = 0.15, angle = 15)

par(op)
