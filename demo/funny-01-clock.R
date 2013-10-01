library(circlize)

op = par(no.readonly = FALSE)
factors = letters[1]
par(mar = c(1, 1, 1, 1))

while(1) {
    circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
    circos.initialize(factors = factors, xlim = c(0, 12))
    circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
    circos.axis(sector.index = "a", major.at = 0:12, labels = "", direction = "inside", labels.cex = 1.5, major.tick.percentage = 0.3)
    circos.text(1:12, rep(0.5, 12), 1:12, direction = "horizontal")
     
    current.time = as.POSIXlt(Sys.time())
    sec = ceiling(current.time$sec)
    min = current.time$min
    hour = current.time$hour
     
    sec.degree = 90 - sec/60 * 360
    arrows(0, 0, cos(sec.degree/180*pi)*0.8, sin(sec.degree/180*pi)*0.8)
     
    min.degree = 90 - min/60 * 360
    arrows(0, 0, cos(min.degree/180*pi)*0.7, sin(min.degree/180*pi)*0.7,
             lwd = 2)   
     
    hour.degree = 90 - hour/12 * 360 - min/60 * 360/12
    arrows(0, 0, cos(hour.degree/180*pi)*0.4, sin(hour.degree/180*pi)*0.4, lwd = 2)
     
    circos.clear()
    Sys.sleep(1)
}
par(op)