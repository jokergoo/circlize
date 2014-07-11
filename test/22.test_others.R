source("R/genomic_utils.R")
d = read.cytoband()
d = read.cytoband(species = "mm10")
d = read.cytoband(species = "hg")


bed = generateRandomBed(nr = 100000)
chr = bed[[1]]
x = (bed[[2]]+bed[[3]])/2
y = bed[[4]]

circos.initialize(chr, x)
circos.trackPlotRegion(chr, x, y, panel.fun = function(x, y) {
	circos.points(x, y, pch = ".")
})
circos.trackPlotRegion(chr, x, y, panel.fun = function(x, y) {
	lt = circos.approx(x, y, resolution = 0.01)
	circos.points(lt$x, lt$y, pch = ".")
})


f = colorRamp2(c(-2, 0, 1), c("green", "white", "red"))
y = rnorm(100)
plot(1:100, y, pch = 16, col = f(y))

f = colorRamp2(0:5, c("red", "orange", "yellow", "green", "blue", "purple"))
y = runif(100)*5
plot(1:100, y, pch = 16, col = f(y))
