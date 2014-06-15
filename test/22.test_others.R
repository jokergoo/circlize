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
