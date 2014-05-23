
col = c('#E41A1C', '#A73C52', '#6B5F88', '#3780B3', '#3F918C', '#47A266', '#53A651', '#6D8470', '#87638F', '#A5548D', '#C96555', '#ED761C', '#FF9508', '#FFC11A', '#FFEE2C', '#EBDA30', '#CC9F2C', '#AD6428', '#BB614F', '#D77083', '#F37FB8', '#DA88B3', '#B990A6', '#999999')

par(mar = c(1, 1, 1, 1))
circos.initializeWithIdeogram(plotType = NULL)

circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
		chr = get.cell.meta.data("sector.index")
		
		theta = mean(get.cell.meta.data("xplot")) %% 360
		
		if(theta < 90 || theta > 270) {
			direction = "vertical_right"
		} else {
			direction = "vertical_left"
		}
        circos.text(mean(xlim), 0.5, labels = chr, direction = direction, cex = 0.7)
    })

bed = generateRandomBed(nr = 200, fun = function(k) runif(k))
circos.genomicTrackPlotRegion(bed, bg.border = NA, panel.fun = function(region, value, ...) {
	i = get.cell.meta.data("sector.numeric.index")
	circos.genomicLines(region, value, area = TRUE, border = NA, area.baseline = 0, col = col[i])
}, track.height = 0.1)

circos.trackPlotRegion(ylim = c(0, 1), bg.border = col, bg.col = col, panel.fun = function(x, y) {
	chr = get.cell.meta.data("sector.index")
	circos.axis(h = "bottom", labels = NULL, sector.index = chr, direction = "inside")
}, track.height = 0.05)
	
circos.link("chr1", 12345678, "chr1", 87654321, top.ratio = 0.8)
circos.link("chr1", 22222222, "chr1", 99999999, top.ratio = 0.8)

circos.clear()

par(mar = c(1, 1, 1, 1), new = TRUE)
    
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2), clock.wise = FALSE,
    cell.padding = c(0, 0, 0, 0), gap.degree = 180)
circos.initializeWithIdeogram(chromosome.index = "chr1", plotType = c("ideogram", "axis"))

text(0, 0.6, "chr1")

circos.link("chr1", 12345678, "chr1", 87654321)
circos.link("chr1", 22222222, "chr1", 99999999)

circos.clear()       
     