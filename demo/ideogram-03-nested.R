op = par(no.readonly = FALSE)
cytoband = read.cytoband()
d = cytoband$df
chromosome = cytoband$chromosome
xlim = cbind(rep(0, length(chromosome)), cytoband$chr.len)

col = c('#E41A1C', '#A73C52', '#6B5F88', '#3780B3', '#3F918C', '#47A266', '#53A651', '#6D8470', '#87638F', '#A5548D', '#C96555', '#ED761C', '#FF9508', '#FFC11A', '#FFEE2C', '#EBDA30', '#CC9F2C', '#AD6428', '#BB614F', '#D77083', '#F37FB8', '#DA88B3', '#B990A6', '#999999')


par(mar = c(1, 1, 1, 1), lwd = 0.5)
circos.par("cell.padding" = c(0, 0, 0, 0), "canvas.xlim" = c(-1.1, 1.1), "canvas.ylim" = c(-1.1, 1.1))
circos.initialize(factors = factor(chromosome, levels = chromosome),
    xlim = xlim)

circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
		chr = get.cell.meta.data("sector.index")
		
		 # chromosome names, only the number part or the letter part
		theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
		
		if(theta < 90 || theta > 270) {
			direction = "vertical_right"
			adj = c(0, 0.5)
		} else {
			direction = "vertical_left"
			adj = c(1, 0.5)
		}
        circos.text(mean(xlim), 0.5, labels = chr, direction = direction, cex = 1, font=2, adj = adj)
    })
	
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
		i = get.cell.meta.data("sector.numeric.index")
        x = seq(xlim[1], xlim[2], by = 10000000)
        y = runif(length(x))
        circos.lines(x, y, area = TRUE, baseline = 0, col=col[i], border = NA)
    })
	
circos.trackPlotRegion(factors = chromosome, ylim = c(0, 1),
    bg.border = NA, track.height = 0.1, panel.fun = function(x, y) {
	chr.xlim = get.cell.meta.data("xlim")
	chr = get.cell.meta.data("sector.index")
	i = get.cell.meta.data("sector.numeric.index")
	
	d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    
    # rectangle that cover the whole chromosome
    circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, col = col[i], border = NA)
	
	major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 50000000)
    circos.axis(h = "bottom", major.at = major.at, 
        labels = NULL, sector.index = chr, direction = "inside")
    
})

circos.link("chr1", 12345678, "chr1", 87654321, top.ratio = 0.8)
circos.link("chr1", 22222222, "chr1", 99999999, top.ratio = 0.8)


circos.clear()

d = read.table(file = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""),
    colClasses = c("character", "numeric", "numeric", "character", "character"))

chromosome = c("chr1")
    
xlim = matrix(nrow = 0, ncol = 2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
}
    
par(mar = c(1, 1, 1, 1), new = TRUE)
    
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2), clock.wise = FALSE,
    cell.padding = c(0, 0, 0, 0), gap.degree = 180)
circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
circos.trackPlotRegion(factors = factor(chromosome, levels = chromosome),
    ylim = c(0, 1), bg.border = NA, track.height = 0.2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    col = cytoband.col(d2[[5]])
    for(i in seq_len(n)) {
        circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
            col = col[i], border = NA)
    }
    circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
    major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 10000000)
    circos.axis(h = 0.5, major.at = major.at, 
        labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, 
        labels.cex = 0.4, labels.direction = "vertical_left")
    cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
    circos.text(cell.xlim[1] + mean(cell.xlim), -1, labels = chr, 
        sector.index = chr, cex = 1)
    circos.link("chr1", 12345678, "chr1", 87654321)
    circos.link("chr1", 22222222, "chr1", 99999999)
}
circos.clear()       
par(op)