\name{circos.initializeWithIdeogram}
\alias{circos.initializeWithIdeogram}
\title{
  Initialize the circos layout with an ideogram


}
\description{
  Initialize the circos layout with an ideogram


}
\usage{
circos.initializeWithIdeogram(file = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""), track.height = 0.1)
}
\arguments{
  \item{file}{cytoband file. By default it is the cytoband data for human}
  \item{track.height}{height for the track}

}
\details{
  This is not a full functional function. It jus provides a way to show how todraw genomics ideogram by this package. How to embed the ideogram into thecircos layout is really subjective and should be applied according to specific situation.

  In fact, draw ideogram with this package is really simple, you can look at the source codeof this function to get a clue.

  The cytoband data for human is downloaded from UCSC ftp site (\url{http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz),}should be uncompressed.


}
\examples{
\dontrun{
library(circlize)
set.seed(12345)

circos.initializeWithIdeogram()

circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.2,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")

        x1 = xlim[1] + runif(5)*xrange
        x1 = sort(x1)
        x2 = seq(xlim[1], xlim[2], length.out = 5)
        for(i in 1:5) {
            circos.lines(c(x1[i], x2[i]), c(1, 0.5), straight = TRUE)
            circos.text(x2[i], 0.4, labels = "gene", adj = c(0, 0.5), cex = 0.4,
                direction = "vertical_left")
        }
    })


circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, bg.col ="#EEEEEE",
    track.height = 0.1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        for(i in -2:2) {
            circos.lines(xlim, c(i, i)/2, col = "#999999", lwd = 0.2)
        }
        xrange = get.cell.meta.data("xrange")
        x = NULL
        y = NULL
        for(i in 1:5) {
            
            x2 = seq(xlim[1] + (i-1)/5*xrange, xlim[1] + (i)/5*xrange, by = 1000000)
            x = c(x, x2)
            y = c(y, runif(length(x2))^2*sample(c(-1, 1), 1))
        }
        col = ifelse(y > 0, "#E41A1C", "#4DAF4A")
        circos.points(x, y, col = col, cex = 0.2, pch = 16)
    })

circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, track.height = 0.1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        y = runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA,
            col = "#FF7F00")
        y = -runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA,
            col = "#FFFF33")
    })


circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05, 
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        for(i in seq_along(x)) {
            if(i == 1) {
                next
            }
            circos.rect(x[i-1], 0, x[i], 1, col = rgb(runif(1), runif(1), runif(1)),
                border = NA)
        }
    })

chromosome = paste("chr", c(1:22, "X", "Y"), sep = "")
for(i in 1:50) {
    chr = sample(chromosome, 2)
    xlim1 = get.cell.meta.data("xlim", sector.index = chr[1], track.index = 1)
    xrange1 = get.cell.meta.data("xrange", sector.index = chr[1], track.index = 1)
    xlim2 = get.cell.meta.data("xlim", sector.index = chr[2], track.index = 1)
    xrange2 = get.cell.meta.data("xrange", sector.index = chr[2], track.index = 1)
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x1 = c(xlim1[1] + r[1]*xrange1, xlim1[1] + r[2]*xrange1)
    } else {
        x1 = c(xlim1[1] + r[1]*xrange1)
    }
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x2 = c(xlim2[1] + r[1]*xrange2, xlim2[1] + r[2]*xrange2)
    } else {
        x2 = c(xlim2[1] + r[1]*xrange2)
    }
    
    
    circos.link(chr[1], x1, chr[2], x2, col = sample(c('#9E0142', '#D53E4F',
        '#F46D43', '#FDAE61', '#FEE08B', '#FFFFBF', '#E6F598', '#ABDDA4', '#66C2A5',
        '#3288BD', '#5E4FA2'), 1))
}

degree = get.cell.meta.data("xplot", sector.index = "chr1", track.index = 1)
start.degree = degree[1]
end.degree = degree[2]
rou1 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 1)[2]
rou2 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 5)[1]

draw.sector(center = c(0, 0), start.degree = start.degree, end.degree = end.degree,
            rou1 = rou1+0.05, rou2 = rou2-0.01, col = "#FF000020", border = NA)
circos.clear()

#############################################################
# the second example is how to zoom part of chromosomes

library(circlize)
circos.clear()

circos.initializeWithIdeogram()
circos.link("chr1", 12345678, "chr1", 87654321, top.ratio = 0.8)
circos.link("chr1", 22222222, "chr1", 99999999, top.ratio = 0.8)
circos.clear()

d = read.table(file = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""),
    colClasses = c("factor", "numeric", "numeric", "factor", "factor"))

chromosome = c("chr1")
    
xlim = matrix(nrow = 0, ncol = 2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
}
    
circos.clear()

par(mar = c(1, 1, 1, 1), new = TRUE)
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2), clock.wise = FALSE,
    start.degree = -90)
circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
circos.trackPlotRegion(factors = factor(chromosome, levels = chromosome),
    ylim = c(0, 1), bg.border = NA, track.height = 0.2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    col = rep("#FFFFFF", n)
    col[d2[[5]] == "acen"] = "#E41A1C"
    col[d2[[5]] == "stalk"] = "#377EB8"
    col[d2[[5]] == "gvar"] = "#404040"
    col[d2[[5]] == "gpos100"] = "#000000"
    col[d2[[5]] == "gpos"] = "#000000"
    col[d2[[5]] == "gpos75"] = "#BFBFBF"
    col[d2[[5]] == "gpos50"] = "#808080"
    col[d2[[5]] == "gpos25"] = "#404040"
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
    circos.text(cell.xlim[1] + mean(cell.xlim), -0.5, labels = chr,
        sector.index = chr, cex = 0.8)
    circos.link("chr1", 12345678, "chr1", 87654321)
    circos.link("chr1", 22222222, "chr1", 99999999)
}
}
}