cytoband = read.cytoband()
d = cytoband$df
chromosome = cytoband$chromosome
chr.len = cytoband$chr.len

d.zoom = d[d$V1 %in% c("chr7", "chr8"), ]
d.zoom$V1[d.zoom$V1 == "chr7"] = "chr7_zoom"
d.zoom$V1[d.zoom$V1 == "chr8"] = "chr8_zoom"

d = rbind(d, d.zoom)
xlim = cbind(rep(0, 26), c(chr.len, chr.len[7:8]))
chromosome = c(chromosome, c("chr7_zoom", "chr8_zoom"))

par(mar = c(1, 1, 1, 1), lwd = 0.5)
circos.par("cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
sector.width = c( chr.len/sum(chr.len), chr.len[7:8]/sum(chr.len[7:8]))
circos.initialize(factors = factor(chromosome, levels = chromosome), xlim = xlim, sector.width = sector.width)
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    col = cytoband.col(d2[[5]])
    for(i in seq_len(n)) {
        circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
            col = col[i], border = NA)
    }
    circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
    major.at = seq(0, 10^nchar(max(xlim[, 2])), by = ifelse(grepl("zoom", chr), 10000000,100000000))
    circos.axis(h = 0.5, major.at = major.at, 
        labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, 
        labels.cex = 0.4, labels.direction = "vertical_left")
    cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
    circos.text(cell.xlim[1] + mean(cell.xlim), 1.2, labels = chr, 
        sector.index = chr, cex = 0.7, adj = c(0.5, 0))
}

circos.link("chr7", c(0, chr.len[7]), "chr7_zoom", c(0, chr.len[7]), col = "#0000FF10", border = NA)
circos.link("chr8", c(0, chr.len[8]), "chr8_zoom", c(0, chr.len[8]), col = "#FF000010", border = NA)
circos.clear()       
     