
op = par(no.readonly = TRUE)

library(circlize)

category = paste0("category", "_", 1:10)
percent = sort(sample(40:80, 10))
color = rev(rainbow(length(percent)))

library(circlize)
par(mar = c(1, 1, 1, 1))
circos.par("start.degree" = 90)
circos.initialize("a", xlim = c(0, 100)) # 'a` just means there is one sector
circos.trackPlotRegion(ylim = c(0.5, length(percent)+0.5), , track.height = 0.8, 
    bg.border = NA, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim") # in fact, it is c(0, 100)
        for(i in seq_along(percent)) {
            circos.lines(xlim, c(i, i), col = "#CCCCCC")
            circos.rect(0, i - 0.45, percent[i], i + 0.45, col = color[i], border = "white")
        }

        for(i in seq_along(percent)) {
            circos.text(xlim[1], i, paste0(category[i], " - ", percent[i], "%"), adj = c(1.1, 0.5)) 
        }

        breaks = seq(0, 90, by = 5)
        circos.axis(h = "top", major.at = breaks, labels = paste0(breaks, "%"),
            major.tick.percentage = 0.02, labels.cex = 0.6, labels.away.percentage = 0.01)
})
circos.clear()

par(op)
