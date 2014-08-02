library(circlize)

set.seed(12345)
rand_color = function() {
    return(rgb(runif(1), runif(1), runif(1)))
}

op = par(no.readonly = TRUE)

layout(matrix(1:9, 3, 3))
for(i in 1:9) {
    factors = 1:8
    par(mar = c(0.5, 0.5, 0.5, 0.5))
    circos.par(cell.padding = c(0, 0, 0, 0))
    circos.initialize(factors, xlim = c(0, 1))
    circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.05,
        bg.col = sapply(1:8, function(x) rand_color()),
        bg.border = NA)
    for(i in 1:20) {
        se = sample(1:8, 2)
        col = rand_color()
        col = paste(col, "40", sep = "")
        circos.link(se[1], runif(2), se[2], runif(2), col = col)
    }
    circos.clear()
}

par(op)
layout(1)
