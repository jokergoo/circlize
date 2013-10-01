color.pal = function(x, col = c("green", "black", "red"), breaks = c(-5, 0, 5)) {
    
    if(length(col) != length(breaks)) {
		stop("Length of col should be equal to the length of breaks.\n")
	}
	
    # change col represented as strings to RGB space
    col_section = sapply(col, function(x) as.vector(col2rgb(x)))
    col_section = t(col_section)
    
    x[x >= max(breaks)] = max(breaks)
    x[x <= min(breaks)] = min(breaks)
    
    color = character(length(x))
    for(i in 1:length(x)) {
        # NA values, grey color
        if(!is.numeric(x[i])) {
            color[i] = rgb(128, 128, 128, maxColorValue = 255)
            next
        }
        value = x[i]
        
        # find which interval the value belongs to 
        interval = which(breaks >= x[i])[1]
        if(length(interval) == 0) {
            interval = length(interval)
        }
        if(interval == 1) {
            interval = 2
        }
        
        # linear interpolation
        col_num = (value - breaks[interval])*(col_section[interval, ] - col_section[interval - 1, ]) / (breaks[interval] - breaks[interval - 1]) + col_section[interval, ]
        
        col_num = ifelse(col_num > 255, 255, col_num)
        col_num = ifelse(col_num < 0, 0, col_num)
        
        color[i] = rgb(col_num[1], col_num[2], col_num[3], maxColorValue = 255)
    }
    
    return(color)
}
op = par(no.readonly = FALSE)
par(mfrow = c(1, 2))

set.seed(12345)
n = 9
m = matrix(rnorm(n^2), n, n)
colnames(m) = letters[1:n]
m2 = cor(m)
xlim = cbind(rep(0, n), apply(m2, 2, function(x) sum(abs(x)) - 1))

library(circlize)
factors = rownames(m2)
colors = 1:n

############################################
# first figure
par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        current.sector.index = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), 0.75, labels = current.sector.index,
            direction = "horizontal")
        i = get.cell.meta.data("sector.numeric.index")
        circos.rect(min(xlim), 0, max(xlim), 0.25, col = colors[i])
    })

	
rn = rownames(m2)
sector.sum = numeric(length(rn))
for(i in 2:n) {
    for(j in 1:(i-1)) {
        sector.index1 = rn[i]
        sector.index2 = rn[j]
        circos.link(sector.index1, c(sector.sum[i],sector.sum[i] + abs(m2[i, j])),
            sector.index2, c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
            col = ifelse(m2[i, j] > 0, "#FF0000A0", "#00FF00A0"), border = "grey")
        sector.sum[i] = sector.sum[i] + abs(m2[i, j])
        sector.sum[j] = sector.sum[j] + abs(m2[i, j])
    }
}

circos.clear()


#############################################
# second figure
colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    current.sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0.75, labels = current.sector.index, direction = "horizontal")
    i = get.cell.meta.data("sector.numeric.index")
    circos.rect(min(xlim), 0, max(xlim), 0.25, col = colors[i])
})

rn = rownames(m2)
sector.sum = numeric(length(rn))
for(i in 2:n) {
    for(j in 1:(i-1)) {
        sector.index1 = rn[i]
        sector.index2 = rn[j]
        circos.link(sector.index1, c(sector.sum[i], sector.sum[i] + abs(m2[i, j])),
                    sector.index2, c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
                    col = color.pal(m2[i, j], col = c("#1A9641", "#A6D96A", "#FFFFBF", "#FDAE61", "#D7191C"), breaks = seq(-1, 1, length=5)), 
                    border = "grey", lwd = 0.5)
        sector.sum[i] = sector.sum[i] + abs(m2[i, j])
        sector.sum[j] = sector.sum[j] + abs(m2[i, j])
    }
}

circos.clear()
par(op)
