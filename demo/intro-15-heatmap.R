
# dend: dendrogram object, usually returned by hclust
# maxy: height of the dendrogram tree
circos.dendrogram = function(dend, maxy) {
  labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5
    names(x) = labels

    is.leaf = function(object) (is.logical(L <- attr(object, "leaf"))) && L

    draw.d = function(dend, maxy) {
        leaf = attr(dend, "leaf")
        d1 = dend[[1]]
        d2 = dend[[2]]
        height = attr(dend, 'height')
        midpoint = attr(dend, 'midpoint')

        if(is.leaf(d1)) {
            x1 = x[as.character(attr(d1, "label"))]
        } else {
            x1 = attr(d1, "midpoint") + x[as.character(labels(d1))[1]]
        }
        y1 = attr(d1, "height")

        if(is.leaf(d2)) {
            x2 = x[as.character(attr(d2, "label"))]
        } else {
            x2 = attr(d2, "midpoint") + x[as.character(labels(d2))[1]]
        }
        y2 = attr(d2, "height")

        circos.lines(c(x1, x1), maxy - c(y1, height), straight = TRUE)
        circos.lines(c(x1, x2), maxy - c(height, height))
        circos.lines(c(x2, x2), maxy - c(y2, height), straight = TRUE)

        if(!is.leaf(d1)) {
            draw.d(d1, maxy)
        }
        if(!is.leaf(d2)) {
            draw.d(d2, maxy)
        }
    }
    
    draw.d(dend, maxy)
}


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

library(circlize)
mat = matrix(rnorm(100*10), nrow = 10, ncol = 100)
factors = rep(letters[1:2], 50)
par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(factors, xlim = c(0, 50))
maxy = 0
circos.trackPlotRegion(ylim = c(0, 10), bg.border = NA, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
    m = mat[, factors == sector.index]
    
    dend.col = as.dendrogram(hclust(dist(t(m))))

    maxy = ifelse(maxy > attr(dend.col, "height"), maxy, attr(dend.col, "height"))
    assign("maxy", maxy, envir = .GlobalEnv)

    m2 = m[, labels(dend.col)]
    nr = nrow(m2)
    nc = ncol(m2)
    for(i in 1:nr) {
        for(j in 1:nc) {
            circos.rect(j-1, nr-i, j, nr-i+1, border = color.pal(m2[i, j], breaks = c(-2, 0, 2)), col = color.pal(m2[i, j], breaks = c(-2, 0, 2)))
        }
    }
    
})
circos.trackPlotRegion(ylim = c(0, maxy), bg.border = NA, track.height = 0.3, panel.fun = function(x, y) {
    sector.index = get.cell.meta.data("sector.index")
    m = mat[, factors == sector.index]
    
    dend.col = as.dendrogram(hclust(dist(t(m))))

    circos.dendrogram(dend.col, maxy)
    
})
circos.clear()