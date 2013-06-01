source("R/plot.R")
source("R/utils.R")
source("R/global.R")
source("R/link.R")

#############
# test histgrame

par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")

circos.clear()
