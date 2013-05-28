source("R/global.R")
source("R/plot.R")
source("R/utils.R")


factors = 1:8
circos.par(start.degree = 90)
circos.initialize(factors = factors, xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()

factors = 1:8
circos.par(start.degree = 90, clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()

factors = 1:8
circos.par(start.degree = -90)
circos.initialize(factors = factors, xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()

factors = 1:8
circos.par(start.degree = -90, clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()
