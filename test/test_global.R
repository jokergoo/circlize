source("R/global.R")
source("R/plot.R")
source("R/utils.R")


# circos.par
circos.par()
circos.par("gap.degree")
circos.par("gap.degree", "cell.padding")
circos.par("gap.degree" = 10)
circos.par("gap.degree")
circos.par("gap.degree" = 20, "cell.padding" = c(0, 0, 0, 0))
circos.par("gap.degree")
circos.par("cell.padding")

# warnings
circos.initialize(factors = 1:10, xlim = c(0,1))
circos.par("gap.degree" = 10)
circos.par("start.degree" = 10)

###############################
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
