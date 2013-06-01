source("R/global.R")
source("R/plot.R")
source("R/utils.R")


circos.initialize(factors = sample(letters[1:4], 20, replace = TRUE), xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()
circos.initialize(factors = sample(letters[1:4], 20, replace = TRUE), xlim = cbind(1:4, 1:4*2))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()
circos.initialize(factors = sample(letters[1:4], 20, replace = TRUE), x = rnorm(20))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()
circos.initialize(factors = sample(letters[1:4], 20, replace = TRUE), xlim = c(0, 1), x = rnorm(20))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()

factors = 1:8
circos.par(clock.wise = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
get(".SECTOR.DATA", envir = .CIRCOS.ENV)
circos.clear()

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
