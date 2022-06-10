
set.seed(123)
bed = generateRandomBed(nr = 50, fun = function(k) sample(letters, k, replace = TRUE))
bed[1, 4] = "aaaaa"

circos.par$xaxis.clock.wise = FALSE

circos.initializeWithIdeogram(plotType = "axis")
circos.genomicLabels(bed, labels.column = 4, side = "inside")
circos.clear()
