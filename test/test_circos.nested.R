set.seed(123)
df = data.frame(cate = sample(letters[1:8], 400, replace = TRUE),
                x = runif(400),
                y = runif(400),
                stringsAsFactors = FALSE)
df = df[order(df[[1]], df[[2]]), ]
rownames(df) = NULL
df$interval_x = as.character(cut(df$x, c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))
df$name = paste(df$cate, df$interval_x, sep = ":")
df$start = as.numeric(gsub("^\\((\\d(\\.\\d)?).*(\\d(\\.\\d)?)]", "\\1", df$interval_x))
df$end = as.numeric(gsub("^\\((\\d(\\.\\d)?),(\\d(\\.\\d)?)]$", "\\3", df$interval_x))
nm = sample(unique(df$name), 20)
df2 = df[df$name %in% nm, ]

correspondance = unique(df2[, c("cate", "start", "end", "name", "start", "end")])
zoom_sector = unique(df2[, c("name", "start", "end", "cate")])
zoom_data = df2[, c("name", "x", "y")]

data = df[, 1:3]
sector = data.frame(cate = letters[1:8], start = 0, end = 1, stringsAsFactors = FALSE)

sector_col = structure(rand_color(8, transparency = 0.5), names = letters[1:8])

f1 = function() {
    circos.par(gap.degree = 10)
    circos.initialize(sector[, 1], xlim = sector[, 2:3])
    circos.track(data[[1]], x = data[[2]], y = data[[3]], ylim = c(0, 1), 
        panel.fun = function(x, y) {
            circos.points(x, y, pch = 16, cex = 0.5, col = "red")
    })
}

f2 = function() {
    circos.par(gap.degree = 2, cell.padding = c(0, 0, 0, 0))
    circos.initialize(zoom_sector[[1]], xlim = as.matrix(zoom_sector[, 2:3]))
    circos.track(zoom_data[[1]], x = zoom_data[[2]], y = zoom_data[[3]], 
        panel.fun = function(x, y) {
            circos.points(x, y, pch = 16, cex = 0.5)
        })
}

circos.nested(f1, f2, correspondance)
# random shuffle correspondance should be affect the plot
circos.nested(f1, f2, correspondance[sample(nrow(correspondance), nrow(correspondance)), ])

# correspance only contains subset of sector in f1
circos.nested(f1, f2, correspondance[correspondance[, 1] != "b", ])
# correspance only contains subset of sector in f2
circos.nested(f1, f2, correspondance[!grepl("a", correspondance[, 4]), ])
# correspance only contains subset of sector in f2
circos.nested(f1, f2, correspondance[correspondance[, 1] != "b" & !grepl("a", correspondance[, 4]), ])

circos.nested(f2, f1, correspondance[, c(4:6, 1:3)])

# change the sector order of f1() should give some warnings
sector_bk = sector
sector = sector[sample(nrow(sector), nrow(sector)), ]
circos.nested(f1, f2, correspondance)
sector = sector_bk

zoom_sector_bk = zoom_sector
zoom_sector = zoom_sector[sample(nrow(zoom_sector), nrow(zoom_sector)), ]
circos.nested(f1, f2, correspondance)
zoom_sector = zoom_sector_bk

