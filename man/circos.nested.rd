\name{circos.nested}
\alias{circos.nested}
\title{
Nested zooming with two circular plots
}
\description{
Nested zooming with two circular plots
}
\usage{
circos.nested(f1, f2, correspondance, connection_height = convert_height(5, "mm"),
    connection_col = NA, connection_border = "black",
    connection_lty = par("lty"), connection_lwd = par("lwd"),
    adjust_start_degree = TRUE)
}
\arguments{

  \item{f1}{a self-defined function for making the first circular plot. The function should have no argument.}
  \item{f2}{a self-defined function for making the second circular plot. The function should have no argument.}
  \item{correspondance}{a six-column data frame which contains correspondance between the coordinates in two circular plots}
  \item{connection_height}{the height of the connection track, measured as the percent to the radius of the unit circle. The value can be specified by \code{\link{uh}} or \code{\link{convert_height}} with absolute units.}
  \item{connection_col}{filled color of the connection track. The value can be a vector with same length as number of rows of \code{correspondance}}
  \item{connection_border}{border color of the connection track.}
  \item{connection_lty}{line style of the connection track borders}
  \item{connection_lwd}{line width of the connection track borders}
  \item{adjust_start_degree}{If \code{circos.par(start.degree = ...)} is not set in \code{f2()}, the start degree for the second circular plot will be adjusted to make the distance of sectors between the two plots to the minimal.}

}
\details{
The function visualizes zoomings by combining two circular plots into one page where
one is the normal circular plot and the other one only contains regions that need to be zoomed.
This function automatically arranges the two plots to make it easy to correspond between
the original and the zoomed sectors.

Since the function needs to know the information of the two circular plots, please do not call
\code{\link{circos.clear}} in either \code{f1()} or \code{f2()}. It will be called internally in \code{\link{circos.nested}}.

If \code{adjust_start_degree} is set to \code{TRUE}, \code{start.degree} should not be set in \code{f2()}.
Also \code{canvas.xlim} and \code{canvas.ylim} are reset in \code{f2()}, they should not be set in \code{f2()}
either.
}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/nested-zooming.html}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
\dontrun{
#### simulate data ####
set.seed(123)
df = data.frame(cate = sample(letters[1:8], 400, replace = TRUE),
                x = runif(400),
                y = runif(400),
                stringsAsFactors = FALSE)
df = df[order(df[[1]], df[[2]]), ]
rownames(df) = NULL
df$interval_x = as.character(cut(df$x, c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))
df$name = paste(df$cate, df$interval_x, sep = ":")
df$start = as.numeric(gsub("^\\\\((\\\\d(\\\\.\\\\d)?).*(\\\\d(\\\\.\\\\d)?)]", "\\\\1", df$interval_x))
df$end = as.numeric(gsub("^\\\\((\\\\d(\\\\.\\\\d)?),(\\\\d(\\\\.\\\\d)?)]$", "\\\\3", df$interval_x))
nm = sample(unique(df$name), 20)
df2 = df[df$name \%in\% nm, ]

correspondance = unique(df2[, c("cate", "start", "end", "name", "start", "end")])
zoom_sector = unique(df2[, c("name", "start", "end", "cate")])
zoom_data = df2[, c("name", "x", "y")]

data = df[, 1:3]
sector = data.frame(cate = letters[1:8], start = 0, end = 1, stringsAsFactors = FALSE)

sector_col = structure(rand_color(8, transparency = 0.5), names = letters[1:8])

#### define two circular plots ####
f1 = function() {
    circos.par(gap.degree = 10)
    circos.initialize(sector[, 1], xlim = sector[, 2:3])
    circos.track(data[[1]], x = data[[2]], y = data[[3]], ylim = c(0, 1), 
        panel.fun = function(x, y) {
            l = correspondance[[1]] == CELL_META$sector.index
            if(sum(l)) {
                for(i in which(l)) {
                    circos.rect(correspondance[i, 2], CELL_META$cell.ylim[1],
                                correspondance[i, 3], CELL_META$cell.ylim[2],
                                col = sector_col[CELL_META$sector.index],
                                border = sector_col[CELL_META$sector.index])
                }
            }
            circos.points(x, y, pch = 16, cex = 0.5)
            circos.text(CELL_META$xcenter, CELL_META$ylim[2] + uy(2, "mm"), 
                CELL_META$sector.index, niceFacing = TRUE, adj = c(0.5, 0))
    })
}

f2 = function() {
    circos.par(gap.degree = 2, cell.padding = c(0, 0, 0, 0))
    circos.initialize(zoom_sector[[1]], xlim = as.matrix(zoom_sector[, 2:3]))
    circos.track(zoom_data[[1]], x = zoom_data[[2]], y = zoom_data[[3]], 
        panel.fun = function(x, y) {
            circos.points(x, y, pch = 16, cex = 0.5)
        }, bg.col = sector_col[zoom_sector$cate],
        track.margin = c(0, 0))
}
circos.nested(f1, f2, correspondance, connection_col = sector_col[correspondance[[1]]])
}

}
