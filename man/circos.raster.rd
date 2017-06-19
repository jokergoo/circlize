\name{circos.raster}
\alias{circos.raster}
\title{
Add raster images
}
\description{
Add raster images
}
\usage{
circos.raster(image, x, y, width, height,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
    "downward", "bending.inside", "bending.outside"),
    niceFacing = FALSE, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    scaling = 1)
}
\arguments{

  \item{image}{a \code{raster} object, or an object that can be converted by \code{\link[grDevices]{as.raster}}}
  \item{x}{position of the center of the raster image, measued in the data coordinate in the cell}
  \item{y}{position of the center of the raster image, measued in the data coordinate in the cell}
  \item{width}{width of the raster image. When \code{facing} is one of "inside", "outside", "clockwise" and "reverse.clockwise", the image should have absolute size where the value of \code{width} should be specified like \code{20mm}, \code{1cm} or \code{0.5inche}. When \code{facing} is one of \code{bending.inside} and \code{bending.outside}, the value of \code{width} is measured in the data coordinate in the cell.}
  \item{height}{height of the raster image. Same format as \code{width}. If the value of \code{height} is omit,  default height is calculated by taking the aspect ratio of the original image. But when \code{facing} is one of \code{bending.inside} and \code{bending.outside}, \code{height} is mandatory to set.}
  \item{facing}{facing of the raster image}
  \item{niceFacing}{facing of text. Please refer to vignette for different settings}
  \item{sector.index}{index for the sector}
  \item{track.index}{index for the track}
  \item{scaling}{scaling factor to resize the raster image.}

}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
require(png)
image = system.file("extdata", "Rlogo.png", package = "circlize")
image = as.raster(readPNG(image))
circos.initialize(letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.raster(image, CELL_META$xcenter, CELL_META$ycenter, width = "2cm", 
        facing = "inside", niceFacing = TRUE)
})
circos.clear()

\dontrun{
# NOTE: following takes quite a long time to run
load(system.file("extdata", "doodle.RData", package = "circlize"))
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(letters[1:16], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    img = img_list[[CELL_META$sector.numeric.index]]
    circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, width = 1, 
        height = 1, facing = "bending.inside")
}, track.height = 0.25, bg.border = NA)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    img = img_list[[CELL_META$sector.numeric.index + 16]]
    circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, width = 1, 
        height = 1, facing = "bending.inside")
}, track.height = 0.25, bg.border = NA)
circos.clear()
}
}
