\name{circos.stackedText}
\alias{circos.stackedText}
\title{
Add a stacked text track
}
\description{
Add a stacked text track
}
\usage{
circos.stackedText(sectors, x, text,
    col = par("col"), font = par("font"), cex = par("cex"), family = par("family"),
    bg.border = "black", bg.col = "#FF8080",
    niceFacing = FALSE,
    side = c("outside", "inside"))
}
\arguments{

  \item{sectors}{A vector of sector names.}
  \item{x}{A vector of x-coordinates.}
  \item{text}{A vector of texts.}
  \item{bg.border}{Background color.}
  \item{bg.col}{Colors for borders.}
  \item{niceFacing}{Current not supported.}
  \item{side}{Side of the track.}
  \item{col}{Text colors.}
  \item{font}{Text fontfaces.}
  \item{cex}{Font sizes.}
  \item{family}{Font families.}

}
\details{
The height of the track is not fixed, so you may need to manually adjust the track height.
}
\examples{
\dontrun{
circos.par$circle.margin = 0.5
circos.par$cell.padding = rep(0, 4)
circos.par$track.margin = rep(0, 2)

circos.initialize(sectors = letters[1:4], xlim = c(0, 1))

sectors = sample(letters[1:4], 40, replace = TRUE)
x = runif(40)
text = sapply(letters[sample(26, 40, replace = TRUE)], function(x) strrep(x, sample(4:6, 1)))
circos.stackedText(sectors, x, text, bg.col = "#EEEEEE")

circos.track(ylim = c(0, 1))
circos.clear()

#### genome plot
circos.par$track.margin = rep(0, 2)
circos.par$cell.padding = rep(0, 4)

circos.initializeWithIdeogram(plotType = NULL)
bed = generateRandomBed(50)
text = sapply(
    letters[sample(26, nrow(bed), replace = TRUE)], 
    function(x) strrep(x, sample(4:6, 1))
)
bed$text = text

circos.stackedText(bed[, 1], bed[, 2], bed$text, cex = 0.7)
circos.genomicIdeogram()
circos.clear()

}
}
