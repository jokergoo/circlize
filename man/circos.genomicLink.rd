\name{circos.genomicLink}
\alias{circos.genomicLink}
\title{
  Add links from two sets of genomic positions  


}
\description{
  Add links from two sets of genomic positions  


}
\usage{
circos.genomicLink(region1, region2,
    rou = get_most_inside_radius(), rou1 = rou, rou2 = rou,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, ...)
}
\arguments{
  \item{region1}{A genomic data frame}
  \item{region2}{A genomic data frame}
  \item{rou}{Pass to \code{\link{circos.link}}}
  \item{rou1}{Pass to \code{\link{circos.link}}}
  \item{rou2}{Pass to \code{\link{circos.link}}}
  \item{col}{Pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{lwd}{Pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{lty}{Pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{border}{Pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{...}{Pass to \code{\link{circos.link}}}

}
\details{
  Of course, number of rows should be same in \code{region1} and \code{region2}.  

  If you want to have more controls on links, please use \code{\link{circos.link}} directly. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
set.seed(123)

library(circlize)

par(mar = c(1, 1, 1, 1))
bed1 = generateRandomBed(nr = 100)
bed1 = bed1[sample(nrow(bed1), 20), ]
bed2 = generateRandomBed(nr = 100)
bed2 = bed2[sample(nrow(bed2), 20), ]
circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

circos.genomicLink(bed1, bed2, col = sample(1:5, 20, replace = TRUE), border = NA)
circos.clear()

}
}
