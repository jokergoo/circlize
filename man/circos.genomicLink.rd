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
    rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA,
    top.ratio.low = NULL)
}
\arguments{
  \item{region1}{a genomic data frame}
  \item{region2}{a genomic data frame}
  \item{rou}{pass to \code{\link{circos.link}}}
  \item{top.ratio}{pass to \code{\link{circos.link}}}
  \item{col}{pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{lwd}{pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{lty}{pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{border}{pass to \code{\link{circos.link}}, length can be either one or nrow of \code{region1}}
  \item{top.ratio.low}{pass to \code{\link{circos.link}}}

}
\details{
  Of course, number of rows should be same in \code{region1} and \code{region2}. 


}
