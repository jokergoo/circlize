\name{circos.initializeWithIdeogram}
\alias{circos.initializeWithIdeogram}
\title{
  Initialize the circos layout with an ideogram


}
\description{
  Initialize the circos layout with an ideogram


}
\usage{
circos.initializeWithIdeogram(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), track.height = 0.1)
}
\arguments{
  \item{file}{cytoband file. By default it is the cytoband data for human}
  \item{track.height}{height for the track}

}
\details{
  This is not a full functional function. It jus provides a way to show how todraw genomics ideogram by this package. How to embed the ideogram into thecircos layout is really subjective and should be applied according to specific situation.

  In fact, draw ideogram with this package is really simple, you can look at the source codeof this function to get a clue.

  The cytoband data for human is downloaded from UCSC ftp site (\url{http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz),}should be uncompressed.


}
