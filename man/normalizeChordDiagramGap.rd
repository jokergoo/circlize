\name{normalizeChordDiagramGap}
\alias{normalizeChordDiagramGap}
\title{
Adjust gaps to make chord diagrams comparable

}
\description{
Adjust gaps to make chord diagrams comparable

}
\usage{
normalizeChordDiagramGap(mat1, gap.degree = circos.par("gap.degree"), mat2)}
\arguments{

  \item{mat1}{matrix that has the largest sum of absolute}
  \item{gap.degree}{gap.degree for the Chord Diagram which corresponds to \code{mat1}}
  \item{mat2}{matrix to be compared}
}
\details{
Normally, in Chord Diagram, values in mat are normalized to the summation and each value is put 
to the circle according to its percentage, which means the width for each link only represents 
kind of relative value. However, when comparing two Chord Diagrams, it is necessary that unit 
width of links in the two plots should be represented in a same scale. This problem can be solved by 
adding more blank gaps to the Chord Diagram which has smaller values.

}
\value{
Sum of gaps for \code{mat2}.

}
