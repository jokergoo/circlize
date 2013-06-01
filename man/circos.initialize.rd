\name{circos.initialize}
\alias{circos.initialize}
\title{
  Initialize the circos sectors


}
\description{
  Initialize the circos sectors


}
\usage{
circos.initialize(factors, x = NULL, xlim = NULL)
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data}
  \item{xlim}{Limitations for values on x-axis}

}
\details{
  The function allocates the sectors according to the values on x-axis.The number of sectors are determined by the \code{factors} and the orderof sectors are determined by the levels of factors. In this function,the start and end position  for each sector on the circle (measured by degree)are calculated according to the values on x-axis.

  If \code{x} is set, the length of \code{x} must be equal to the length of \code{factor}.Then the data range for each sector are calculated from \code{x} and \code{factor}.

  If \code{xlim} is set, it should be a vector containing two numbers or a matrix with 2 columns.If \code{xlim} is a vector, it means all sector share the same \code{xlim}.If \code{xlim} is a matrix, the number of rows should be equal to the number of categories (number of levels)identified by \code{factors}, then each row of \code{xlim} corresponds to the data range for each sectorand the order of rows is corresponding to the order of levels of \code{factors}.

  The function finally call \code{\link[graphics]{plot}} and be ready to draw.


}
