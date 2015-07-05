\name{adjacencyList2Matrix}
\alias{adjacencyList2Matrix}
\title{
Convert adjacency list to adjacency matrix

}
\description{
Convert adjacency list to adjacency matrix

}
\usage{
adjacencyList2Matrix(lt, square = FALSE)}
\arguments{

  \item{lt}{a data frame which contains adjacency list.}
  \item{square}{should returned matrix a square matrix?}
}
\details{
Convert adjacency list to adjacency matrix.

}
\examples{
lt = data.frame(letters[1:5], letters[6:10])
adjacencyList2Matrix(lt)

lt = data.frame(letters[1:5], letters[6:10], 1:5)
adjacencyList2Matrix(lt)

set.seed(123)
lt = data.frame(sample(letters, 4), sample(letters, 4), 1:4)
adjacencyList2Matrix(lt)
adjacencyList2Matrix(lt, square = TRUE)}
