\name{adjacencyList2Matrix}
\alias{adjacencyList2Matrix}
\title{
Convert adjacency list to an adjacency matrix
}
\description{
Convert adjacency list to an adjacency matrix
}
\usage{
adjacencyList2Matrix(lt, square = FALSE)
}
\arguments{

  \item{lt}{A data frame which contains adjacency list.}
  \item{square}{Should the returned matrix be a square matrix?}

}
\examples{
set.seed(123)
df = data.frame(from = sample(letters, 10, replace = TRUE), 
                to = sample(letters, 10, replace = TRUE), 
                value = 1:10)
adjacencyList2Matrix(df)
adjacencyList2Matrix(df, square = TRUE)
}
