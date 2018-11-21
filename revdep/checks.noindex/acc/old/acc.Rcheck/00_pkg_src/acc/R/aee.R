#' @export
#' @importFrom stats model.matrix
#' @importFrom methods getClass
#' @importFrom Rcpp evalCpp
#' @useDynLib acc


aee <- function(ID, time, minutes) {
  if (sum(time <= 0) > 0)
    stop("Observation time must be positive.")
  
  index <- which(!duplicated(ID))
  N <- length(index)
  uniqueID <- ID[index]
  timeGrid <- sort(unique(time))
  runLength <- rle(as.numeric(ID))$lengths
  cumLength <- cumsum(runLength)
  
  #panelMatrix <- matrix(NA, N, length(timeGrid))
  #for (i in 1:N) {
  #  rowSet <- which(ID == uniqueID[i])
  #  panelMatrix[i, which(timeGrid %in% time[rowSet])] <- minutes[rowSet]
  #}
  
  panelMatrix <-  makePanelMatrix(time,minutes,uniqueID,timeGrid,cumLength)
  
  ps <- list(psDF=data.frame(ID=ID, time=time, minutes=minutes),
             timeGrid=timeGrid, panelMatrix=panelMatrix)
  class(ps) <- "aee"
  ps
}

is.aee <- function(x) inherits(x, "aee")