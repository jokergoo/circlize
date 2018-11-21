
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



simAcc <- function(timelength,paLevel='moderate',epoch="1 min",
                   startDate=ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT"),
                   endDate=ISOdate(2017,1,10,hour=24,min=0,sec=0,tz="GMT"),
                   mu = c(0, 30, 2500),
                   sigma = c(0, 30, 1000),
                   seedset=1234,tpm=NULL){
  
  
  randomTime <- seq(startDate,endDate,epoch)
  
  # User specified 
  if( is.null(paLevel)==TRUE ){
    
    b <- list(mu = mu, sigma = sigma)
    J <- length(mu); initial <- rep(1/J, J)
    model <- hmmspec(init = initial, trans = tpm, parms.emission = b,dens.emission = dnorm.hsmm)
    train <- simulate.hmmspec(model, nsim = (timelength), seed = seedset, rand.emission = rnorm.hsmm)
    simdata <- data.frame(TimeStamp = randomTime[1:timelength], counts = round(train$x,0))
    
  }
  
  # Using pre set parameters 
  if( is.null(paLevel)==FALSE ){
    
    mu = c(0, 30, 2500)
    sigma = c(0, 30, 1000)
    
    J <- 3; initial <- rep(1/J, J)
    P <- matrix(rep(NA,9),byrow='TRUE',nrow=J)
    
    if(paLevel=='low'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.9, 0.01, 
                    0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
    }
    
    if(paLevel=='moderate'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.8, 0.11, 
                    0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
    }
    
    if(paLevel=='high'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.7, 0.21, 
                    0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
    }
    
    b <- list(mu = mu, sigma = sigma)
    model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
    train <- simulate.hmmspec(model, nsim = (timelength), seed = seedset, rand.emission = rnorm.hsmm)
    simdata <- data.frame(TimeStamp = randomTime[1:timelength], counts = round(train$x,0))
  }
  
  simdata
}
