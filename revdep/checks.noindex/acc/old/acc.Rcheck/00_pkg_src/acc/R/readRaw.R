#' @export
#' @importFrom utils head tail 
#' @importFrom R.utils countLines
#' @importFrom iterators ireadLines nextElem 
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



readRaw <- function(filepath,type,resting=NULL){
  
  filelength <- countLines(filepath)
  fname <- filepath
  it <- ireadLines(con=fname,warn=FALSE)
  #on.exit(close(file(fname)))
  line1 <- nextElem(it)
  
  if(grepl("at", line1) == TRUE & grepl("Hz", line1) == TRUE){
    hertzPre <- sub(".*at ", "", line1)
    myhertz <- strsplit(hertzPre, " ")[[1]][1]
    devicePre <- sub(".*ActiGraph ", "", line1)
    device <- strsplit(devicePre, " ")[[1]][1]
    dateformatPre <- sub(".*date format ", "", line1)
    dateformat <- strsplit(dateformatPre, " ")[[1]][1]
  }
  
  line2 <- nextElem(it)
  serialNumberPre <- sub(".*Serial Number: ", "", line2)
  serialNumberPre2 <- strsplit(serialNumberPre, " ")[[1]][1]
  serialNumber <- gsub(',,','',serialNumberPre2)
  
  line3 <- nextElem(it)
  startTimePre <- sub(".*Start Time ", "", line3)
  startTimePre2 <- strsplit(startTimePre, " ")[[1]][1]
  startTime <- gsub(',,','',startTimePre2)
  
  line4 <- nextElem(it)
  startDatePre <- sub(".*Start Date ", "", line4)
  startDatePre2 <- strsplit(startDatePre, " ")[[1]][1]
  startDate <- gsub(',,','',startDatePre2)
  
  cat(noquote(paste("Raw data read for ", device, " device.", sep = "")))
  cat("\n")
  cat(noquote(paste("Start date is ", startDate, " and sampling rate is ", 
                    myhertz, " Hz.", sep = "")))
  cat("\n")
  
  invisible(nextElem(it));
  invisible(nextElem(it));
  invisible(nextElem(it));
  invisible(nextElem(it));
  invisible(nextElem(it));
  invisible(nextElem(it));
  invisible(nextElem(it));
  
  # MAD
  if(type == "mad" | type == "MAD" | type == "Mad"){
    
    windowSize <- 6*as.numeric(as.character(myhertz))
    numit <- floor((filelength[1]-11)/windowSize)
    mad <- rep(NA, numit)
    
    for(j in 1:numit){
      # A six second window
      mywindow <- matrix(rep(NA, windowSize*3),ncol=3)
      for(i in 1:windowSize){
        myline <- strsplit(nextElem(it), ",")[[1]]
        mywindow[i,] <- c(as.numeric(myline[1]),
                          as.numeric(myline[2]),
                          as.numeric(myline[3]))
      }
      
      vm1 <- sqrt(mywindow[,1]^2 + mywindow[,2]^2 + mywindow[,3]^2)
      vm1.mean <- mean(vm1,na.rm=TRUE)
      mad[j] <- mean(abs(vm1-vm1.mean),na.rm=TRUE)
    }
    
    timeseq <- seq(ISOdate(strsplit(startDate, "/")[[1]][3], 
                           strsplit(startDate, "/")[[1]][1], 
                           strsplit(startDate, "/")[[1]][2], 
                           hour = strsplit(startTime, ":")[[1]][1], 
                           min = strsplit(startTime, ":")[[1]][2], 
                           sec = strsplit(startTime, ":")[[1]][3], tz = "GMT"), 
                   by = "6 sec", length.out=length(mad))
    
    madCat <- mad
    madCat[mad < .9] <- "Below Moderate"
    madCat[mad >= .9 & mad < 4.14] <- "Moderate"
    madCat[mad >= 4.14] <- "Vigorous"
    
    mydata <-  data.frame(Time = timeseq,
                          MAD = mad,
                          pa.category = madCat)
  }
  
  
  # AI
  if(type == "ai" | type == "AI" | type == "Ai"){
    if((is.numeric(resting)==TRUE) & (length(resting)==3)){rsd <- resting}
    if((is.numeric(resting)==TRUE) & (length(resting)==1)){rsd <- rep(resting,3)}
    windowSize <- as.numeric(as.character(myhertz))
    numit <- floor((filelength[1]-11)/windowSize)
    ai <- rep(NA, numit)
    
    for(j in 1:numit){
      # A one second window
      mywindow <- matrix(rep(NA, windowSize*3),ncol=3)
      for(i in 1:windowSize){
        myline <- strsplit(nextElem(it), ",")[[1]]
        mywindow[i,] <- c(as.numeric(myline[1]),
                          as.numeric(myline[2]),
                          as.numeric(myline[3]))
      }
      
      sd11 <- sd(mywindow[,1],na.rm=TRUE)
      sd12 <- sd(mywindow[,2],na.rm=TRUE)
      sd13 <- sd(mywindow[,3],na.rm=TRUE)
      ai[j] <-max((((sd11-rsd[1])/rsd[1]) + ((sd12-rsd[2])/rsd[2]) + ((sd13-rsd[3])/rsd[3])),0)
    }
    
    timeseq <- seq(ISOdate(strsplit(startDate, "/")[[1]][3], 
                           strsplit(startDate, "/")[[1]][1], 
                           strsplit(startDate, "/")[[1]][2], 
                           hour = strsplit(startTime, ":")[[1]][1], 
                           min = strsplit(startTime, ":")[[1]][2], 
                           sec = strsplit(startTime, ":")[[1]][3], tz = "GMT"), 
                   by = "1 sec", length.out=length(ai))
    
    mydata <-  data.frame(Time = timeseq, AI = ai)
  }
  
  
  # Resting state
  if(type == "resting" | type == "Resting" | type == "RESTING"){
    
    windowSize <- as.numeric(as.character(myhertz))
    numit <- floor((filelength[1]-11)/windowSize)
    resting <- matrix(rep(NA,numit*3),ncol=3)
    
    for(j in 1:numit){
      # A one second window
      mywindow <- matrix(rep(NA, windowSize*3),ncol=3)
      for(i in 1:windowSize){
        myline <- strsplit(nextElem(it), ",")[[1]]
        mywindow[i,] <- c(as.numeric(myline[1]),
                          as.numeric(myline[2]),
                          as.numeric(myline[3]))
      }
      
      sd11 <- sd(mywindow[,1],na.rm=TRUE)
      sd12 <- sd(mywindow[,2],na.rm=TRUE)
      sd13 <- sd(mywindow[,3],na.rm=TRUE)
      resting[j,] <- c(sd11, sd12, sd13)
    }
    
    mydata <-  c(mean(resting[,1],na.rm=TRUE),mean(resting[,2],na.rm=TRUE),mean(resting[,3],na.rm=TRUE))
  }
  
  mydata
}
