
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend lines
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom zoo rollmean rollsum rollmedian
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom grDevices dev.new
#' @importFrom Rcpp evalCpp
#' @useDynLib acc




plotAcc <- function(object,markbouts='FALSE'){
  
  
  ##
  ## Need code to reset graphical parameters here!!
  ##
  dev.new()
  
  if(names(object[1])=="totalDates"){
    counts <- object[[3]]$counts  # View(counts)
    time <- object[[3]]$TimeStamp
    PAState <- object[[3]]$inPA
    nonwearState <- object[[3]]$nonwear
    inbout <- object[[3]]$inboutPA
    
    par(mar=c(6, 4, 3, 2)) # bottom, left, top and right
    plot(counts,type="l",ylim=c((min(counts,na.rm=TRUE)-30),(max(counts,na.rm=TRUE)+300)),axes=FALSE, ann=FALSE)
    mytime <- strptime(time,format='%Y-%m-%d %H:%M:%S')
    timeaxis <- format(mytime, format="%Y-%m-%d")
    min <- format(mytime, format="%M")
    hour <- format(mytime, format="%H")
    
    if(length(time)<=120){
      hourmark <- ifelse(( min=="00" | min=="15" | min=="30" | min=="45"  | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(120<length(time) & length(time)<=600){
      hourmark <- ifelse(( min=="00" | min=="30" | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(600<length(time) & length(time)<=1440){
      hourmark <- ifelse(min=="00",1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>1440 & length(time)<=2880){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="06")|(hour=="12")|(hour=="18")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>2880 & length(time)<=4320){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="12")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>4320 & length(time)<=7200){
      hourmark <- ifelse(min=="00"  & ((hour=="00")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>7200){
      hourmark <- ifelse(min=="00"  & (hour=="00"),1,0)
      myrownum <- rep(1:length(mytime))
      #timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timeaxis <- format(mytime, format="%Y-%m-%d")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.9, xlab="Date")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    title(main="Plot of accelerometer data",cex.main=1.5)
    title(ylab="Activity counts")
    #title(xlab="Time",line=5)
    
    if(markbouts=='TRUE'){
      cols <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
      t <- rep(1:length(counts))
      
      Statebind <- data.frame(cbind(t,PAState))  
      Statebind$col[Statebind$PAState == 1] <- "thistle3"
      rect(xleft =Statebind$t-1, xright = Statebind$t, 
           ybottom=min(counts,na.rm=TRUE), ytop=max(counts,na.rm=TRUE), 
           col=Statebind$col, border=NA )
      legend(3*length(counts)/6, (max(counts,na.rm=TRUE))*1.12, 
             #c(expression(italic("Activity in ten minute bouts"))), 
             paste(object$pacut[1]," <= Activity counts <= ",object$pacut[2],sep=""), 
             text.col = "blue", cex=.8, horiz = TRUE, 
             bty = "n", fill=c("thistle3"), xjust = 0.01)
      
      lines(counts,type="l",ylim=c((min(counts)-30),(max(counts)+30)))
      
      barcols <- c("white","sienna1")
      for(ii in 0:length(inbout)){
        rect(xleft   = ii,
             ybottom = (min(counts,na.rm=TRUE)-max(counts,na.rm=TRUE)*.01),
             xright  = ii+1, 
             ytop    = (min(counts,na.rm=TRUE)-max(counts,na.rm=TRUE)*.04),
             col = barcols[inbout[ii]+1], border = 0)
        # Create a legend 
        #legend(1*length(counts)/6, (max(counts,na.rm=TRUE))*1.08, 
        legend(1*length(counts)/6, (max(counts,na.rm=TRUE))*1.12, 
               #"In bout", 
               paste("Activity in ",object$boutsize," minute bouts",sep=""), 
               text.col = "blue", cex=.8, horiz = TRUE, #xpd=TRUE, 
               bty = "n", fill=c("sienna1"), xjust = 0.01)
        
      }              
    }
    
    
  }
  
}