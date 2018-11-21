#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend mtext text
#' @importFrom circlize circos.clear circos.par circos.initialize circos.trackPlotRegion circos.lines circos.rect circos.text get.cell.meta.data
#' @importFrom grDevices dev.new
#' @importFrom Rcpp evalCpp
#' @useDynLib acc




racePlot <- function(summary, title,
                     cex.title=1.8, cex.text=1.4, cex.center=1.5,
                     color = c("cadetblue1", "wheat","violetred1")){
  ##
  ## Need code to reset graphical parameters here!!
  ##
  dev.new()
  summaryNames <- colnames(summary)
  minCols <- substring(summaryNames,(nchar(summaryNames)-6),nchar(summaryNames)) == 'minutes'
  minNames <- substring(summaryNames,1,(nchar(summaryNames)-8))[minCols]
  wearMin <- as.numeric(summary[ , which(summaryNames == 'wearTime')])
  minOther <- summary[ , which(minCols)]
  dates <- summary[ , 1]
  numDates <- length(dates)
  minutes <- as.matrix(cbind(wearMin,minOther))
  
  Category <- c("Wear Time",minNames)
  Category = rev(Category)
  minutes =  minutes[, rev(seq_len(ncol(minutes)))]
  color = rev(color)
  
  n.window.h <- ceiling(numDates/3)
  n.window.v <- ceiling(numDates/(numDates/3))
  par(mfrow=c(n.window.h,n.window.v))
  par(oma=c(1,1,2,1))  
  par(mar=c(0,0,2,0))  # mar = c(bottom, left, top, right) 
  
  
  for(i in 1:numDates){
    
    circos.clear()
    circos.par("start.degree" = 90)
    circos.initialize("a", xlim = c(0, (60*24))) # 'a` just means there is one sector
    circos.trackPlotRegion(ylim = c(0.5, length(minutes[i,])+0.5), track.height = 0.8, 
                           bg.border = NA, panel.fun = function(x, y) {
                             xlim = get.cell.meta.data("xlim") # in fact, it is c(0, 100)
                             
                             for(j in 1:length(Category)) {
                               circos.lines(xlim, c(j, j), col = "#CCCCCC")
                               minPlot <- (as.numeric(minutes[i,j]))
                               circos.rect(0, j - 0.45, minPlot, j + 0.45, col = color[j], border = "white")
                             }
                             
                             for(j in seq_along(minutes[i,])) {
                               minPlot <- (as.numeric(minutes[i,j]))
                               circos.text(xlim[2], j, paste0(Category[j], " (", minPlot, " min", ")"), 
                                           adj = c(1, 0.5),cex=cex.text,col="gray20") 
                             }
                           }
    )
    circos.clear()
    text(0, 0, paste("Day ",i,sep=""), col = "gray50",cex=cex.center)
  }
  
  mtext(title, outer = TRUE, col = "gray40",cex=cex.title)
}