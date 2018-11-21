#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend mtext text boxplot
#' @importFrom ggplot2 ggplot guide_legend aes geom_tile facet_grid theme_bw theme element_text scale_fill_manual ggtitle labs guides scale_x_continuous scale_y_discrete unit
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



visGroup <- function(data,ID,activity,
                     type,title,yaxis,xaxis=TRUE,
                     time=NULL,legendTitle=NULL,
                     groupBy=NULL,groupFun=NULL,
                     levels=NULL,heatcol=NULL){
  
  ##
  ## Need code to reset graphical parameters here!!
  ##
  dev.new()
  
  par(mfrow=c(1,1))
  if(type == 'boxplot' | type == 'Boxplot'){
    toPlot <- data[,which(colnames(data)==activity),]
    ID <- data[,which(colnames(data)==ID),]
    med <- sort(with(data , tapply(toPlot, ID, median)))
    myIDs <- names(med)
    numID <- length(unique(ID))  # 50 1.2; 100 .8     50*1.2   60/50   60/100
    par(mgp=c(3.3,1,0))
    par(oma=c(2,.5,.5,.5)) #c(bottom, left, top, right) 
    boxplot(toPlot ~ factor(ID, levels = names(med)),
            col = "lightgray", names=myIDs, 
            main=title,cex.main=1.5,ann = FALSE,xaxt='n')
    #xlab="ID",cex.lab=1.2,cex.axis=min(1.2,(60/numID)),
    #ylab=yaxis,las=2)
    title(ylab=yaxis, cex.lab = 1.2,line = 3.2)
    #axis(2,at=seq(1,max(toPlot,na.rm=TRUE),5),cex.axis=1.2)
    if(xaxis==TRUE){
      axis(1,at=1:numID,labels=myIDs,cex.axis=min(1.2,(60/numID)),las=2)
      title(xlab="ID", cex.lab = 1.2,line = 4)
    }
  }
  
  if(type == 'heatmap' | type == 'Heatmap'){
    
    if(is.null(groupBy)==FALSE){
      legendTitle <- paste(legendTitle, ":   ", sep="")
      Time <- NULL
      data$Time <- NULL
      data$Time <- data[,which(colnames(data)==time),]
      plotBy <- data[,which(colnames(data)==groupBy),]
      groupingCut <- apply(as.matrix(plotBy), 2, eval(parse(text = groupFun)))
      grp1.labels <- paste(groupBy," < ",groupingCut, sep="")
      grp2.labels <- paste(groupBy," >= ",groupingCut, sep="")
      data$group <- ifelse(plotBy < groupingCut,grp1.labels,grp2.labels)
      toPlot <- data[,which(colnames(data)==activity),]
      ID <- data[,which(colnames(data)==ID),]
      cat <- cut(toPlot, breaks=levels,dig.lab = 5)
      data$category <- ifelse(toPlot==0,0,cat)
      data$labels <- ifelse(toPlot==0,"0",as.character(cat))
      numlevel <- (length(table(data$labels))-1)
      catnames <- names(table(data$labels))[1:numlevel]
      data$labels <- factor(data$category,
                             levels=names(table(data$category)),
                             labels=c("0    ",paste(catnames, "   ", sep="")),
                             ordered=TRUE)
      collapsed <- do.call("rbind", by(data, INDICES=ID, FUN=function(DF) DF[which.max(DF$Time), ]))
      if(dim(collapsed)[1]>300){stop("The heatmap may not be optimal when there are data for more than 300 individuals.")}
      
      p1 <- ggplot(data, aes(Time, as.factor(data$ID)), color=data$labels) + geom_tile(aes(fill = data$labels), colour = "gray90") +
        facet_grid(group ~., scales = "free_y") + 
        theme_bw() +
        theme(strip.text.y = element_text(size = 12, colour = "gray30", vjust =0.5, face="bold")) + 
        scale_fill_manual(values=heatcol,drop=FALSE) +
        theme(axis.text.y=element_text(size=5, color="gray60")) +
        ggtitle(title) +
        labs(x="Number of days since the first available measurement day",y="ID") + 
        theme(axis.text.x=element_text(size=12, vjust=0, color="gray40", hjust=0.5)) +
        theme(axis.text.y=element_text(size=12, vjust=0.4, color="gray40")) +
        theme(plot.title = element_text(size=14, vjust=2)) +
        theme(axis.title.x = element_text(size=12,color="gray40", vjust=0)) +
        theme(axis.title.y = element_text(size=12,color="gray40" , vjust=1.2)) +
        theme(legend.title=element_text(size=12,color="gray30" , hjust=-10)) + 
        theme(legend.text = element_text(size = 12,color="gray30")) + 
        guides(fill=guide_legend(override.aes=list(colour="black"))) +  
        #scale_x_continuous(breaks=c(1,2,3,4,5,6,7)) +
        scale_x_continuous(breaks=1:max(data$Time)) +
        scale_y_discrete(breaks=c(1,(1:10)*(length(unique(data$ID))/10))) + 
        theme(panel.margin = unit(c(0.3), "in")) + 
        theme(legend.position="bottom") +
        labs(fill = legendTitle) 
      p1
    }
    
    
    if(is.null(groupBy)==TRUE){
      legendTitle <- paste(legendTitle, ":   ", sep="")
      data$Time <- data[,which(colnames(data)==time),]
      toPlot <- data[,which(colnames(data)==activity),]
      ID <- data[,which(colnames(data)==ID),]
      cat <- cut(toPlot, breaks=levels,dig.lab = 5)
      data$category <- ifelse(toPlot==0,0,cat)
      data$labels <- ifelse(toPlot==0,"0",as.character(cat))
      numlevel <- (length(table(data$labels))-1)
      catnames <- names(table(data$labels))[1:numlevel]
      data$labels <- factor(data$category,
                             levels=names(table(data$category)),
                             labels=c("0    ",paste(catnames, "   ", sep="")),
                             ordered=TRUE)
      collapsed <- do.call("rbind", by(data, INDICES=ID, FUN=function(DF) DF[which.max(DF$Time), ]))
      if(dim(collapsed)[1]>300){stop("The heatmap may not be optimal when there are data for more than 300 individuals.")}
      
      p1 <- ggplot(data, aes(Time, as.factor(data$ID)), color=data$labels) + geom_tile(aes(fill = data$labels), colour = "gray90") +
        theme_bw() +
        theme(strip.text.y = element_text(size = 12, colour = "gray30", vjust =0.5, face="bold")) + 
        scale_fill_manual(values=heatcol,drop=FALSE) +
        theme(axis.text.y=element_text(size=5, color="gray60")) +
        ggtitle(title) +
        labs(x="Number of days since the first available measurement day",y="ID") + 
        theme(axis.text.x=element_text(size=12, vjust=0, color="gray40", hjust=0.5)) +
        theme(axis.text.y=element_text(size=12, vjust=0.4, color="gray40")) +
        theme(plot.title = element_text(size=14, vjust=2)) +
        theme(axis.title.x = element_text(size=12,color="gray40", vjust=0)) +
        theme(axis.title.y = element_text(size=12,color="gray40" , vjust=1.2)) +
        theme(legend.title=element_text(size=12,color="gray30" , hjust=-10)) + 
        theme(legend.text = element_text(size = 12,color="gray30")) + 
        guides(fill=guide_legend(override.aes=list(colour="black"))) +  
        #scale_x_continuous(breaks=c(1,2,3,4,5,6,7)) +
        scale_x_continuous(breaks=1:max(data$Time)) +
        scale_y_discrete(breaks=c(1,(1:10)*(length(unique(data$ID))/10))) + 
        theme(panel.margin = unit(c(0.3), "in")) + 
        theme(legend.position="bottom") +
        labs(fill = legendTitle) 
      p1
    }
    p1
  }	
}