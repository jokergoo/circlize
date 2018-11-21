#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom Rcpp evalCpp
#' @useDynLib acc

acc <- function(data, tri='FALSE', axis=NULL,
                spuriousDef=20, nonwearDef=60, minWear=600, 
                patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                epoch=c('1 min','1 min'),
                boutsize=c(10,10), tolerance=c('FALSE','TRUE')){
  
  summaryByPA <- list() 
  
  if(length(patype)!=0){
    for(i in 1:length(patype)){ 
      summaryByPA[[patype[i]]] <- accSummary(data=data, tri=tri, axis=axis,
                                             spuriousDef=spuriousDef, 
                                             nonwearDef=nonwearDef, minWear=minWear, 
                                             patype=patype[i],epoch=epoch[i],
                                             pacut=c(pacut[seq(1, length(pacut), 2)][i],pacut[seq(2, length(pacut), 2)][i]), 
                                             boutsize=boutsize[i], tolerance=tolerance[i],returnbout='FALSE')
    }
  }
  
  summary <- Reduce(function(...) merge(..., all=TRUE), summaryByPA)
  summary[is.na(summary)] <- 0
  summary <- summary[ which(summary$wearTime>=minWear), ]
  summary
}
