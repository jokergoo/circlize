#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom Rcpp evalCpp
#' @useDynLib acc

accBatch <- function(path, tri='TRUE', axis='vm',
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     epoch=c('1 min','1 min'),
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE')){
  
  myfilenames <- list.files(path = path)
  newpath <- paste(path,"/summaryfiles",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  for(i in 1:length(myfilenames)){
    # i <- 1
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-5,mynchar)
    
    if(mystr == ".Rdata"){
      myfile <- paste(path,"/",myfilenames[i],sep="")
      counts <- NULL
      load(myfile)
      myID <- substr(myfilenames[i], 1, mynchar-6)
      person <- data.frame(ID = myID)
      if(ncol(counts)==4){
      summarized <- acc(data = counts, tri=tri, axis=axis,
                        spuriousDef=spuriousDef, nonwearDef=nonwearDef, minWear=minWear, 
                        patype=patype,pacut=pacut,epoch=epoch,
                        boutsize=boutsize, tolerance=tolerance)
      }
      if(ncol(counts)!=4){
        summarized <- acc(data = counts, tri='FALSE', axis=NULL,
                          spuriousDef=spuriousDef, nonwearDef=nonwearDef, minWear=minWear, 
                          patype=patype,pacut=pacut,epoch=epoch,
                          boutsize=boutsize, tolerance=tolerance)
      }
      summary <- merge(person, summarized)
      file.out <- paste(newpath,"/summary.",myID,".Rdata",sep="")
      save(summary, file = file.out) 
    }
  }
}
