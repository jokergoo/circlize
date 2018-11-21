#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



readCountsBatch <- function(path,filetype=NULL){
  
  if(is.null(filetype)=='TRUE'){
    myfilenames <- list.files(path = path)
  }
  if(is.null(filetype)=='FALSE'){
    allfilenames <- list.files(path = path)
    myfilenames = allfilenames[ grep(filetype, allfilenames) ]
  }
  newpath <- paste(path,"/readfiles",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  for(i in 1:length(myfilenames)){
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-2,mynchar)
    
    if(mystr == "dat" | mystr == "csv" | mystr == "agd"){
      infilename <- paste(path,"/",myfilenames[i],sep="")
      myID <- substr(myfilenames[i], 1, mynchar-4)
      counts <- readCounts(infilename)
      file.out <- paste(newpath,"/",myID,".Rdata",sep="")
      save(counts, file = file.out) 
    }
  }
}