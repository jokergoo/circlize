\name{read.chromInfo}
\alias{read.chromInfo}
\title{
Read/parse chromInfo data from a data frame/file/UCSC database
}
\description{
Read/parse chromInfo data from a data frame/file/UCSC database
}
\usage{
read.chromInfo(chromInfo = system.file(package = "circlize",
    "extdata", "chromInfo.txt"), species = NULL, chromosome.index = usable_chromosomes(species),
    sort.chr = TRUE)
}
\arguments{

  \item{chromInfo}{Path of the chromInfo file or a data frame that already contains chromInfo data}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{chromInfo.txt.gz} from UCSC website automatically.}
  \item{chromosome.index}{subset of chromosomes, also used to reorder chromosomes.}
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters). If \code{chromosome.index} is set, this argument is enforced to \code{FALSE}}

}
\details{
The function read the chromInfo data, sort the chromosome names and calculate the length of each chromosome. 
By default, it is human hg19 chromInfo data.

You can find the data structure for the chromInfo data from \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/chromInfo.txt.gz}
}
\value{
\describe{
  \item{\code{df}}{Data frame for chromInfo data (rows are sorted if \code{sort.chr} is set to \code{TRUE})}
  \item{\code{chromosome}}{Sorted chromosome names}
  \item{\code{chr.len}}{Length of chromosomes. Order are same as \code{chromosome}}
}
}
\examples{
data = read.chromInfo(species = "hg19")
data = read.chromInfo(chromInfo = system.file(package = "circlize", "extdata", "chromInfo.txt"))
chromInfo = read.table(system.file(package = "circlize", "extdata", "chromInfo.txt"), 
    colClasses = c("character", "numeric"), sep = "\t")
data = read.chromInfo(chromInfo = chromInfo)
}
