\name{read.cytoband}
\alias{read.cytoband}
\title{
Read/parse cytoband data from a data frame/file/UCSC database
}
\description{
Read/parse cytoband data from a data frame/file/UCSC database
}
\usage{
read.cytoband(cytoband = system.file(package = "circlize",
    "extdata", "cytoBand.txt"), species = NULL, chromosome.index = usable_chromosomes(species),
    sort.chr = TRUE)
}
\arguments{

  \item{cytoband}{Path of the cytoband file or a data frame that already contains cytoband data}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{cytoBand.txt.gz} from UCSC website automatically.}
  \item{chromosome.index}{subset of chromosomes, also used to reorder chromosomes.}
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters). If \code{chromosome.index} is set, this argument is enforced to \code{FALSE}}

}
\details{
The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome. 
By default, it is human hg19 cytoband data.

You can find the data structure of the cytoband data from \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/cytoBand.txt.gz}
}
\value{
\describe{
  \item{\code{df}}{Data frame for cytoband data (rows are sorted if \code{sort.chr} is set to \code{TRUE})}
  \item{\code{chromosome}}{Sorted chromosome names}
  \item{\code{chr.len}}{Length of chromosomes. Orders are same as \code{chromosome}}
}
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
data = read.cytoband(species = "hg19")
data = read.cytoband(cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"))
cytoband = read.table(system.file(package = "circlize", "extdata", "cytoBand.txt"), 
    colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
data = read.cytoband(cytoband = cytoband)
}
