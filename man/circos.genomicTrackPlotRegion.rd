\name{circos.genomicTrackPlotRegion}
\alias{circos.genomicTrackPlotRegion}
\title{
  Create or update a track with genomic graphics  


}
\description{
  Create or update a track with genomic graphics  


}
\usage{
circos.genomicTrackPlotRegion(data, ylim = NULL, stack = FALSE, numeric.column = NULL,
    panel.fun = function(region, value, ...)  {NULL}, ...)
}
\arguments{
  \item{data}{A bed-file-like data frame or a \code{GRanges} object. It can also be a list containing data frames and \code{GRanges} objects.}
  \item{ylim}{if it is \code{NULL}, the value will be calculated from data. If \code{stack} is set to \code{TRUE}, the value is disabled.}
  \item{stack}{If \code{data} is a list of data frames or contains numeric columns more than one, whether to plot in a "stack" mode}
  \item{numeric.column}{Columns of numeric values in \code{data} that will be used for plotting.  If it is from a \code{GRanges} object, its value start from meta-columns.}
  \item{panel.fun}{self-defined function which will be applied on each sector. Please not it is different from that in \code{\link{circos.trackPlotRegion}}. In this function, there are two arguments (\code{region} and \code{value}) plus \code{...}. In them, \code{region} is a two-column data frame with start positions and end positions in current genomic category (e.g. chromosome).  \code{value} is a data frame which is derived from \code{data} but excluding the first three columns. Rows in \code{value} correspond to  rows in \code{region}. \code{...} is mandatory and is used to pass internal parameters to other functions.}
  \item{...}{pass to \code{\link{circos.trackPlotRegion}}.}

}
\details{
  Similar as \code{\link{circos.trackPlotRegion}}, users can add customized graphics by \code{panel.fun}, but the behavior of \code{panel.fun} would change depending on users' input data and \code{stack} setting.  

  When \code{data} is a single data frame, \code{region} in \code{panel.fun} is a data frame containing the second and third column in \code{data} in 'current` chromosome. \code{value} is also a data frame containing columns in \code{data} excluding the first three columns.  

  When \code{data} is a list containing data frames and/or \code{GRanges} objects, \code{panel.fun} will be applied iteratively on each data frame, thus,  \code{region} is extracted from the data frame which is in current iteration.  

  If \code{stack} is set, \code{ylim} in argument list will be re-defined. in \code{stack} mode, the y-axis will be splitted into several part with equal height and graphics will be drawn on each 'horizontal' lines. In this case:  

  When \code{data} is a single data frame, each numeric column defined in \code{numeric.column} will be treated as a single unit. If low-level genomic graphical function such \code{\link{circos.genomicPoints}} is applied, the y-axis will be re-defined to the index of current numeric column.  

  When \code{data} is a list containing data frames and/or \code{GRanges} objects, each data frame will be treated as a single unit, and will be drawn on horizontal lines.  

  Being different from \code{panel.fun} in \code{\link{circos.trackPlotRegion}}, there should be an additional argument \code{...} in \code{panel.fun}. This additional argument is used to pass values to low-level functions. 


}
