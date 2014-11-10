\name{circos.genomicTrackPlotRegion}
\alias{circos.genomicTrackPlotRegion}
\title{
  Create a track for genomic graphics  


}
\description{
  Create a track for genomic graphics  


}
\usage{
circos.genomicTrackPlotRegion(data = NULL, ylim = NULL, stack = FALSE,
    numeric.column = NULL, jitter = 0,
    panel.fun = function(region, value, ...)  {NULL}, ...)
}
\arguments{
  \item{data}{A bed-file-like data frame or a list of data frames}
  \item{ylim}{If it is \code{NULL}, the value will be calculated from data. If \code{stack} is set to \code{TRUE}, this value is ignored.}
  \item{stack}{whether to plot in a "stack" mode.}
  \item{numeric.column}{Columns of numeric values in \code{data} that will be used for plotting.  If \code{data} is a data frame list, \code{numeric.column} should be either length of one or length of \code{data}. If value of \code{numeric.column} is not set, its value will depend on the structure of \code{data}. If \code{data} is a data frame, the default value for \code{numeric.column} is all the numeric column starting from the fourth column. If \code{data} is a list of data frame, the default value for \code{numeric.column} is a vector which have the same length as \code{data} and the value in default \code{numeric.column} is the index of the first numeric column in corresponding data frame.}
  \item{jitter}{Numeric. Only works for adding points in \code{circos.genomicTrackPlotRegion} under \code{stack} mode}
  \item{panel.fun}{Self-defined function which will be applied on each sector. Please not it is different from that in \code{\link{circos.trackPlotRegion}}. In this function, there are two arguments (\code{region} and \code{value}) plus \code{...}. In them, \code{region} is a two-column data frame with start positions and end positions in current genomic category (e.g. chromosome).  \code{value} is a data frame which is derived from \code{data} but excluding the first three columns. Rows in \code{value} correspond to  rows in \code{region}. \code{...} is mandatory and is used to pass internal parameters to other functions. The definition of \code{value} will be different according to different input data (data frame or list of data frame) and different settings (stacked or not),  please refer to 'details' section and vignettes to detailed explanation.}
  \item{...}{Pass to \code{\link{circos.trackPlotRegion}}.}

}
\details{
  Similar as \code{\link{circos.trackPlotRegion}}, users can add customized graphics by \code{panel.fun}, but the behaviour of \code{panel.fun} will change depending on users' input data and \code{stack} setting.  

  When \code{data} is a single data frame, \code{region} in \code{panel.fun} is a data frame containing the second and third column in \code{data} in 'current` genomic category (e.g. current chromosome). \code{value} is also a data frame containing columns in \code{data} excluding the first three columns.  

  When \code{data} is a list containing data frames, \code{panel.fun} will be applied iteratively on each data frame, thus,  \code{region} is extracted from the data frame which is in the current iteration. For example, if \code{data} contains two data frames, \code{panel.fun} will be applied with the first data frame in current chromosome and then applied with the second data frame in the same chromosome.  

  If \code{stack} is set to \code{TRUE}, \code{ylim} will be re-defined. in \code{stack} mode, the y-axis will be splitted into several part with equal height and graphics will be drawn on each 'horizontal' lines (y = 1, 2, ...). In this case:  

  When \code{data} is a single data frame containing one or more numeric columns, each numeric column defined in \code{numeric.column} will be treated as a single unit.  \code{ylim} is re-defined to \code{c(0.5, n+0.5)} in which \code{n} is number of numeric columns. \code{panel.fun} will be applied iteratively on each numeric column. In each iteration, in \code{panel.fun}, \code{region} is still the genomic regions in current genomic category, but \code{value} contains current numeric column plus all non-numeric columns. Under \code{stack} mode, in \code{panel.fun}, all low-level genomic graphical functions will draw on the 'horizontal line' \code{y = i} in which \code{i} is the index of current numeric column  and the value of \code{i} can be obtained by \code{\link{getI}}.  

  When \code{data} is a list containing data frames, each data frame will be treated as a single unit. The situation is quite similar as described in previous paragraph. \code{ylim} is re-defined to \code{c(0.5, n+0.5)} in which \code{n} is number of data frames. \code{panel.fun} will be applied iteratively on each data frame. In each iteration, in \code{panel.fun}, \code{region} is still the genomic regions in current genomic category, and \code{value} contains columns in current data frame excluding the first three columns. Under \code{stack} mode, in \code{panel.fun}, all low-level genomic graphical functions will draw on the 'horizontal line' \code{y = i} in which \code{i} is the index of current data frame.  

  Being different from \code{panel.fun} in \code{\link{circos.trackPlotRegion}}, there should be an additional argument \code{...} in \code{panel.fun}. This additional argument is used to pass hidden values to low-level graphical functions. So if you are using functions like \code{circos.genomicPoints}, you should also add \code{...} as an additional argument into \code{circos.genomicPoints}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
