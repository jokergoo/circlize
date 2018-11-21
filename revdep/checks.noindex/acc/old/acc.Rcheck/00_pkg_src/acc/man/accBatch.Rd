\name{accBatch}
\alias{accBatch}
\title{
Summarizes multiple accelerometer datafiles
}
\description{
Summarizes multiple accelerometer datafiles in a batch mode. Summary can be provided for multiple types of physical activities, by day.
}

\usage{
accBatch(path, tri, axis, spuriousDef, nonwearDef, minWear,
    patype, pacut, epoch, boutsize, tolerance)
}

\arguments{
  \item{path}{Path to accelerometer data files read in by function readCounts or readCountsBath. Files in this path can have both uni-axial and tri-axial data. If at least one tri-axial data is present in the path, please specify tri='TRUE' and axis. This information will be used to summarize tri-axial data.}
  \item{tri}{Is there at least one dataset from a tri-axial accelerometer in the folder? Default is tri=`TRUE'. If tri=`TRUE' then option `axis' should be specified. Default axis is axis='vm'.}
  \item{axis}{If the data is from a tri-axial device, this option is applied. Options are `x',`y',`z',`sum', or `vm'. Options `x', `y', or `z' can be spefied to summarize data using only data from a single axis. If the option 'vm' is used, the square root of the squared sum of counts from three axes (i.e. \eqn{\sqrt{{x}^{2}+{y}^{2}+{z}^{2}}}) are used for the summary. If the option 'sum' is used, sum of the counts from three axes are used.}
  \item{spuriousDef}{Definition of spurious observation. Defined as minutes of consecutive zeros. For example, if spuriousDef = 20, this means that an observation point will be determined as a spurious observation if there are consequtive counts of at least 20 zeros before and after the single non-zero observation. Default is  spuriousDef = 20.}
  \item{nonwearDef}{Definition of non-wear time. Defined as minutes of consecutive zeros. For example, if nonwearDef=60, this means that a period will be defined as non-wear time if there are at least 60 consecutive zeros. Default is nonwearDef=60. To consider all observations as wear time specify nonwearDef=`Inf'}
  \item{minWear}{Minimum wear time definition. Defined as minutes of wear time. or example, if minWear = 600, this means that a day will be considered valid only if the wear time is at least 600 minutes. Default is minWear = 600. To return summary for all dates in the data, set minWear = 0.}       
  \item{patype}{Types of physical activity for summary. For example, to summarize sedentary and moderate-vigorous physical activities, user specifies patype=c(`Sedentary',`MVPA'). This labels the summary accordingly.}
  \item{pacut}{Cut points to be used for the physical activity type. For example, if the user specified patype=c(`Sedentary',`MVPA'), pacut can be specified as pacut=c(c(0,99),c(1952,Inf)). The options requires to have a lower and a upper limit for each activity type (i.e. c(0,99) for sedentary activity). The specified interval includes its lower and upper endpoints (it is a closed inerval). }
  \item{boutsize}{Boutsize to summarize a physical activity. If multiple patype is specified, boutsize should be for each one (e.g., if patype=c(`Sedentary',`MVPA') then one can use  boutsize=c(10,10)).}
  \item{epoch}{Epoch size. Default is '1 min'. Other epoch size can be specified using this option (e.g., '1 sec')}
  \item{tolerance}{Whether two observations outside the physical activity should be permitted in summarizing a physical activity. If multiple patype is specified, tolerance should be for each one (e.g., if patype=c(`Sedentary',`MVPA') then one can use tolerance=c(`FALSE',`TRUE')).}
}


\value{
A folder `summaryfiles' is created within the specified path. In the folder, summary files are saved by the same filenames as in the accelerometer data for valid days which consists of columns
[Date, SedentaryMinutes, wearTime, numberOfBoutsSed, mvpaMinutes, numberOfBoutsMVPA] 
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}
\references{Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2011). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc, 43(2):357-64.

Hall, K. S., Howe, C. A., Rana, S. R., Martin, C. L., and Morey, M. C. (2013). METs and Accelerometry of Walking in Older Adults: Standard versus Measured Energy Cost. Medicine and Science in Sports and Medicine, 45(3). 574-82.

Freedson, P., Melanson, E.,  and Sirard, J. (1998). Calibration of the Computer Sciences and Applications, Inc. accelerometer. Medicine and Science in Sports and Exercercise, 30(5):777-81.

Swartz, A. M., Strath, S. J., Bassett, D. R. Jr., O'Brien, W. L., King, G. A., and Ainsworth, B. E. (2000). Estimation of energy expenditure using CSA accelerometers at hip and wrist sites. Medicine and Science in Sports and Exercercise, 32: S450-456. 

Copeland, J. L., and Esliger, D. W. (2009). Accelerometer assessment of physical activity in active, healthy older adults. J Aging Phys Act, 17: 17-30. }

\keyword{ accelerometer}

\examples{
##
## Example
##
\dontrun{
mypath <- "C:/Accelerometry files/readfiles"
accBatch(path=mypath, tri='TRUE', axis='vm',
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
}
}
