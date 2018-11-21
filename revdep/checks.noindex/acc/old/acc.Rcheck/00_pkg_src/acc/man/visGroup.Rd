\name{visGroup}
\alias{visGroup}
\title{
Group level plots
}
\description{
Group level plots of minutes of activity per day
}

\usage{
visGroup(data,ID,activity,type,title,yaxis,xaxis,time,legendTitle,
         groupBy,groupFun,levels,heatcol)
}

\arguments{
  \item{data}{Summary data by day}
  \item{ID}{Name of column that identifies groups or individuals}
  \item{activity}{Name of column for activity to be plotted}
  \item{type}{Type of plot. Options 'boxplot' or 'heatmap'}
  \item{title}{Title of the plot}
  \item{yaxis}{Axis title for vertical axis}
  \item{xaxis}{Whether to pring the x axis label. Options are TRUE or FALSE}
  \item{time}{Name of column that contains time index}
  \item{legendTitle}{Title of legend for 'type=heatmap'}
  \item{groupBy}{Option to plot by group, for 'type=heatmap'}
  \item{groupFun}{If groupBy is specified, how are two groups defined? Provide a function to specify a statistic that divides the groups. For example, specify 'median' to divide the two groups by the overall median of the data.}
  \item{levels}{Specifies thresholds for heatmap. For example, user can specify 'levels = c(0,300,600,1440)' to plot a heatmap of four levels.}
  \item{heatcol}{Specifies colors for the heatmap.}
}
\value{
A plot is returned.
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}

\keyword{ accelerometer }

\examples{

##
## Example: A box plot
##
\dontrun{
library(acc)
data(NHANES)
NHANES_subset <- NHANES[1:300,]
visGroup(
data = NHANES_subset,
activity = 'wearTime',
ID = 'ID',
type = 'boxplot',
title = "Wear time at baseline",
yaxis = "Wear time (minutes per day)",xaxis=TRUE)


##
## Example: A heatmap
##
library(acc)
data(NHANES)
NHANES_subset <- NHANES[1:1000,]

visGroup(
data = NHANES_subset,
ID = 'ID',activity = 'wearTime',
type = 'heatmap',title = "Wear time at baseline",
yaxis = "ID",time = 'Day',
legendTitle = "Wear time",
groupBy = 'VO2max',groupFun = 'median',
levels = c(0,300,600,1440),
heatcol = c("white","mistyrose","lightpink","violetred1"))
}
}
