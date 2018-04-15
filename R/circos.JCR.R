#' Circular visualization of Journal Citing/Cited Relationships
#'
#' circos.JCR is a extension of 'circlize' package for drawing the graph of Journal Citing/Cited Relationships.
#' Graph example as http://ipscience-help.thomsonreuters.com/incitesLiveJCR/JCRGroup/jcrJournalProfile/jcrJournalProfileEgoNetwork.html

#' @param x Citing or cited data of JCR in sector.index1. The 1st value in dataset 'x' is the sector.index1 as default.
#' @param JCR.name Journal names (N.B., can NOT rename, i.e. No name is repeated each other).
#' @param col The color for sector.index and circos.link.
#' @param transparency Transparency of color.
#' @param order.by The direction of the circos, 0 = orde by the number of 'x', 1 = order by alphabet for 'JCR.name'.
#' @param self.include Whether the link include the parent journal. Default is FALSE.
#' @param start.degree For the start degree of the 1st sector.index. see \code{\link[circlize]{circos.par}}.
#' @param text.size The font size of text. Value as cex (e.g., 0.8).
#' @param track.height The height of one track.index.
#' @param ... additional parameters to \code{\link[circlize]{circos.link}},such as directional, rou, rou1, tou2, lty, arr.lty.
#'
#' @import circlize
#' @import grDevices
#' @import stats

#' @export

#' @examples
#'
#' library(circos.JCR)
#' x = c(15, 11, 18, 16, 14, 13, 12, 7)  # an example of citing data of journal "Nature"
#' Name =c("Nature", "PLOS ONE", "SCIENCE", "CELL", "ECOLOGY", "OTHER1", "OTHER2", "OTHER3")
#'
#' circos.JCR(x = x, JCR.name = Name, start.degree = 90, order.by = 0, text.size = 0.7)
#'
#' @author  Weiping Mei <meiweipingg@163.com>
#' @seealso \code{\link[circlize]{circos.link}}

circos.JCR<-
  function(x, JCR.name,
           col = NULL,transparency = 0.8,
           order.by = 0,
           self.include = FALSE,start.degree = 90,
           text.size = 0.5, track.height = 0.3,
           ...)
{


set.seed(999)
circlize::circos.clear()

if (length(x) != length(JCR.name)){
  stop( "Length of 'x' and 'JCR.name' should be the same!")
  }

# order.by setting
JCR.name = factor(JCR.name)
dataset.orgin = data.frame(x, JCR.name)

data.1row = dataset.orgin[1, ]
colnames(data.1row) <- c("x", "JCR.name")

data.2.N = dataset.orgin[-1, ]
colnames(data.2.N) <- c("x", "JCR.name")

  if (order.by == 0){

    dataset0 = data.2.N[order(data.2.N$x, decreasing = TRUE),]
    dataset0 = rbind(data.1row, dataset0)

    x = dataset0$x
    Name =dataset0$JCR.name
   }

  if (order.by == 1){

    dataset1 = data.2.N[order(data.2.N$JCR.name), ]
    dataset1 = rbind(data.1row, dataset1)

    x = dataset1$x
    Name =dataset1$JCR.name

    }

# basic graphic
  cell_cycle = data.frame(phase = factor(Name, levels = Name),
                        hour = x)
  color20 = c("#FF9900", "#FFCC00", "#808080", "#C0C0C0", "#0066CC",
              "#3399FF", "#660099", "#9966CC", "#990000", "#CC3300",
              "#339966", "#99CC66", "#646464", "#C0C0C0", "#4B4B4B",
              "#000000", "#FF6600", "#FF9900", "#003399", "#3366FF"
              )

  if (is.null(col)){
    col = color20
  }else{
      col=col}

  color = col

#
  transparency = transparency # c(0, 1)
  color.trans = adjustcolor(color, alpha.f = transparency)
#
  circlize::circos.par(start.degree = start.degree, cell.padding = c(0, 0, 0, 0), gap.degree = 0)

  circlize::circos.initialize(cell_cycle$phase, xlim = cbind(rep(0, length(x)), cell_cycle$hour))

  circlize::circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
      circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2],
               arrow.head.width = 0, arrow.head.length = 0,
               col = color.trans[CELL_META$sector.numeric.index],
               border = NA)
      circos.text(CELL_META$xcenter, CELL_META$cell.top.radius,
              CELL_META$sector.index,
              # if (radian in -90:90 degree){
              # adj = c(0, 1),
              # }else{
              adj = c(1, 0),
              #}
              facing = "downward", niceFacing = TRUE,
              cex = text.size,col = color[CELL_META$sector.numeric.index])
              },
      bg.border = NA, track.height = track.height)

# link width

  for (i in 2:length(x)){
    x = x

    if (self.include == FALSE){
    x2 = x[-1]
    x2 = c(0,x2)
    }else{
    x2=x}

    x3 = x2/sum(x2) #sum(x3)==1
    x4 = x3*x[1]  # sum(x4)==x[1]

    if (self.include == TRUE){
      circlize::circos.link(sector.index1=factor(Name[1]), c(0,x4[1]),
                sector.index2=factor(Name[1]), c(0, x4[1]),
                col = color.trans[1],border = color.trans[1],
                ...)}

    circlize::circos.link(sector.index1=factor(Name[1]), c(sum(x4[1:i-1]),sum(x4[1:i])),
              sector.index2=factor(Name[i]), c(0, x4[i]),
              col = color.trans[i],border = color.trans[i],
              ...)
  }
}

# Question to Zuguang Gu <z.gu@dkfz.de>, the maintainer of 'circlize' package.
# How to set the following message for
#              circos.text in Line 109 to Line 113 of this .R document?

# left panel of graph (-90,90)degree: adj=c(1,0)
# right panel of graph (90,270)degree: adj= c(0,1)
