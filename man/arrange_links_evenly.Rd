\name{arrange_links_evenly}
\alias{arrange_links_evenly}
\title{
Arrange links evenly on each sector
}
\description{
Arrange links evenly on each sector
}
\usage{
arrange_links_evenly(df, directional = 0)
}
\arguments{

  \item{df}{A data frame with two columns. The values should only contain sector names.}
  \item{directional}{Whether the links are directional.}

}
\details{
This function only deals with single-line links.
}
\value{
A data frame with four columns of the sectors and the positions of the links.
}
\examples{
sectors = letters[1:20]
df = data.frame(from = sample(sectors, 40, replace = TRUE),
                to   = sample(sectors, 40, replace = TRUE),
                stringsAsFactors = FALSE)
df = unique(df)
df = df[df$from != df$to, ]

circos.initialize(sectors, xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index)
})

df2 = arrange_links_evenly(df, directional = 1)

for(i in seq_len(nrow(df2))) {
    s1 = df$from[i]
    s2 = df$to[i]
    circos.link(df2[i, "sector1"], df2[i, "pos1"], 
                df2[i, "sector2"], df2[i, "pos2"],
                directional = 1)
}
}
