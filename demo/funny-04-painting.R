
library(circlize)
library(png)
library(jpeg)


par(mar = c(1, 1, 1, 1))
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = letters[1:4], xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.4)
 
link = c("http://www.google.com/logos/2011/mothersday11-hp.jpg",
         "http://www.google.com/logos/2012/newyearsday-2012-hp.jpg",
         "http://www.google.com/logos/2012/haring-12-hp.png",
         "http://www.google.com/logos/2012/4th_july-12-hp.png")

for(il in 1:4) {
	tmp_file = tempfile(fileext = ifelse(grepl("png$", link[il]), ".png", ".jpg"))
	download.file(link[il], tmp_file)
	
	if(grepl("png$", link[il])) {
		img <- readPNG(tmp_file)
	} else {
		img <- readJPEG(tmp_file)
	}
	
	nr = dim(img)[2]
	nc = dim(img)[1]
	
	w = 2
	
	for(i in seq(1, nr, by = w)) {
		for(j in seq(1, nc, by = w)) {
	
			col = rgb(img[nc-j+1,i,1], img[nc-j+1,i,2], img[nc-j+1,i,3])
	
			if(col != "#FFFFFF") {
				circos.rect((i-w/2)/nr, (j-w/2)/nc, (i+w/2)/nr, (j+w/2)/nc, col=col, border=col, sector.index = letters[il])
			}
			
		}
	}
}

circos.clear()
 
