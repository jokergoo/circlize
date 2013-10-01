
library(circlize)
library(png)
library(jpeg)
library(RCurl)

op = par(no.readonly = FALSE)
par(mar = c(1, 1, 1, 1))
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = letters[1:16], xlim = c(0, 1))

tmp_file = tempfile(fileext = ".jpg")

curl = getCurlHandle()
bfile=getBinaryURL (
        "http://www.thegreenhead.com/imgs/keith-haring-double-retrospect-worlds-largest-jigsaw-puzzle-2.jpg",
        curl= curl
)
writeBin(bfile, tmp_file)
rm(curl, bfile)

img = readJPEG(tmp_file)
	
nr = dim(img)[1]
nc = dim(img)[2]
	
w = 1

i_sum = 0
for(fi in 1:4) {
	for(fj in 1:8) {
		
		if((i_sum+1) %% 16 == 1) {
			circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.25)
		}
		
		img2 = img[floor((fi-1)/4*nr+1):floor(fi/4*nr), floor((fj-1)/8*nc+1):floor(fj/8*nc), ]
		
		nr2 = dim(img2)[1]
		nc2 = dim(img2)[2]
		
		for(i in seq(1, nr2, by = w)) {
			for(j in seq(1, nc2, by = w)) {
		
				col = rgb(img2[nr2-i+1,j,1], img2[nr2-i+1,j,2], img2[nr2-i+1,j,3])
		
				if(col != "#FFFFFF") {
					circos.rect((j-w/2)/nc2, (i-w/2)/nr2, (j+w/2)/nc2, (i+w/2)/nr2, col=col, border=col, sector.index = letters[(i_sum %% 16) + 1], track.index = floor(i_sum / 16)+1)
				}
				
			}
		}
		
		i_sum = i_sum + 1
	}
}	

circos.clear()
par(op)

