
library(jpeg)
library(RCurl)

tmp_file = tempfile(fileext = ".jpg")

curl = getCurlHandle()
bfile=getBinaryURL (
    "http://www.thegreenhead.com/imgs/keith-haring-double-retrospect-worlds-largest-jigsaw-puzzle-2.jpg",
    curl= curl
)
writeBin(bfile, tmp_file)
rm(curl, bfile)

img = readJPEG(tmp_file)

nr = nrow(img)
nc = ncol(img)
wp = nc/8
hp = nr/4
img_list = list()
for(j in 1:4) {
	for(i in 1:8) {
		ind1 = seq(round((i-1)*wp) + 1, round(i*wp))
		ind2 = seq(round((j-1)*hp) + 1, round(j*hp))
		img_list = c(img_list, list(img[ind2, ind1, ]))
	}
}

par(mfrow = c(4, 8), mar = c(0, 0, 0, 0))
for(i in seq_along(img_list)) {
	plot(1, ann = FALSE, axes = FALSE, xlim = c(0, 1), ylim = c(0, 1))
	rasterImage(img_list[[i]], 0.1, 0.1, 0.9, 0.9)
}


