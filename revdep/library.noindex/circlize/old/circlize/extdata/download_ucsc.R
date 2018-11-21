species = scan(textConnection("anoGam1 apiMel2 braFlo1 caeJap1 caePb1 caePb2 caeRem2 caeRem3 
canFam1 canFam2 cb1 cb3 cbJul2002 ce1 ce10 ce11 ce2 ce4 ce6 ceMay2003 danRer1 danRer2 danRer3 
danRer4 dm1 dm2 dm3 dp2 dp3 droSim1 droYak1 droYak2 eboVir3 equCab1 equCab2 fr1 fr2 galGal2 
galGal3 gasAcu1 hg10 hg11 hg12 hg13 hg15 hg16 hg17 hg18 hg19 hg38 hg6 hg7 hg8 mm1 mm10 mm2 
mm3 mm4 mm5 mm6 mm7 mm8 mm9 monDom4 monDom5 panTro1 panTro2 ponAbe2 priPac1 rheMac2 rn1 rn2 
rn3 rn4 rn6 rnJan2003 rnJun2003 sacCer1 sacCer2 sacCer3 sc1 scApr2003 susScr2 taeGut1 tetNig1 tetNig2"), what = "character")

cytoband_list = lapply(species, function(s) {
	url = paste("http://hgdownload.cse.ucsc.edu/goldenPath/", s, "/database/cytoBand.txt.gz", sep = "")
	oe = try({
		download.file(url, destfile = paste0(s, ".cytoBand.txt.gz"), quiet = TRUE)
		d <- read.table(paste0(s, ".cytoBand.txt.gz"), colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t", stringsAsFactors = FALSE)
	})
	if(file.exists(paste0(s, ".cytoBand.txt.gz"))) {
		file.remove(paste0(s, ".cytoBand.txt.gz"))
	}
	if(inherits(oe, "try-error")) {
		return(NULL)
	} else {
		return(d)
	}
})
names(cytoband_list) = species
l = sapply(cytoband_list, is.null)
cytoband_list = cytoband_list[!l]
saveRDS(cytoband_list, file = "~/project/circlize/inst/extdata/cytoband_list.rds")


chrom_info_list = lapply(species, function(s) {
	url = paste("http://hgdownload.cse.ucsc.edu/goldenPath/", s, "/database/chromInfo.txt.gz", sep = "")
	oe = try({
		download.file(url, destfile = paste0(s, ".chromInfo.txt.gz"), quiet = TRUE)
		d <- read.table(paste0(s, ".chromInfo.txt.gz"), colClasses = c("character", "numeric"), sep = "\t", stringsAsFactors = FALSE)[1:2]
	})
	if(file.exists(paste0(s, ".chromInfo.txt.gz"))) {
		file.remove(paste0(s, ".chromInfo.txt.gz"))
	}
	if(inherits(oe, "try-error")) {
		return(NULL)
	} else {
		return(d)
	}
})
names(chrom_info_list) = species
l = sapply(chrom_info_list, is.null)
chrom_info_list = chrom_info_list[!l]
saveRDS(chrom_info_list, file = "~/project/circlize/inst/extdata/chrom_info_list.rds")


