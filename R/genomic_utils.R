# == title
# Read cytoband data
#
# == param
# -file path of the uncompressed cytoband file
# -species abbrevations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically.
#
# == details
# The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome. 
# By default, it is human hg19 cytoband data.
#
# == values
#
# -df Original data frame for cytoband data
# -chromosome sorted chromosome names
# -chr.len length of chromosomes. Order are same as ``chromosome``
read.cytoband = function(file = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), species = NULL) {
	
	if(!is.null(species)) {
		url = paste("http://hgdownload.cse.ucsc.edu/goldenPath/", species, "/database/cytoBand.txt.gz", sep = "")
		file = paste(tempdir(), "/cytoBand.txt.gz", sep = "")
		e = try(download.file(url, destfile = file, quiet = TRUE), silent = TRUE)
		if(class(e) == "try-error") {
			stop("Seems your species name is wrong or UCSC does not provide cytoband data for your species.\nIf possible, download cytoBand file from\n", url, "\nand use `read.cytoband(file)`.\n")
		}
	}
	
	if(grepl("\\.gz$", file)) {
		file = gzfile(file)
	}
	
	d = read.table(file, colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t")
	
	chromosome = unique(d[[1]])
	chromosome.ind = gsub("chr", "", chromosome)
	chromosome.num = grep("^\\d+$", chromosome.ind, value = TRUE)
	chromosome.letter = chromosome.ind[!grepl("^\\d+$", chromosome.ind)]
	chromosome.num = sort(as.numeric(chromosome.num))
	chromosome.letter = sort(chromosome.letter)
	chromosome.num = paste("chr", chromosome.num, sep = "")
	chromosome.letter = paste("chr", chromosome.letter, sep = "")

	chromosome = c(chromosome.num, chromosome.letter)
	
	chr.len = NULL
	dnew = NULL
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		chr.len = c(chr.len, max(d2[, 3]))
		dnew = rbind(dnew, d2)
	}
	
	return(list(df = dnew, chromosome = chromosome, chr.len = chr.len))
}


# == title
# Assign colors to cytogenetic band according to the Giemsa stain results
#
# == param
# -x a vector containing the Giemsa stain results
#
# == details
# The color theme is from http://circos.ca/tutorials/course/slides/session-2.pdf, page 42.
cytoband.col = function(x) {
	x = as.vector(x)
	col.panel = c("gpos100" = rgb(0, 0, 0, maxColorValue = 255), 
                  "gpos"    = rgb(0, 0, 0, maxColorValue = 255),
                  "gpos75"  = rgb(130, 130, 130, maxColorValue = 255),
                  "gpos66"  = rgb(160, 160, 160, maxColorValue = 255),
                  "gpos50"  = rgb(200, 200, 200, maxColorValue = 255),
                  "gpos33"  = rgb(210, 210, 210, maxColorValue = 255),
                  "gpos25"  = rgb(200, 200, 200, maxColorValue = 255),
                  "gvar"    = rgb(220, 220, 220, maxColorValue = 255),
                  "gneg"    = rgb(255, 255, 255, maxColorValue = 255),
                  "acen"    = rgb(217, 47, 39, maxColorValue = 255),
                  "stalk"   = rgb(100, 127, 164, maxColorValue = 255) )
    col = col.panel[x]
    col[is.na(col)] = "#FFFFFF"
    return(col)
}

# == title
# generate random genomic data
#
# == param
# -nr number of rows
# -nc number of numeric columns
# -fun function to generate random data
#
# == details
# The function will sample positions form human genome and chromosome names start with "chr".
# Positions are sorted
generateRandomBed = function(nr = 10000, nc = 1, fun = function(k) rnorm(k, 0, 0.5)) {
	cyto = read.cytoband()
	chr.len = cyto$chr.len
	chromosome = cyto$chromosome
	dl = lapply(seq_along(chr.len), function(i) {
		k = round(nr*2 * chr.len[i] / sum(chr.len))
		k = ifelse(k %% 2, k + 1, k)
		breaks = sort(sample(chr.len[i], k))
		res = data.frame(chr = rep(chromosome[i], length(breaks)/2),
						  start = breaks[seq_along(breaks) %% 2 == 1],
						  start = breaks[seq_along(breaks) %% 2 == 0],
						  stringsAsFactors = FALSE)
		for(k in seq_len(nc)) {
			res = cbind(res, value = fun(length(breaks)/2))
		}
		res
	})

	df = NULL
	for(i in seq_along(dl)) {
		df = rbind(df, dl[[i]])
	}
	return(df)
}
