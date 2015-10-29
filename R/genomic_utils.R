# == title
# Read/parse cytoband data from a data frame/file/UCSC database
#
# == param
# -cytoband Path of the cytoband file or a data frame that already contains cytoband data
# -species  Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download ``cytoBand.txt.gz`` or ``chromInfo.txt.gz`` from
#          UCSC website automatically.
# -chromosome.index subset of chromosomes, also used to re-set chromosome orders.
# -sort.chr Whether chromosome names should be sorted (first sort by numbers then by letters).
#           If ``chromosome.index`` is set, this argument is enforced to ``FALSE``
#
# == details
# The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome. 
# By default, it is human hg19 cytoband data.
#
# You can find the data structure for the cytoband data from http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/cytoBand.txt.gz
#
# If ``sort.chr`` is not set and ``chromosome.index`` is not specified, there would be several circumstances when 
# determining the order of chromosomes. Assuming ``chromosome`` is the first column in the cytoband data frame,
# then, if ``cytoband`` is defined as a file path, or ``species`` is set, the order of chromosomes is ``unique(chromosome)`` 
# which is read from the file; If ``cytoband``
# is set as a data frame and the first column is a factor, the order of chromosomes is ``levels(chromosome)``; If ``cytoband`` is a data frame
# and the first column is just a character vector, the order of chromosomes is ``unique(chromosome)``. Please not this concept is really
# important since the order of chromosomes will be used to control the order of sectors when initializing the circos plot.
#
# == values
# -df         Data frame for cytoband data (rows are sorted if ``sort.chr`` is set to ``TRUE``)
# -chromosome Sorted chromosome names
# -chr.len    Length of chromosomes. Orders are same as ``chromosome``
#
read.cytoband = function(cytoband = paste0(system.file(package = "circlize"),
    "/extdata/cytoBand.txt"), species = NULL, chromosome.index = NULL, sort.chr = TRUE) {

	# this function should also take charge of the order of chromosome
	if(!is.null(chromosome.index)) sort.chr = FALSE
	
	# `species` is prior to `cytoband`
	if(!is.null(species)) {
		url = paste("http://hgdownload.soe.ucsc.edu/goldenPath/", species, "/database/cytoBand.txt.gz", sep = "")
		cytoband = paste0(circos.par("__tempdir__"), "/", species, "_cytoBand.txt.gz")
		if(!file.exists(cytoband)) {
			e = try(suppressWarnings(download.file(url, destfile = cytoband, quiet = TRUE)), silent = TRUE)
			if(class(e) == "try-error") {
				file.remove(cytoband)
				stop("It seems your species name is wrong or UCSC does not provide cytoband data\nfor your species or internet connection was interrupted.\nIf possible, download cytoBand file from\n", url, "\nand use `read.cytoband(file)`.\n")
			}
		}
	}
	
	if(is.data.frame(cytoband)) {
		d = cytoband
	} else {
		if(grepl("\\.gz$", cytoband)) {
			cytoband = gzfile(cytoband)
		}
		
		d = read.table(cytoband, colClasses = c("character", "numeric", "numeric", "character", "character"), sep = "\t", stringsAsFactors = FALSE)
	}

	# now cytoband data is normalized to `d`
	if(!is.null(chromosome.index)) {
		chromosome.index = intersect(chromosome.index, unique(as.vector(d[[1]])))
		d = d[d[[1]] %in% chromosome.index, , drop = FALSE]
		if(is.factor(d[[1]])) {
			# re-factor because levels may decrease due to `chromosome.index`
			levels(d[[1]]) = intersect(chromosome.index, unique(as.vector(d[[1]])))  # ensures the remaining order is same as in `chromosome.index`
		}
	}

	if(nrow(d) == 0) {
		stop("Cannot find any chromosome. It is probably related to your chromosome names having or not having 'chr' prefix.")
	}
	
	if(is.null(chromosome.index)) {
		if(is.factor(d[[1]])) {
			chromosome = levels(d[[1]])
		} else {
			chromosome = unique(d[[1]])
		}
	} else {
		chromosome = chromosome.index
	}

	if(sort.chr) {
		chromosome = sort_chr(chromosome)
	}

	chr.len = NULL
	dnew = NULL
	for(chr in chromosome) {
		d2 = d[d[[1]] == chr, ]
		chr.len = c(chr.len, max(d2[, 3]))
		dnew = rbind(dnew, d2)
	}
	names(chr.len) = chromosome
	
	return(list(df = dnew, chromosome = chromosome, chr.len = chr.len))
}

# == title
# Read/parse chromInfo data from a data frame/file/UCSC database
#
# == param
# -chromInfo Path of the chromInfo file or a data frame that already contains chromInfo data
# -species  Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download ``chromInfo.txt.gz`` from
#          UCSC website automatically.
# -chromosome.index subset of chromosomes, also used to re-set chromosome orders.
# -sort.chr Whether chromosome names should be sorted (first sort by numbers then by letters).
#           If ``chromosome.index`` is set, this argument is enforced to ``FALSE``#
#
# == details
# The function read the chromInfo data, sort the chromosome names and calculate the length of each chromosome. 
# By default, it is human hg19 chromInfo data.
#
# You can find the data structure for the cytoband data from http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/chromInfo.txt.gz
#
# If ``sort.chr`` is not set and ``chromosome.index`` is not specified, there would be several circumstances when determining the order of chromosomes. 
# Assuming ``chromosome`` is the first column in the chromInfo data frame,
# then, if ``chromInfo`` is defined as a file path, or ``species`` is set, the order of chromosomes is ``unique(chromosome)`` 
# which is read from the file; If ``chromInfo``
# is set as a data frame and the first column is a factor, the order of chromosomes is ``levels(chromosome)``; If ``chromInfo`` is a data frame
# and the first column is just a character vector, the order of chromosomes is ``unique(chromosome)``. Please not this concept is really
# important since the order of chromosomes will be used to control the order of sectors when initializing the circos plot.
#
# == values
# -df         Data frame for chromInfo data (rows are sorted if ``sort.chr`` is set to ``TRUE``)
# -chromosome Sorted chromosome names
# -chr.len    Length of chromosomes. Order are same as ``chromosome``
#
read.chromInfo = function(chromInfo = paste0(system.file(package = "circlize"),
    "/extdata/chromInfo.txt"), species = NULL, chromosome.index = NULL, sort.chr = TRUE) {
	
	# this function should also take charge of the order of chromosome
	if(!is.null(chromosome.index)) sort.chr = FALSE
	
	if(!is.null(species)) {
		url = paste("http://hgdownload.soe.ucsc.edu/goldenPath/", species, "/database/chromInfo.txt.gz", sep = "")
		chromInfo = paste0(circos.par("__tempdir__"), "/", species, "_chromInfo.txt.gz")
		if(!file.exists(chromInfo)) {
			e = try(suppressWarnings(download.file(url, destfile = chromInfo, quiet = TRUE)), silent = TRUE)
			if(class(e) == "try-error") {
				file.remove(chromInfo)
				stop("It seems your species name is wrong or UCSC does not provide chromInfo data\nfor your species or internet connection was interrupted.\nIf possible, download chromInfo file from\n", url, "\nand use `read.chromInfo(file)`.\n")
			}
		}
	}
	
	if(is.data.frame(chromInfo)) {
		d = chromInfo
	} else {
		if(grepl("\\.gz$", chromInfo)) {
			chromInfo = gzfile(chromInfo)
		}
		
		d = read.table(chromInfo, colClasses = c("character", "numeric"), sep = "\t", stringsAsFactors = FALSE)[1:2]  # only first two columns
	}
	rownames(d) = d[[1]]

	if(!is.null(chromosome.index)) {
		chromosome.index = intersect(chromosome.index, unique(as.vector(d[[1]])))
		d = d[d[[1]] %in% chromosome.index, , drop = FALSE]
		if(is.factor(d[[1]])) {
			# re-factor because levels may decrease due to `chromosome.index`
			levels(d[[1]]) = intersect(chromosome.index, unique(as.vector(d[[1]])))  # ensures the remaining order is same as in `chromosome.index`
		}
	}

	if(nrow(d) == 0) {
		stop("Cannot find any chromosome. It is probably related to your chromosome names having or not having 'chr' prefix.")
	}
	
	# now cytoband data is normalized to `d`
	if(!is.null(chromosome.index)) {
		chromosome.index = intersect(chromosome.index, unique(as.vector(d[[1]])))
		d = d[d[[1]] %in% chromosome.index, , drop = FALSE]
		if(is.factor(d[[1]])) {
			# re-factor because levels may decrease due to `chromosome.index`
			levels(d[[1]]) = intersect(chromosome.index, unique(as.vector(d[[1]])))  # ensures the remaining order is same as in `chromosome.index`
		}
	}

	if(is.null(chromosome.index)) {
		if(is.factor(d[[1]])) {
			chromosome = levels(d[[1]])
		} else {
			chromosome = unique(d[[1]])
		}
	} else {
		chromosome = chromosome.index
	}

	if(sort.chr) {
		chromosome = sort_chr(chromosome)
	}

	dnew = d[chromosome, , drop = FALSE]
	chr.len = dnew[[2]]
	names(chr.len) = chromosome

	dnew = data.frame(chr = chromosome, start = rep(0, nrow(dnew)), end = dnew[[2]], stringsAsFactors = FALSE)
	
	return(list(df = dnew, chromosome = chromosome, chr.len = chr.len))
}


# == title
# Assign colors to cytogenetic band (hg19) according to the Giemsa stain results
#
# == param
# -x A vector containing the Giemsa stain results
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
	names(col) = NULL
    return(col)
}

# == title
# Generate random genomic data
#
# == param
# -nr  Number of rows
# -nc  Number of numeric columns / value columns
# -fun Function for generating random values
# -species species, pass to `read.cytoband`
#
# == details
# The function will uniformly sample positions from the genome. Chromosome names start with "chr"
# and positions are sorted. The final number of rows may not be exactly as same as ``nr``.
generateRandomBed = function(nr = 10000, nc = 1, fun = function(k) rnorm(k, 0, 0.5),
    species = NULL) {
	cyto = read.cytoband(species = species)
	chr.len = cyto$chr.len
	chromosome = cyto$chromosome
	dl = lapply(seq_along(chr.len), function(i) {
		k = round(nr*2 * chr.len[i] / sum(chr.len))
		k = ifelse(k %% 2, k + 1, k)
		breaks = sort(sample(chr.len[i], k))
		res = data.frame(chr = rep(chromosome[i], length(breaks)/2),
						  start = breaks[seq_along(breaks) %% 2 == 1],
						  end = breaks[seq_along(breaks) %% 2 == 0],
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
	
	if(ncol(df) == 3) {
		colnames(df) = c("chr", "start", "end")
	} else {
		colnames(df) = c("chr", "start", "end", paste0("value", seq_len(ncol(df)-3)))
	}
	return(df)
}


sort_chr = function(chromosome) {
	chromosome = sort(chromosome)
	
	chromosome.ind = gsub("chr", "", chromosome)
	chromosome.ind = gsub("_.*$", "", chromosome.ind)
	l_num = grepl("^\\d+$", chromosome.ind)
	l_letter = !l_num

	chromosome.num = chromosome[l_num]
	chromosome.levels = chromosome[!l_num]

	chromosome.num = chromosome.num[order(as.numeric(chromosome.ind[l_num]))]
	chromosome.letter = chromosome.levels[order(chromosome.ind[!l_num])]
	chromosome = c(chromosome.num, chromosome.letter)
	return(chromosome)
}

