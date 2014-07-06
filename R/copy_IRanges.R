## implement sort, reduce, findOverlaps and pintersect in IRanges package
## the reason is I don't want to make a dependency on IRanges package, it is too heavy

sort_region = function(region) {
	region[order(region[[1]], region[[2]]), , drop = FALSE]
}

# region has been sorted
reduce_region = function(region) {
	nr = nrow(region)
	r = data.frame(start = rep(NA, nr), end = rep(NA, nr))
	
	if(nr == 0) {
		return(r)
	} else if(nr == 1) {
		return(region)
	} else {
		r[1, ] = region[1, ]
		k = 1
		for(i in seq_len(nrow(region))[-1]) {
			if(region[i, 1] <= r[k, 2] + 1) {
				r[k, 2] = region[i, 2]
			} else {
				k = k + 1
				r[k, ] = region[i, ]
			}
		}
		return(r[!is.na(r[[1]]), , drop = FALSE])
	}
	
}

# to calculate how much region in gr1 are covered by gr2
# here gr1 and gr2 are all sorted
# gr2 are reduced
overlap_region = function(gr1, gr2, percent = TRUE) {
	nr1 = nrow(gr1)
	nr2 = nrow(gr2)
	
	overlap = rep(0, length = nr1)
	if(nr1 == 0) {
		return(overlap)
	}
	
	k_gr2 = 1
	for(i in seq_len(nr1)) {
		for(j in seq(k_gr2, nr2)) {
			if(gr2[j, 2] < gr1[i, 1]) {
				k_gr2 = ifelse(k_gr2 < nr2, k_gr2 + 1, nr2)
				next
			} else if(gr2[j, 1] > gr1[i, 2]) {
				break
			} else {
				overlap[i] = overlap[i] + overlap_interval(c(gr1[i, 1], gr1[i, 2]),
                                                           c(gr2[j, 1], gr2[j, 2]))
			}
		}
	}
	
	if(percent) {
		overlap / (gr1[, 2] - gr1[, 1] + 1)
	} else {
		overlap
	}
}

# how much in it1 is covered by it2
overlap_interval = function(it1, it2) {
	start = it1[1]
	end = it1[2]
	it2_start = it2[1]
	it2_end = it2[2]

	if(end < it2_start) {
		return(0)
	} else if(start <= it2_start & end >= it2_start & end <= it2_end) {
		return(end - it2_start + 1)
	} else if(it2_start <= start & it2_end >= end) {
		return(end - start + 1)
	} else if(start <= it2_start & end >= it2_end) {
		return(it2_end - it2_start + 1)
	} else if(it2_start <= start & start <= it2_end & it2_end <= end) {
		return(it2_end - start + 1)
	} else if(it2_end < start) {
		return(0)
	} else {
		return(0)
	}
}
