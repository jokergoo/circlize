bed = random_bed(nr = 10)
normalizeToDataFrame(bed)
normalizeToDataFrame(bed[1:3])
normalizeToDataFrame(df2GRanges(bed))
normalizeToDataFrame(df2GRanges(bed[1:3]))


bed_list = list(random_bed(nr = 10, nc = 2),
                random_bed(nr = 10, nc = 2))
				
normalizeToDataFrame(bed_list)

bed_list = list(random_bed(nr = 10, nc = 0),
                random_bed(nr = 10, nc = 0))
				
normalizeToDataFrame(bed_list)

bed_list = list(random_bed(nr = 10, nc = 2),
                df2GRanges(random_bed(nr = 10, nc = 2)))
				
normalizeToDataFrame(bed_list)

bed_list = list(random_bed(nr = 10, nc = 0),
                df2GRanges(random_bed(nr = 10, nc = 0)))
				
normalizeToDataFrame(bed_list)
