bed = random_bed(nr = 10)
normalizeToDataFrame(bed)
normalizeToDataFrame(bed[1:3])


bed_list = list(random_bed(nr = 10, nc = 2),
                random_bed(nr = 10, nc = 2))
				
normalizeToDataFrame(bed_list)

bed_list = list(random_bed(nr = 10, nc = 0),
                random_bed(nr = 10, nc = 0))
				
normalizeToDataFrame(bed_list)

