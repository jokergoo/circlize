region = data.frame(start = c(4, 1, 7),
                    end = c(9, 5, 10))

region = sort_region(region)


r1 = data.frame(c(1, 5, 10),
                c(4, 9, 14))
r2 = data.frame(start = c(4, 1, 7),
                    end = c(9, 5, 10))

overlap_region(r1, reduce_region(sort_region(r2)))
