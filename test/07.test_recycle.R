# recycle.with.factors
factors = factor(sample(letters[1:5], 20, replace = TRUE)) 
recycle.with.factors(1, factors)
recycle.with.factors(1:5, factors)
recycle.with.factors(1:10, factors)
recycle.with.factors(NULL, factors)


# recycle.with.levels
levels = letters[1:5]
recycle.with.levels(1, levels)
recycle.with.levels(1:5, levels)
recycle.with.levels(1:3, levels)
recycle.with.levels(NULL, levels)
