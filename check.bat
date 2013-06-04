cd ..
R CMD REMOVE circlize
R CMD build circlize
R CMD check --as-cran --timings circlize_0.0.4.tar.gz
R CMD INSTALL circlize_0.0.4.tar.gz