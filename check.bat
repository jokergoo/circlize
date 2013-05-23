cd ..
R CMD REMOVE circlize
R CMD build circlize
R CMD check --as-cran circlize_0.0.2.tar.gz
R CMD INSTALL circlize_0.0.2.tar.gz