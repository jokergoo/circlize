cd ..
R CMD REMOVE circlize
R CMD build --compact-vignettes=gs+qpdf circlize
R CMD check --as-cran --timings circlize_0.0.9.tar.gz
R CMD INSTALL circlize_0.0.9.tar.gz