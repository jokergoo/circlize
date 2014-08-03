perl -Ilib -MR::Comment2Man -e "R::Comment2Man->draft('R')"

cd ..

R CMD REMOVE circlize
R CMD build --compact-vignettes=gs+qpdf circlize
R CMD check --as-cran --timings circlize_0.1.1.tar.gz
R CMD INSTALL circlize_0.1.1.tar.gz