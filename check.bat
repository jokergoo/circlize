perl -Ilib -MR::Comment2Man -e "R::Comment2Man->draft('R')"

cd ..

R CMD REMOVE circlize
cR CMD check --as-cran --timings circlize_0.0.9.tar.gz
R CMD INSTALL circlize_0.0.9.tar.gz