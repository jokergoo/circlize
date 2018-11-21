## py2r.R: Conversion of SciPy sparse matrix to R
### Copyright (C) 2017  Binxiang Ni and Dirk Eddelbuettel
##
## This file is part of RcppArmadillo.
##
## RcppArmadillo is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## RcppArmadillo is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

## Reference: https://docs.scipy.org/doc/scipy-0.19.1/reference/sparse.html

.runThisTest <- requireNamespace("Matrix", quietly=TRUE) && requireNamespace("reticulate", quietly=TRUE)

if (.runThisTest) {

    suppressMessages({
        library(Matrix)
        library(reticulate)
    })

    if (py_module_available("scipy")) { # SciPy implies NumPy too

        np <- import("numpy")
        mat <- np$array(list(list(1, 0, 4), list(0, 0, 5), list(2, 3, 6)))
        sp <- import("scipy.sparse")

        mtxt <- c("1 0 4",
                  "0 0 5",
                  "2 3 6")
        M <- as.matrix(read.table(text=mtxt))
        dimnames(M) <- NULL
        
        # Since 'reticulate' automatically converts CSC matrix to dgCMatrix, 
        # no need to convert it in RcppArmadillo
        
        test.csc2dgc <- function() {
            csc <- sp$csc_matrix(mat)
            dgC <- methods::as(M, "dgCMatrix")
            checkEquals(dgC, csc, msg="csc2dgc")
        }

        test.coo2dgt <- function() {
            coo <- sp$coo_matrix(mat)
            dgT <- new("dgTMatrix",
                       i = c(0L, 0L, 1L, 2L, 2L, 2L),
                       j = c(0L, 2L, 2L, 0L, 1L, 2L),
                       x = c(1, 4, 5, 2, 3, 6),
                       Dim = c(3L, 3L))
            checkEquals(dgT, RcppArmadillo:::.SciPy2R(coo), msg="coo2dgt")
        }

        test.csr2dgr <- function() {
            csr <- sp$csr_matrix(mat)
            dgR <- methods::as(M, "dgRMatrix")
            checkEquals(dgR, RcppArmadillo:::.SciPy2R(csr), msg="csr2dgr")
        }

        test.other <- function() {
            bsr <- sp$bsr_matrix(list(3, 4))
            checkException(RcppArmadillo:::.SciPy2R(bsr))
        }
    }
}
