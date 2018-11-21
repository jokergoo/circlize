#!/usr/bin/r -t
#
# Copyright (C) 2014  Dirk Eddelbuettel
#
# This file is part of RcppArmadillo.
#
# RcppArmadillo is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# RcppArmadillo is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

.runThisTest <- suppressMessages(require(Matrix))

if (.runThisTest) {

    .setUp <- RcppArmadillo:::unit_test_setup("sparse.cpp") 

    ## setting up an example matrix -- using the fact that the as<sp_mat>
    ## converter prefers sparse matrix objects create by the Matrix package
    suppressMessages(require(Matrix))
    ## cf http://people.sc.fsu.edu/~jburkardt/data/st/st.html
    mtxt <- c("11   0   0  14   0  16",
              " 0  22   0   0  25  26",
              " 0   0  33  34   0  36",
              "41   0  43  44   0  46")
    M <- as.matrix(read.table(text=mtxt))
    dimnames(M) <- NULL
    SM <- Matrix(M, sparse=TRUE)

    test.as.sparse <- function() {
        checkEquals(SM, asSpMat(SM), msg="as<sp_mat>")
    }

    test.sparse.addition <- function() {
        checkEquals(SM + SM, sparseAddition(SM), msg="addSparse")
    }

    test.sparse.multiplication <- function() {
        k <- 3
        checkEquals(k*SM, sparseMultiplication(SM, k), msg="multSparse")
    }

    test.sparse.fromTriplet <- function() {
        mtxt <- c("0 0 1",
                  "0 2 0",
                  "3 0 0")
        M <- as.matrix(read.table(text=mtxt))
        dimnames(M) <- NULL
        SM <- Matrix(M, sparse=TRUE)

        spM <- fromTriplet(0:2,         # rows
                           2:0,         # cols
                           1:3)         # values
        checkEquals(SM, spM, msg="fromTriplet")
    }

    test.sparse.transpose <- function() {
        checkEquals(t(SM), sparseTranspose(SM), msg="transposeSparse")
    }

    test.sparse.sqrt <- function() {
        checkEquals(sqrt(SM), sparseSqrt(SM), msg="sqrtSparse")
    }
    
    test.sparse.square <- function() {
        checkEquals(SM^2, sparseSquare(SM), msg="squareSparse")
    }
    
    test.sparse.iterators <- function() {
        SM <- matrix(0, 5, 5)
        diag(SM) <- 1:5
        SM <- methods::as(SM, "dgCMatrix")
        spM <- sparseIterators(SM, -1.5)
        diag(SM) <- diag(SM) - 1.5
        checkEquals(SM, spM, msg="sparseIterators")
    }
    
    test.sparse.list <- function() {
        SM <- matrix(0, 5, 5)
        diag(SM) <- 1:5
        SM <- methods::as(SM, "dgCMatrix")
        l  <- list(SM, SM)
        checkEquals(l, sparseList(l), msg="sparseList")
    }
    
    test.speye <- function() {
      SM <- speye(4, 4)
      SM2 <- sparseMatrix(i = c(1:4), j = c(1:4), x = 1)
      checkEquals(SM, SM2, msg="speye")
      SM <- speye(3, 5)
      SM2 <- sparseMatrix(i = c(1:3), j = c(1:3), x = 1, dims = c(3, 5))
      checkEquals(SM, SM2, msg="speye")
      SM <- speye(5, 3)
      SM2 <- sparseMatrix(i = c(1:3), j = c(1:3), x = 1, dims = c(5, 3))
      checkEquals(SM, SM2, msg="speye")
    }
}
