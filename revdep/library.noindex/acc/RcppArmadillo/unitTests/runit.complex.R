#!/usr/bin/r -t
#
# Copyright (C) 2013  Baptiste Auguie and Dirk Eddelbuettel
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

.setUp <- RcppArmadillo:::unit_test_setup("complex.cpp")

test.complex <- function() {

    set.seed(123)

    ## create variables

    A <- matrix(rnorm(9), 3)
    B <- matrix(rnorm(9), 3)
    C <- A + 1i * B

    V <- rnorm(3) + 1i * rnorm(3)
    S <- matrix(rnorm(5*3), nrow=3)

    ## Basic operations

    rl <- complexCppTests(A, B, V, S)   # returns results list from C++

    checkEquals(rl[["C"]],     C,          msg="complex matrix")
    checkEquals(rl[["Cst"]],   t(C),       msg="complex matrix transpose")
    checkEquals(rl[["Ct"]],    Conj(t(C)), msg="complex matrix transpose conjugated")
    checkEquals(rl[["conjC"]], Conj(C),    msg="complex matrix conjugated")
    checkEquals(rl[["absC"]],  Mod(C),     msg="complex matrix mod")
    checkEquals(rl[["CV"]],    C %*% V,    msg="complex matrix product")
    checkEquals(rl[["CS"]],    C %*% S,    msg="complex matrix times vector")
    checkEquals(rl[["CC"]],    C * C,      msg="complex matrix ops mult")
    checkEquals(rl[["CdC"]],   C / C,      msg="complex matrix ops div")
    checkEquals(rl[["CpC"]],   C + C,      msg="complex matrix ops plus")
    checkEquals(rl[["CmC"]],   C - C,      msg="complex matrix ops minus")
}
