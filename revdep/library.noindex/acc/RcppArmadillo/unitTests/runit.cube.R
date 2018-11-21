#!/usr/bin/r -t
#
# Copyright (C) 2015 Dirk Eddelbuettel and Nathan Russell
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

# 30 November 2015

.setUp <- RcppArmadillo:::unit_test_setup("cube.cpp")

test.cube <- function() {
  
  ## test arrays
  dbl_cube <- array(1.5:27.5, rep(3, 3))
  int_cube <- array(1L:27L, rep(3, 3))
  cplx_cube <- array(1.5:27.5 + 2i, rep(3, 3))
  
  ## check cube (Cube<double>) and fcube (Cube<float>)
  checkEquals(cube_test(dbl_cube), (dbl_cube ** 2), "cube_test")
  checkEquals(fcube_test(dbl_cube), (dbl_cube ** 2), "fcube_test")
  
  ## check icube (Cube<sword>) and ucube (Cube<uword>)
  checkEquals(icube_test(int_cube), (int_cube ** 2), "icube_test")
  checkEquals(ucube_test(int_cube), (int_cube ** 2), "ucube_test")
  
  ## check cx_cube (Cube<cx_double>) and cx_fcube (Cube<cx_float>)
  checkEquals(cx_cube_test(cplx_cube), (cplx_cube ** 2), "cx_cube_test")
  checkEquals(cx_fcube_test(cplx_cube), (cplx_cube ** 2), "cx_fcube_test",
              tolerance = 1.5e-7)
  
  
  ## test that exception is thrown with dims(x) != 3
  dbl_cube <- array(1.5:16.5, rep(2, 4))
  int_cube <- array(1L:16L, rep(2, 4))
  cplx_cube <- array(1.5:16.5 + 2i, rep(2, 4))
  
  ## cube_test and fcube_test should throw here
  checkTrue(
    inherits(try(cube_test(dbl_cube), silent = TRUE), "try-error"),
    "cube_test bad dimensions")
  checkTrue(
    inherits(try(fcube_test(dbl_cube), silent = TRUE), "try-error"),
    "fcube_test bad dimensions")
  
  ## icube_test and ucube_test should throw here
  checkTrue(
    inherits(try(icube_test(int_cube), silent = TRUE), "try-error"),
    "icube_test bad dimensions")
  checkTrue(
    inherits(try(ucube_test(int_cube), silent = TRUE), "try-error"),
    "ucube_test bad dimensions")
  
  ## cx_cube_test and cx_fcube_test should throw here
  checkTrue(
    inherits(try(cx_cube_test(cplx_cube), silent = TRUE), "try-error"),
    "cx_cube_test bad dimensions")
  checkTrue(
    inherits(try(cx_fcube_test(cplx_cube), silent = TRUE), "try-error"),
    "cx_fcube_test bad dimensions")
  
  
  ## sanity check for explicit calls to Rcpp::as< arma::Cube<T> > 
  dbl_cube <- array(1.5:27.5, rep(3, 3))
  int_cube <- array(1L:27L, rep(3, 3))
  cplx_cube <- array(1.5:27.5 + 2i, rep(3, 3))
  
  ## check cube (Cube<double>) and fcube (Cube<float>)
  checkEquals(as_cube(dbl_cube), (dbl_cube ** 2), "as_cube")
  checkEquals(as_fcube(dbl_cube), (dbl_cube ** 2), "as_fcube")
  
  ## check icube (Cube<sword>) and ucube (Cube<uword>)
  checkEquals(as_icube(int_cube), (int_cube ** 2), "as_icube")
  checkEquals(as_ucube(int_cube), (int_cube ** 2), "as_ucube")
  
  ## check cx_cube (Cube<cx_double>) and cx_fcube (Cube<cx_float>)
  checkEquals(as_cx_cube(cplx_cube), (cplx_cube ** 2), "as_cx_cube")
  checkEquals(as_cx_fcube(cplx_cube), (cplx_cube ** 2), "as_cx_fcube",
              tolerance = 1.5e-7)
}
