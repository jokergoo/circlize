#!/usr/bin/r -t
#
# Copyright (C) 2014  Christian Gunning and Dirk Eddelbuettel
# Copyright (C) 2013  Romain Francois
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

.runThisTest <- isTRUE(capabilities("long.double"))

if (.runThisTest) {

    .setUp <- RcppArmadillo:::unit_test_setup("rmultinom.cpp") 

    test.rmultinom <- function() {
        ## Seed needs to be reset to compare R to C++
        .seed=39
        ## test cases 
        ## should be indentical between R and C++
        tests <- list(
            vanilla=list( n=5, size=100, prob=rep(1/10,10)),
            big=list( n=5, size=1e6, prob=rep(1/1e3,1e3)),
            fixup.prob=list( n=10, size=1e5, prob=1:10),
            n0=list( n=0, size=5, prob=1:10),
            size0=list( n=10, size=0, prob=1:10)
        )
        fail.tests <- list(
            na.prob=list( n=7, size=100, prob=c(1:10,NA)),
            prob0=list( n=10, size=100, prob=0)
        )
        ## these give errors
        lapply(names(fail.tests), function(.name) {
            with(fail.tests[[.name]], { 
                checkException(msg=sprintf("rmultinom.R.error.%s",.name),  rmultinom(n, size, prob))
            })
            with(fail.tests[[.name]], {
                checkException(msg=sprintf("rmultinom.cpp.error.%s",.name), rmultinomC(n, size, prob))
            })
        })
        ## for each test, check that results match
        lapply(names(tests), function(.name) {
            with(tests[[.name]], {
                set.seed(.seed)
                r.multinom <- rmultinom(n, size, prob)
                set.seed(.seed)
                c.multinom <- rmultinomC(n, size, prob)
                checkEquals(r.multinom, c.multinom, msg=sprintf("rmultinom.%s",.name))
            })
        })
    }
}
