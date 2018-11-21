#!/usr/bin/r -t
#
# Copyright (C) 2010 - 2017  Dirk Eddelbuettel, Romain Francois and Douglas Bates
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

.setUp <- RcppArmadillo:::unit_test_setup( packages = c("RcppArmadillo", "datasets" ) )

test.fastLm <- function() {
    data(trees, package="datasets")
    flm <- fastLmPure(cbind(1, log(trees$Girth)),
                      log(trees$Volume))
    fit <- lm(log(Volume) ~ log(Girth), data=trees)

    checkEquals(as.numeric(flm$coefficients), as.numeric(coef(fit)),
                msg="fastLm.coef")
    checkEquals(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]),
                msg="fastLm.stderr")
    checkEquals(as.numeric(flm$df.residual), as.numeric(fit$df.residual),
                msg="fastLm.df.residual")
}

test.fastLm.default <- function() {
    data(trees, package="datasets")
    flm <- RcppArmadillo:::fastLm.default(cbind(1, log(trees$Girth)), log(trees$Volume))
    fit <- lm(log(Volume) ~ log(Girth), data=trees)

    checkEquals(as.numeric(flm$coefficients), as.numeric(coef(fit)),
                msg="fastLm.default.coef")
    checkEquals(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]),
                msg="fastLm.default.stderr")
    checkEquals(as.numeric(flm$df.residual), as.numeric(fit$df.residual),
                msg="fastLm.default.df.residual")
    checkEquals(as.numeric(flm$residuals), as.numeric(fit$residuals),
                msg="fastLm.default.residuals")
    checkEquals(as.numeric(flm$fitted.values), as.numeric(fit$fitted.values),
                msg="fastLm.default.fitted.values")
}

test.summary.fastLm <- function() {
    data(trees, package="datasets")
    sflm <- summary(fastLm(log(Volume) ~ log(Girth), data=trees))
    sfit <- summary(lm(log(Volume) ~ log(Girth), data=trees))

    checkEquals(as.numeric(coef(sflm)), as.numeric(coef(sfit)),
                msg="summary.fastLm.coef")
    checkEquals(sflm$r.squared, sfit$r.squared,
                msg="summary.fastLm.r.squared")
    checkEquals(sflm$adj.r.squared, sfit$adj.r.squared,
                msg="summary.fastLm.r.squared")
    checkEquals(sflm$sigma, sfit$sigma,
                msg="summary.fastLm.sigma")

    ## no intercept case
    sflm <- summary(fastLm(log(Volume) ~ log(Girth) - 1, data=trees))
    sfit <- summary(lm(log(Volume) ~ log(Girth) - 1, data=trees))
    checkEquals(as.numeric(coef(sflm)), as.numeric(coef(sfit)),
                msg="summary.fastLm.coef.noint")
    checkEquals(sflm$r.squared, sfit$r.squared,
                msg="summary.fastLm.r.squared.noint")
    checkEquals(sflm$adj.r.squared, sfit$adj.r.squared,
                msg="summary.fastLm.r.squared.noint")
    checkEquals(sflm$sigma, sfit$sigma,
                msg="summary.fastLm.sigma.noint")

    ## non-formula use
    sflm <- summary(fastLm(log(trees$Girth), log(trees$Volume)))
    sfit <- summary(lm(log(Volume) ~ log(Girth) - 1, data=trees))
    checkEquals(as.numeric(coef(sflm)), as.numeric(coef(sfit)),
                msg="summary.fastLm.coef.nonform")
    checkEquals(sflm$r.squared, sfit$r.squared,
                msg="summary.fastLm.r.squared.nonform")
    checkEquals(sflm$adj.r.squared, sfit$adj.r.squared,
                msg="summary.fastLm.r.squared.nonform")
    checkEquals(sflm$sigma, sfit$sigma,
                msg="summary.fastLm.sigma.nonform")
}

test.fastLm.formula <- function() {
    data(trees, package="datasets")
    flm <- fastLm(log(Volume) ~ log(Girth), data=trees)
    fit <- lm(log(Volume) ~ log(Girth), data=trees)

    checkEquals(flm$coefficients, coef(fit), msg="fastLm.formula.coef")
    checkEquals(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]),
                msg="fastLm.formula.stderr")
    checkEquals(as.numeric(flm$df.residual), as.numeric(fit$df.residual),
                msg="fastLm.formula.df.residual")
    checkEquals(as.numeric(flm$residuals), as.numeric(fit$residuals),
                msg="fastLm.formula.residuals")
    checkEquals(as.numeric(flm$fitted.values), as.numeric(fit$fitted.values),
                msg="fastLm.formula.fitted.values")
}

