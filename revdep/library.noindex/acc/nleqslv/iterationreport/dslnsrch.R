# Dennis & Schnabel,1996,"Numerical methods for unconstrained optimization and nonlinear equations", SIAM
# example 6.5.1 page 149

library(nleqslv)
packageVersion("nleqslv")
.libPaths()

dslnex <- function(x) {
    y <- numeric(2)
    y[1] <- x[1]^2 + x[2]^2 - 2
    y[2] <- exp(x[1]-1) + x[2]^3 - 2
    y
}

xstart <- c(2,0.5)

# \section{Report for linesearch methods}
cat("Linesearch qline\n----------------\n")
nleqslv(xstart,dslnex, global="qline", control=list(trace=1))
# These two not in iteration report doc
cat("\nLinesearch gline\n----------------\n")
nleqslv(xstart,dslnex, global="gline", control=list(trace=1))
cat("\nLinesearch cline\n----------------\n")
nleqslv(xstart,dslnex, global="cline", control=list(trace=1))
