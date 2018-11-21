## ---- echo = FALSE, message = FALSE---------------------------------------------------------------
library(markdown)
options(markdown.HTML.options = c(options('markdown.HTML.options')[[1]], "toc"))

library(knitr)
knitr::opts_chunk$set(
    error = FALSE,
    tidy  = FALSE,
    message = FALSE,
    fig.align = "center")
options(markdown.HTML.stylesheet = "custom.css")

options(width = 100)

## -------------------------------------------------------------------------------------------------
library(GlobalOptions)
opt = set_opt(
    a = 1,
    b = "text"
)

## -------------------------------------------------------------------------------------------------
opt()
opt("a")
opt$a
op = opt()
op
opt(a = 2, b = "new text")
opt()
opt$b = ""
opt()
opt(op)
opt()

## -------------------------------------------------------------------------------------------------
opt(a = 2, b = "new text")
opt(RESET = TRUE)
opt()

## -------------------------------------------------------------------------------------------------
opt

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = list(.value = 1,
             .length = c(1, 3),
             .class = "numeric")
)

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = list(.value = 1,
             .read.only = TRUE),
    b = 2
)
opt(READ.ONLY = TRUE)
opt(READ.ONLY = FALSE)
opt(READ.ONLY = NULL)  # default, to return both

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    verbose = 
        list(.value = TRUE,
             .filter = function(x) {
                 if(is.null(x)) {
                     return(FALSE)
                 } else if(is.na(x)) {
                     return(FALSE)
                 } else {
                     return(x)
                 }
              })
)
opt(verbose = FALSE); opt("verbose")
opt(verbose = NA); opt("verbose")
opt(verbose = NULL); opt("verbose")

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    margin = 
        list(.value = c(1, 1, 1, 1),
             .length = c(1, 2, 4),
             .filter = function(x) {
                if(length(x) == 1) {
                    return(rep(x, 4))
                } else if(length(x) == 2) {
                    return(rep(x, 2))
                } else {
                    return(x)
                }
            })
)
opt(margin = 2); opt("margin")
opt(margin = c(2, 4)); opt("margin")

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    prefix = ""
)
opt(prefix = function() paste("[", Sys.time(), "] ", sep = " "))
opt("prefix")  # or opt$prefix
Sys.sleep(2)
opt("prefix")

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    test_fun = list(.value = function(x1, x2) t.test(x1, x2)$p.value,
                    .class = "function")
)
opt(test_fun = function(x1, x2) cor.test(x1, x2)$p.value)
opt("test_fun")  # or opt$test_fun

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = 1,
    b = function() 2 * .v$a
)
opt("b")  # or opt$b
opt(a = 2)
opt("b")

## -------------------------------------------------------------------------------------------------
opt(a = 2, b = 3) # b was overwriiten and will not be 2*a
opt()

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = 1
)

opt(LOCAL = TRUE)
opt(a = 2)
opt$a
opt(LOCAL = FALSE)
opt$a

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = 1
)

f1 = function() {
    opt(LOCAL = TRUE)
    opt(a = 2)
    return(opt$a)
}
f1()
opt$a

f2 = function() {
    opt(LOCAL = TRUE)
    opt(a = 4)
    return(opt$a)
}
f2()
opt$a

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = 1
)

f1 = function() {
    opt(LOCAL = TRUE)
    opt(a = 2)
    return(f2())
}

f2 = function() {
    opt$a
}

f1()
opt$a

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    old = 1,
    new = list(.value = 1,
               .synonymous = "old")
)
opt()
opt$old = 2
opt()
opt$new = 3
opt()

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = 1,
    b = "b",
    c = list(.value = letters[1:4],
             .class = "character",
             .description = "26 letters"),
    d = list(.value = c(0, 0),
             .class = "numeric",
             .validate = function(x) x[1]^2 + x[2]^2 <= 1,
             .failed_msg = "The point should be in the unit circle",
             .description = "start points in the unit circle"),
    e = list(.value = rnorm,
             .class = "function",
             .description = "distribution to generate random numbers")
)
opt

## -------------------------------------------------------------------------------------------------
dump_opt(opt, "a")
dump_opt(opt, "d")

## -------------------------------------------------------------------------------------------------
opt = set_opt(a = 1)
opt(b = 2, ADD = TRUE)
opt

## -------------------------------------------------------------------------------------------------
opt = set_opt(
	a = list(.value = 1,
	         .visible = FALSE),
	b = 2
)
opt()
opt$a
opt$a = 2
opt$a
opt()

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = list(.value = 1,
             .private = TRUE)
)
require(stats)
ns = getNamespace("stats")
environment(opt)$options$a$`__generated_namespace__` = ns

## -------------------------------------------------------------------------------------------------
opt$a

## -------------------------------------------------------------------------------------------------
args(opt)

## -------------------------------------------------------------------------------------------------
opt1 = set_opt(
    a = list(.value = 1)
)
opt2 = set_opt(
    a = list(.value = 1)
)
opt1$a = 2
opt1$a
opt2$a

## -------------------------------------------------------------------------------------------------
opt = set_opt(
  a = list(.value = 1,
           class = "numeric")  # <- here it should be .class
)
opt$a

## -------------------------------------------------------------------------------------------------
opt = set_opt(
    a = list(.value = -1,
             .validate = function(x) x > 0)
)
opt$a

## -------------------------------------------------------------------------------------------------
sessionInfo()

