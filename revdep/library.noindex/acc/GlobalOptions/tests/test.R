context("Test `GlobalOptions`")


foo.options = setGlobalOptions(
	a = 1,
	b = "text"
)

test_that("get option values", {
	expect_that(foo.options(), is_identical_to(list(a = 1, b = "text")))
	expect_that(foo.options("a"), is_identical_to(1))
	expect_that(foo.options$a, is_identical_to(1))
	expect_that(foo.options("b"), is_identical_to("text"))
	expect_that(foo.options("c"), throws_error("No such option"))
	expect_that(foo.options(c("a", "b")), is_identical_to(list(a = 1, b = "text")))
	expect_that(foo.options("a", "b"), is_identical_to(list(a = 1, b = "text")))
	expect_that(foo.options(c("a", "b", "c")), throws_error("No such option"))
	expect_that(foo.options("a", "b", "c"), throws_error("No such option"))
})

test_that("set option values", {
	foo.options("a" = 2)
	expect_that(foo.options("a"), is_identical_to(2))

	foo.options$a = 4
	expect_that(foo.options$a, is_identical_to(4))
	
	foo.options(RESET = TRUE)
	expect_that(foo.options("a"), is_identical_to(1))
	
	foo.options("a" = 2, "b" = "str")
	expect_that(foo.options("a"), is_identical_to(2))
	expect_that(foo.options("b"), is_identical_to("str"))
	
	foo.options(RESET = TRUE)
	op = foo.options()
	foo.options("a" = 2, "b" = "str")
	foo.options(op)
	expect_that(foo.options("a"), is_identical_to(1))
	expect_that(foo.options("b"), is_identical_to("text"))
	
	expect_that(foo.options("c" = 1), throws_error("No such option"))
	expect_that(foo.options(1, "b" = "a"), throws_error("When setting options, all arguments should be named"))
	expect_that(foo.options(list(1, "b" = "a")), throws_error("When setting options, all arguments should be named"))
	expect_that(foo.options("a" = 1, "c" = 1), throws_error("No such option"))
})

test_that("testing valus are also list", {
	foo.options("a" = list(a = 1, b = 2))
	expect_that(foo.options("a"), is_identical_to(list(a = 1, b = 2)))

})

# testing if advanced setting is not mixed
test_that("testing on mixed setting", {
	expect_that(foo.options <- setGlobalOptions(
	a = list(.value = 1,
	         length = 1,
	         class = "numeric")
	), gives_warning("mixed"))
	expect_that(foo.options("a"), is_identical_to(
		list(.value = 1,
	         length = 1,
	         class = "numeric")
	))
})


# testing .length and .class
foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .length = 1,
	         .class = "numeric")
)

test_that("tesing on .length and .class ", {
	expect_that(foo.options(), is_identical_to(list(a = 1)))
	expect_that(foo.options(a = 1:3), throws_error("Length of .* should be"))
	expect_that(foo.options(a = "text"), throws_error("Class of .* should be"))		
})

# testing read.only
foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .read.only = TRUE),
	b = 2
)

test_that("tesing on .read.only ", {
	expect_that(foo.options(), is_identical_to(list(a = 1, b = 2)))
	expect_that(foo.options(a = 2), throws_error("is a read-only option"))
	expect_that(foo.options(READ.ONLY = TRUE), is_identical_to(list(a = 1)))
	expect_that(foo.options(READ.ONLY = FALSE), is_identical_to(list(b = 2)))
})

foo.options = setGlobalOptions(
	a = list(.value = 1,
		     .validate = function(x) x > 0,
		     .failed_msg = "'a' should be a positive number.")
)

test_that("testing on .failed_msg", {
	expect_that(foo.options(a = -1), throws_error("positive"))
})

# testing .validate and .filter
foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .validate = function(x) x > 0 && x < 10,
	         .filter = function(x) c(x, x))
)

test_that("tesing on .validate and .filter ", {
	expect_that(foo.options(), is_identical_to(list(a = c(1))))
	foo.options(a = 2)
	expect_that(foo.options(), is_identical_to(list(a = c(2, 2))))
	expect_that(foo.options(a = 20), throws_error("Your option is invalid"))
})

# test value after filter
foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .length = 1,
	         .filter = function(x) c(x, x))
)
test_that("testing on validation of filtered value", {
	expect_that(foo.options(a = 2), throws_error("Length of filtered"))
})

# testing if .value is a function
foo.options = setGlobalOptions(
	a = list(.value = 1),
	b = list(.value = 2,
		     .class = "function"),
	c = list(.value = function(x) 3,
		     .class = "numeric")
)

test_that("testing if '.value' is set as a function", {
	#expect_that(foo.options(), is_identical_to(list(a = 1, b = 2, c = 3)))
	foo.options(a = function(x) 1)
	expect_that(foo.options("a"), is_identical_to(1))
	foo.options(b = function(x) 2)
	expect_that(body(foo.options("b")), is_identical_to(2))
	expect_that(foo.options(c = function(x) "text"), throws_error("Class of .* should be"))

})

# testing if.value is a function and uses OPT
lt = setGlobalOptions(
	a = list(.value = 1),
	b = list(.value = function() 2 * get_opt_value('a'))
, get_opt_value_fun = TRUE)
foo.options = lt$opt_fun
get_opt_value = lt$get_opt_value


test_that("tesing if '.value' is a function and using other option values", {
	expect_that(foo.options("b"), is_identical_to(2))
	foo.options(a = 2)
	expect_that(foo.options("b"), is_identical_to(4))
	foo.options(RESET = TRUE)
	expect_that(foo.options("b"), is_identical_to(2))
})

# testing if.validate and .filter use OPT
lt = setGlobalOptions(
	a = list(.value = 1),
	b = list(.value = 2,
		     .validate = function(x) {
		     	if(get_opt_value('a') > 0) x > 0
		     	else x < 0
		     },
		     .filter = function(x) {
		     	x + get_opt_value('a')
		     })
, get_opt_value_fun = TRUE)
foo.options = lt$opt_fun
get_opt_value = lt$get_opt_value


test_that("tesing '.validate' and '.filter' using other option values", {
	foo.options(a = 1, b = 2)
	expect_that(foo.options("b"), is_identical_to(3))
	expect_that(foo.options(a = 1, b = -1), throws_error("Your option is invalid"))
	expect_that(foo.options(a = -1, b = 1), throws_error("Your option is invalid"))
})

# test in input value is NULL
foo.options = setGlobalOptions(
	a = 1
)

test_that("tesing if input value is NULL", {
	expect_that(foo.options(NULL), is_identical_to(NULL))
	foo.options(a = NULL)
	expect_that(foo.options("a"), is_identical_to(NULL))
})

## test if .value is invisible
foo.options = setGlobalOptions(
	a = list(.value = 1,
	         .visible = FALSE),
	b = 1
)

test_that("testing if '.value' is visible", {
	expect_that(foo.options(), is_identical_to(list(b = 1)))
	expect_that(foo.options("a"), is_identical_to(1))
	foo.options(a = 2)
	expect_that(foo.options("a"), is_identical_to(2))
})



opt = setGlobalOptions(
	a = 1
)

f1 = function() {
	opt(LOCAL = TRUE)
	opt(a = 2)
	return(opt("a"))
}

f1() # 2


f2 = function() {
	opt(LOCAL = TRUE)
	opt(a = 4)
	return(opt("a"))
}

f2() # 4

test_that("testing local mode", {
	expect_that(f1(), is_identical_to(2))
	expect_that(f2(), is_identical_to(4))
	expect_that(opt$a, is_identical_to(1))

	opt(LOCAL = TRUE)
	opt(a = 4)
	expect_that(opt("a"), is_identical_to(4))
	opt(LOCAL = FALSE)
	expect_that(opt("a"), is_identical_to(1))
})


opt = setGlobalOptions(
	a = 1
)

f1 = function() {
	opt(LOCAL = TRUE)
	opt(a = 2)
	return(f2())
}

f2 = function() {
	opt("a")
}

f1()  # 2

test_that("testing local mode 2", {
	expect_that(f1(), is_identical_to(2))
	expect_that(opt("a"), is_identical_to(1))
})

opt = setGlobalOptions(
	a = 1
)

opt(LOCAL = TRUE)
opt(a = 2)

f1 = function() {
	return(opt("a"))
}

f1()

test_that("testing local mode 3", {
	expect_that(f1(), is_identical_to(2))
})





