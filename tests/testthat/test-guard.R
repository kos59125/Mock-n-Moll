context("guard")

test_that("verify function parameter", {
   f <- function(x) x
   m <- guard(f, list(x = quote(x > 0)), expect_true)
   expect_equal(m(1), f(1))
})

test_that("violated when function verification is not passed", {
   f <- function(x) x
   m <- guard(f, list(x = quote(x > 0)))
   expect_error(m(0))
})

test_that("verify function parameter in function", {
   f <- function(x) x
   g <- function(y) f(y + 2)
   m <- guard(g, list(f = list(x = quote(x > 0))), expect_true)
   expect_equal(m(-1), g(-1))
})

test_that("violated when function in function verification is not passed", {
   f <- function(x) x
   g <- function(y) f(y - 1)
   m <- guard(g, list(f = list(x = quote(x > 0))))
   expect_error(m(1))
})
