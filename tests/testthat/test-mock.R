context("mock")

test_that("verify function is replaced to a function", {
   f <- function(x, y) x + y
   g <- function(z) f(z, 2)
   s <- mock(g, list(f = function(x, y) x * y))
   expect_equal(g(3), 3 + 2)
   expect_equal(s(3), 3 * 2)
})

test_that("verify function is replaced to a value by an expression", {
   f <- function(x, y) x + y
   g <- function(z) f(z, 2)
   s <- mock(g, list(f = 1 + 2))
   expect_equal(g(3), 3 + 2)
   expect_equal(s(3), 1 + 2)
})

test_that("verify function is replaced to a number", {
   f <- function(x, y) x + y
   g <- function(z) f(z, 2)
   s <- mock(g, list(f = 1))
   expect_equal(g(3), 3 + 2)
   expect_equal(s(3), 1)
})

test_that("verify function is replaced to a symbol", {
   f <- function(x, y) x + y
   g <- function(z) f(z, 2)
   h <- quote(function(x, y) x * y)
   s <- mock(g, list(f = h))
   expect_equal(g(3), 3 + 2)
   expect_equal(s(3), 3 * 2)
})
