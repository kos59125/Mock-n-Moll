Mock 'n' Moll
=============

Watches variables and function calls to check they are used correctly.


Installation
------------
   
```r
#install.packages("devtools")

devtools::install_github("kos59125/Mock-n-Moll")
```

Examples
--------

### mock
   
```r
library(MockNMoll)

f <- function(x, y) x + y
mf <- mock(f, list(x = quote(x > 2)))
mf(3, 1)
## mf(1, 2)

g <- function(z) f(z + 1, z - 1)
mg <- mock(g, list(f = list(x = quote(x >= 1))))
mg(1)
## mg(0)
```

With testthat package:

```r
library(MockNMoll)
library(testthat)

f <- function(x) x
m <- mock(f, list(x = quote(x > 0)), expect_true)
m(1)
## m(0)
```

### stub

```r
library(MockNMoll)

f <- function(x, y) {
   a <- g(x)
   b <- h(x, y)
   a + b
}
s <- stub(f, list(g=1, h=function(x, y) x + y))
s(0, 0)
```

With testthat package:

```r
library(MockNMoll)
library(testthat)

f <- function() {
   today <- Sys.Date()
   if (format(today, "%m%d") == "0101") {
      "Happy New Year"
   }
}
newYearTest <- stub(f, list(Sys.Date=as.Date("2010-01-01")))
expect_equal(newYearTest(), "Happy New Year")
```
