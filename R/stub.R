#' Stubbing
#' 
#' Replaces function calls in functions.
#' 
#' @param f
#'    a function whose introspection is watched.
#' @param replacement
#'    function (or value) list to replace.
#' 
#' @examples
#' f <- function(x, y) {
#'    a <- g(x)
#'    b <- h(x, y)
#'    a + b
#' }
#' sf <- stub(f, list(g=1, h=2))
#' sf()
#' 
#' @export
stub <- function(f, replacement) {
   if (missing(replacement)) {
      replacement <- list()
   }
   env <- parent.frame()
   replacement <- substitute(replacement)
   if (length(replacement) > 1L) {
      replacement <- as.list(replacement)[-1L]
   } else {
      replacement <- eval(replacement, envir=env)
   }
   
   traverse <- function(x) {
      switch(class(x),
         "(" = {
            x[[2L]] <- traverse(x[[2L]])
            x
         },
         "{" = {
            for (i in seq_along(x)[-1L]) {
               x[[i]] <- traverse(x[[i]])
            }
            x
         },
         "<-" = {
            x[[3L]] <- traverse(x[[3L]])
            x
         },
         "name" = {
            x
         },
         ## call is stubbed
         "call" = {
            name <- as.character(as.list(x)[[1L]])
            if (!is.null(replacement[[name]])) {
               r <- replacement[[name]]
               if (is.call(r)) {
                  value <- eval(r, envir=env)
                  if (is.function(value)) {
                     x[[1L]] <- value
                  } else {
                     x <- value
                  }
               } else if (is.name(r)) {
                  x[[1L]] <- get(as.character(r), envir=env)
               } else {
                  x <- r
               }
            } else {
               for (i in seq_along(x)[-1L]) {
                  x[[i]] <- traverse(x[[i]])
               }
            }
            x
         },
         "if" = {
            for (i in seq_along(x)[-1L]) {
               x[[i]] <- traverse(x[[i]])
            }
            x
         },
         "function" = {
            body(x) <- traverse(body(x))
            x
         },
         "NULL" = {
            x
         },
         {
            x
         }
      )
   }
   traverse(f)
}
