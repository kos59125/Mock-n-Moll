#' Mocking
#' 
#' Watches variables and function calls to check they are used correctly.
#' 
#' @param f
#'    a function whose introspection is watched.
#' @param verify
#'    constraint that each variable or function call to be satisfied given as a list
#' @param violate
#'    action when the verification is not passed.
#' 
#' @examples
#' f <- function(x, y) x + y
#' mf <- guard(f, list(x = quote(x > 2)))
#' mf(3, 1)
#' \dontrun{
#' mf(1, 2)
#' }
#' 
#' g <- function(z) f(z + 1, z - 1)
#' mg <- guard(g, list(f = list(x = quote(x > 1))))
#' mg(1)
#' \dontrun{
#' mg(0)
#' }
#' 
#' @export
guard <- function(f, verify, violate=stopifnot) {
   if (missing(verify)) {
      verify <- list()
   }
   env <- parent.frame()
   violate <- substitute(violate)

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
         ## invariant condition
         "name" = {
            name <- as.character(x)
            if (!is.null(verify[[name]])) {
               arg <- if (name == "...") { quote(list(...)) } else { x }
               substitute(
                  {
                     violate(verify)
                     x
                  },
                  list(violate=violate, verify=verify[[name]], x=arg)
               )
            } else {
               x
            }
         },
         ## call verification
         "call" = {
            name <- as.character(as.list(x)[[1L]])
            if (!is.null(verify[[name]])) {
               f <- get(name, envir=env)
               call <- match.call(f, x, TRUE)
               dotIndex <- 1L
               for (index in seq_along(call)[-1L]) {
                  param <- names(call)[index]
                  if (param == "") {
                     param <- sprintf("..%d", dotIndex)
                     dotIndex <- dotIndex + 1L
                  }
                  if (param %in% names(verify[[name]])) {
                     table <- list()
                     table[[param]] <- call[[index]]
                     v <- substitute.variable(verify[[name]][[param]], table)
                     call[[index]] <- substitute(
                        {
                           violate(verify)
                           x
                        },
                        list(violate=violate, verify=v, x=call[[index]])
                     )
                  }
               }
               substitute(eval(call), list(call=call))
            } else {
               for (i in seq_along(x)[-1L]) {
                  x[[i]] <- traverse(x[[i]])
               }
               x
            }
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
