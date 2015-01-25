#' \code{substitute} equivalent, but an expression parameter is passed as a variable
#' 
#' @param var
#'    an expression variable
#' @param env
#'    substitution rule
substitute.variable <- function(var, env) {
   eval(substitute(substitute(expr, env), list(expr=var)))
}
